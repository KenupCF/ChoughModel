extract_legend_plot <- function(p) {

  tmp <- ggplotGrob(p)
  leg <- gtable_filter(tmp, "guide-box")
  grid.newpage()
  grid.draw(leg)  # Draw once (optional for debugging)
  
  # Turn into grob plot
  legend_plot <- cowplot::ggdraw() + cowplot::draw_grob(leg)
  return(legend_plot)
}

adjust_prob_by_odds_ratio <- function(X, R) {
  odds_X <- X / (1 - X)
  odds_Y <- R * odds_X
  Y <- odds_Y / (1 + odds_Y)
  return(Y)
}

# Calculate odds info for annotation
calc_odds_label <- function(prob, n = 100, succLabel = "Alive", failLabel = "Dead") {
  # succ <- round(prob * n)
  # fail <- n - succ
  succ <- round(prob*100,2)
  fail <- 100 - succ
  odds <- ifelse(fail == 0, NA, round(succ / fail, 2))
  label <- paste0(
    succLabel, ": ", succ, "% | ",
    failLabel, ": ", fail, "%\n",
    "(Odds: ", ifelse(is.na(odds), "∞", odds),")"
  )
  return(label)
}





WafflePlotOdds_pars<-function(basedf,
                          input,
                          question_field,
                          altTitles,
                          absoluteCount,
                          propVizDivisor,
                          naturalMin,
                          naturalMax,
                          succLabel,
                          failLabel,
                          categs=c("Resident","Translocated"),
                          anchor,
                          invertProp){
  
  options<-data.frame("opt"=c("min","mode","max","oldMin","oldMax"))
  
  
  baseWaffleDF<-plyr::rbind.fill(lapply(seq_along(altTitles),function(i){
    create_waffle_df(prob = input[[paste0("oddsBaseProb_",i)]]/100)%>%
      dplyr::mutate(alternative=altTitles[i],categ=categs[1])
  }))%>%
    merge(options)
    
  baseOdds<-plyr::rbind.fill(lapply(seq_along(altTitles),function(i){
    data.frame(baseProb = input[[paste0("oddsBaseProb_",i)]]/100)%>%
      dplyr::mutate(alternative=altTitles[i])
  }))
  
  # cat("\n baseOdds is \n")
  # cat(str(baseOdds))
  # cat("\n")
  
  # For base (reference)
  base_odds_labels <- baseOdds %>%
    dplyr::mutate(prob = baseProb,
                  label = calc_odds_label(prob, n = absoluteCount,succLabel=succLabel,failLabel=failLabel),
                  categ = categs[1])%>%
    merge(options)%>%
    dplyr::select(label,categ,alternative,opt)
  
  
  # cat("\n base_odds_labels is \n")
  # cat(str(base_odds_labels))
  # cat("\n")
  
  # cat("\n baseOdds is \n")
  # cat(str(baseOdds))
  # cat("\n")
  
  
  # Filter for only the answers for the relevant question
  datLoop<-basedf%>%
    dplyr::filter(question==question_field,
                  alternative%in%altTitles)
  
  alts<-data.frame(alternative=unique(datLoop$alternative))
  
  out_naturalMin<-naturalMin
  out_naturalMax<-naturalMax
  
  datLoop<-find_problems_pert(datLoop,
                              naturalMinEval=out_naturalMin,
                              naturalMaxEval=out_naturalMax)
  


  
    datLoop<-datLoop%>%
    mutate(oldMin=min,oldMax=max,min=trueMin,max=trueMax,dist="pert")%>%
    dplyr::select(min,mode,max,shape,dist,alternative,oldMin,oldMax,invalid,point)%>%
    dplyr::mutate(min=min*propVizDivisor,max=max*propVizDivisor,
                  mode=mode*propVizDivisor,
                  oldMin=oldMin*propVizDivisor,oldMax=oldMax*propVizDivisor)
    
    
  datLoop<-datLoop%>%
    dplyr::filter(!invalid)
  
  # cat("\n datLoop is \n")
  # cat(str(datLoop))
  # cat("\n")
  
  if(nrow(datLoop)==0){
     
    nullPlot<-TRUE
    propdf<-data.frame(x=0,y=0,id=0,image=0,categ="")[0,]
    target_labels<-data.frame()
  }else{
     
    nullPlot<-FALSE
    
    
    propdf<-create_waffle_df(prob=0,absoluteCount = absoluteCount)%>%
        dplyr::mutate(below_cutoff=NULL)%>%
      merge(options)%>%
      merge(alts)
  #   
    # cat("\n propdf is \n")
    # cat(str(propdf))
    # cat("\n")
  #   
    if(is.null(anchor)){
  #   # Defining icons to be selected
  #   
    # cat("\ncreate cutoff df\n")
    cutoff_df<-datLoop%>%
      dplyr::select(min,mode,max,oldMin,oldMax,alternative)%>%
      tidyr::pivot_longer(cols = c("min","mode","max","oldMin","oldMax"))%>%
      left_join(baseOdds)%>%
      dplyr::mutate(value2=adjust_prob_by_odds_ratio(R=value,X=baseProb))%>%
      dplyr::rename(opt=name,cutoff=value2)
    
    # cat("\n cutoff_df is \n")
    # cat(str(cutoff_df))
    # cat("\n")
    
    target_labels <- cutoff_df %>%
      dplyr::mutate(prob = if (invertProp) 1 - cutoff else cutoff,
                    label = calc_odds_label(prob, n = absoluteCount,succLabel=succLabel,failLabel=failLabel),
                    categ = categs[2])%>%
      dplyr::select(label,categ,alternative,opt)
    
    # cat("\n target_labels is \n")
    # cat(str(target_labels))
    # cat("\n")
    

    
      # cat("\n invertProp \n")
    if(invertProp){cutoff_df$cutoff<-1-cutoff_df$cutoff}
    
    propdf<-merge(propdf,cutoff_df,by=c("alternative","opt"))

    propdf<-propdf%>%
      dplyr::mutate(below_cutoff=id<=cutoff,categ=categs[2])

    # cat("\n propdf post cutoff is \n")
    # cat(str(propdf))
    # cat("\n")
    
    }else{}

  #   
  }
  
  labels<-plyr::rbind.fill(target_labels,base_odds_labels)
  resu<-plyr::rbind.fill(baseWaffleDF,propdf)

  return(list(df=resu,labels=labels))
}  

WafflePlotOdds <- function(waffle_pars,
                           input,
                           confAdj,
                           suffix,
                           altTitles,
                           absoluteCount,
                           propVizDivisor,
                           cex = 1.5,
                           icon, anchor = NULL,
                           invertProp,
                           PaletteNames,
                           PaletteColors) {
  
  require(stringr)
  
  if (!confAdj) {
    inputPreffix <- "plotVizControl"
  } else {
    inputPreffix <- "plotVizControlAdj"
  }
  
  selectedVar <- input[[paste(inputPreffix, suffix, sep = "_")]]
  selectedVar <- tolower(gsub(x = selectedVar, "_.*", ""))
  
  adjFromCheckbox <- input[[paste0("adjPlotCheckbox_", suffix, sep = "")]]
  
  if (!adjFromCheckbox) {
    selectedVar[selectedVar == "min"] <- "oldMin"
    selectedVar[selectedVar == "max"] <- "oldMax"
  }
  
  current_alt <- altTitles[suffix]
  
  if (length(altTitles) > 0) {
    plotTitle <- current_alt
  } else {
    plotTitle <- ""
  }
  
  
  currentdf <- waffle_pars$df %>%
    dplyr::filter(alternative == current_alt, opt == selectedVar)
  
  label_data <- waffle_pars$labels %>%
    dplyr::filter(alternative == current_alt,opt == selectedVar)
  
  # cat("\n label_data is\n")
  # cat(str(label_data))
  # cat("\n")

  if (nrow(currentdf) > 0 & length(unique(currentdf$categ))>1) {
    
    currentdf$below_cutoff <- factor(currentdf$below_cutoff, levels = c(FALSE, TRUE))
    
    legend_df <- data.frame(
      x = NaN, y = NaN,
      below_cutoff = factor(c(FALSE, TRUE), levels = c(FALSE, TRUE))
    )
    
    # Create label plot aligned with facets
    label_plot <- ggplot(label_data, aes(x = 1, y = 1, label = label)) +
      geom_text(size = 3 * cex, hjust = 0.5, vjust = 0.5, fontface = "italic") +
      facet_wrap(~categ, ncol = 2) +
      theme_void() +
      theme(
        strip.text = element_blank(),
        panel.spacing = unit(4, "pt"),  # more breathing room between facets
        plot.margin = margin(4, 8, 4, 8, unit = "pt")  # increase margins to avoid clipping
      )
    
    
    gg <- ggplot(data = currentdf, aes(x = x, y = y)) +
      geom_tile(aes(fill = below_cutoff), width = 0.75, height = 0.75, color = NA) +
      geom_tile(data = legend_df, aes(fill = below_cutoff), width = 0, height = 0) +
      facet_wrap(~categ, ncol = 2) +
      coord_fixed() +
      scale_fill_manual(
        name = NULL,
        labels = PaletteNames,
        values = PaletteColors,
        drop = FALSE,
        guide = guide_legend(override.aes = list(alpha = 1))
      ) +
      scale_size_identity() +
      ylab("") +
      xlab("") +
      ggtitle(plotTitle) +
      theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 12 * cex),
        legend.text = element_text(size = 10 * cex),
        legend.position = "top",                     # move legend to bottom
        legend.margin = margin(t = 2, unit = "pt"),     # reduce legend margin
        plot.title = element_text(size = 14 * cex, face = "bold"),
        plot.subtitle = element_text(size = 12 * cex),
        plot.margin = margin(2, 2, 2, 2, unit = "pt")  # << remove outside whitespace
      ) +
      coord_equal()
    
    gg <- gg + theme(
      plot.margin = margin(0, 0, 0, 0, unit = "pt")  # Remove margin around waffle
    )
    
    # legend_plot <- extract_legend_plot(gg)
    
    # gg<-gg+theme(legend.position="none")
    
    
    gg_combined <- gg / label_plot + plot_layout(heights = c(10, 3))
    
    # final_plot <- (gg / label_plot + plot_layout(heights = c(10, 1)))+
    #   plot_layout(widths = c(10, 1), guides = "collect") & 
    #   theme(plot.margin = margin(0, 0, 0, 0, unit = "pt"))
    
    
  } else {
    final_plot <- gg_combined<- gg<- create_error_plot()
  }
  
  return(gg_combined)
}
