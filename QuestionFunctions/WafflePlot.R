create_waffle_df <- function(prob, absoluteCount = 100,invertProp=FALSE) {

  if(invertProp){prob=1-prob}
  # Get the smallest square grid that can fit the absoluteCount
  S <- ceiling(sqrt(absoluteCount))
  
  # Create the base grid
  df <- expand.grid(y = 1:S, x = 1:S) %>%
    dplyr::arrange(desc(y), x) %>% # top-left to bottom-right order
    dplyr::mutate(n = 1:n(),
                  valid = n <= absoluteCount) %>%
    dplyr::group_by(valid) %>%
    dplyr::mutate(id = (1:n()) / n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(valid) %>%
    dplyr::mutate(below_cutoff = id <= prob)
  
  return(df)
}



WafflePlot_pars<-function(basedf,
                          input,
                          question_field,
                          altTitles,
                          absoluteCount,
                          propVizDivisor,
                          naturalMin,
                          naturalMax,
                          anchor,
                          invertProp){
  
  # Filter for only the answers for the relevant question
  datLoop<-basedf%>%
    dplyr::filter(question==question_field,
                  alternative%in%altTitles)
  
  out_naturalMin<-naturalMin
  out_naturalMax<-naturalMax
  
  datLoop<-find_problems_pert(datLoop,
                              naturalMinEval=out_naturalMin,
                              naturalMaxEval=out_naturalMax)
  
  # cat("\n datLoop is \n")
  # cat(str(datLoop))
  # cat("\n")
  
  datLoop<-datLoop%>%
    mutate(oldMin=min,oldMax=max,min=trueMin,max=trueMax,dist="pert")%>%
    
    dplyr::select(min,mode,max,shape,dist,alternative,oldMin,oldMax,invalid,point)%>%
    
    dplyr::mutate(min=min*propVizDivisor,max=max*propVizDivisor,
                  mode=mode*propVizDivisor,
                  oldMin=oldMin*propVizDivisor,oldMax=oldMax*propVizDivisor)
  
  datLoop<-datLoop%>%
    dplyr::filter(!invalid)
  
  if(nrow(datLoop)==0){

    nullPlot<-TRUE
    propdf<-data.frame(x=0,y=0,id=0,image=0)[0,]
    
  }else{
    
    nullPlot<-FALSE
    S<-ceiling(sqrt(absoluteCount))
    
    options<-data.frame("opt"=c("min","mode","max","oldMin","oldMax"))
    
    alts<-data.frame(alternative=unique(datLoop$alternative))
    
    propdf<-
      expand.grid(y=1:S,x=1:S)%>% # Creating the 100 possible sillhouttes
      #arranging in a way where the first ones are top left, and last ones are bottom right
      dplyr::arrange(desc(y),x)%>%
      dplyr::mutate(n=1:n())%>%
      # define which tiles are valid (i.e. the ones that are below the absolute count)
      dplyr::mutate(valid=n<=absoluteCount)%>%
      dplyr::group_by(valid)%>%
      #define the ID for each silhoute based on the proportion it represents
      dplyr::mutate(id=(1:n())/n())%>%
    merge(options)%>%
    merge(alts)%>%
    dplyr::mutate(opt=factor(opt),alternative=factor(alternative))
    
    # cat("\n propdf is \n")
    # cat(str(propdf))
    # cat("\n")
    
    # if(is.null(anchor)){
      # Defining icons to be selected
      
      # cat("\ncreate cutoff df\n")
      cutoff_df<-datLoop%>%
        dplyr::select(min,mode,max,oldMin,oldMax,alternative)%>%
        tidyr::pivot_longer(cols = c("min","mode","max","oldMin","oldMax"))%>%
        dplyr::rename(opt=name,cutoff=value)
      
      # cat("\n invertProp \n")
      if(invertProp){cutoff_df$cutoff<-1-cutoff_df$cutoff}
      
      propdf<-merge(propdf,cutoff_df,by=c("alternative","opt"))
      
      # cat("\n propdf post cutoff is \n")
      # cat(str(propdf))
      # cat("\n")
      
      propdf<-propdf%>%
        dplyr::mutate(below_cutoff=id<=cutoff)
    
    # }else{}
    
  
  }
  
  # cat("\n Waffle Plot Df is \n")
  # cat(str(propdf))
  
  return(list(df=propdf))
}  

WafflePlot2<-function(waffle_pars,
                     input,
                     confAdj,
                     suffix,
                     altTitles,
                     absoluteCount,
                     propVizDivisor,
                     cex=1.5,
                     icon,anchor=NULL,
                     invertProp,
                     PaletteNames,
                     PaletteColors){
  
  require(stringr)
  
  if(!confAdj){
    inputPreffix<-"plotVizControl"
  }else{
    inputPreffix<-"plotVizControlAdj"
  }
  
  # cat("\n input is\n")
  # cat(str(input))
  
  selectedVar<-input[[paste(inputPreffix,suffix,sep="_")]]
  selectedVar<-tolower(gsub(x=selectedVar,"_.*",""))
  
  adjFromCheckbox<-input[[paste0("adjPlotCheckbox_",suffix,sep="")]]
  
  
  if(!adjFromCheckbox){
    selectedVar[selectedVar=="min"]<-"oldMin"
    selectedVar[selectedVar=="max"]<-"oldMax"  
  }
  
  # cat("\n selectedVar is\n")
  # cat(selectedVar)
  # cat("\n")
  
  current_alt<-altTitles[suffix]

  if(length(altTitles)>0){
    plotTitle<-current_alt
  }else{
    plotTitle<-""
  }
  
  
  currentdf<-waffle_pars$df%>%
    dplyr::filter(alternative==current_alt,opt==selectedVar)
  
  # cat("\n currentdf is\n")
  # cat(str(currentdf))
  
  if(nrow(currentdf)>0){
  # Draw empty images on the icons that should not be there
  
  currentdf$below_cutoff <- factor(currentdf$below_cutoff, levels = c(FALSE,TRUE))
  
  # Create a placeholder data frame to ensure both levels appear in the legend
  legend_df <- data.frame(
    x = NaN, y = NaN,
    below_cutoff = factor(c(FALSE, TRUE), levels = c(FALSE, TRUE))
  )
    
  # cex <- 1.5  # Define a multiplier for text size
  
  gg <- ggplot(data = currentdf, aes(x = x, y = y))
  
  gg <- gg +
    # Add tile layers
    geom_tile(aes(fill = below_cutoff),
              width = .75, height = .75,
              color = NA) +
    geom_tile(data = legend_df, aes(fill = below_cutoff), width = 0, height = 0) +
    # Fixed coordinate system
    coord_fixed() +
    # Manual fill scale
    scale_fill_manual(
      name = NULL,
      labels = PaletteNames,
      values = PaletteColors,
      drop = FALSE,
      guide = guide_legend(override.aes = list(alpha = 1))
    )
  
  gg <- gg +
    # Identity scale for size (if needed)
    scale_size_identity() +
    # Remove axis labels
    ylab("") +
    xlab("") +
    ggtitle(plotTitle) +
    # Use a black-and-white theme
    theme_bw() +
    # Customize text and aesthetic elements
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_text(size = 12 * cex),  # Scale axis titles
      legend.text = element_text(size = 10 * cex), # Scale legend text
      plot.title = element_text(size = 14 * cex, face = "bold"), # Scale plot title
      plot.subtitle = element_text(size = 12 * cex) # Scale subtitle, if any
    ) +
    coord_equal()
  
  }else{gg<-create_error_plot()}
  
  return(gg)
}