generatePDFplotInfo<-function(basedf,input,suffix="",
                                     plotMin=NULL,
                                     plotMax=NULL,
                                     altTitles,
                                     question_field="",
                                     previewAnchor=FALSE,
                                     propVizDivisor=1,
                                     fullName="",
                                     envir,
                                     anchor0=NULL,
                                     anchor=NULL,
                                     AnchorLabel="Anchor",
                                     naturalMin=-Inf,
                                     naturalMax=Inf,
                                     trueMax=numeric(),
                                     trueMin=numeric(),
                                     fullRangePlot=TRUE){
  
  
  # cat(colnames(basedf))
  
  # Filter for only the answers for the relevant question
  datLoop<-basedf%>%
    dplyr::filter(question==question_field,
                  alternative%in%altTitles)
  
  out_naturalMin<-naturalMin
  out_naturalMax<-naturalMax
  
  datLoop<-find_problems_pert(datLoop,
                              naturalMinEval=out_naturalMin,
                              naturalMaxEval=out_naturalMax)
  datLoop<-datLoop%>%
    mutate(oldMin=min,oldMax=max,min=trueMin,max=trueMax,dist="pert")%>%
    dplyr::select(min,mode,max,shape,dist,alternative,
                  oldMin,oldMax,invalid,point)%>%
    dplyr::filter(!invalid)%>%
    dplyr::mutate(oldMin=oldMin*propVizDivisor,
                  oldMax=oldMax*propVizDivisor,
                  min=min*propVizDivisor,
                  mode=mode*propVizDivisor,
                  max=max*propVizDivisor)
  
  dat.dist<-datLoop%>%
    dplyr::filter(!invalid & !point)
  
  dat.point<-datLoop%>%
    dplyr::filter(!invalid & point)
  
  # Formatting on the format that generate PDF curves
  
  noAlt<-all(datLoop$alternative==0)
  
  # cat("Sorted types of answers")
  
  
  if(nrow(dat.dist)>0){
    
    # Split for each alternative
    dat.dist.l<-split(dat.dist,dat.dist$alternative)
    
    validPDF<-length(dat.dist.l)>0
    
  }else{validPDF<-FALSE}
  
  if(nrow(dat.point)>0){
    # point.df<-dat.point
    point.df<-plyr::rbind.fill(lapply(1:nrow(dat.point),function(r){
      data.frame(values=rep(dat.point$mode[r],1e3),
                 PDF=seq(from=0,to=1,length.out=1e3),
                 dist="",alternative=dat.point$alternative[r])
    }))
    
  }else{
    # point.df<-data.frame(alternative="",mode=0,width=0,current=FALSE)[0,]
    point.df<-data.frame(alternative="",PDF=0,values=0,dist="")[0,]
  }
  # Generating priors]
  
  if(validPDF){
    
    PDF.dat<-priorPDF(L = dat.dist.l)%>%
      mutate(alternative=par)%>%
      ungroup()%>%
      # Fixing PDF to be max 1
      mutate(PDF=PDF/max(PDF))%>%
      ungroup()
      
  }else{
    
    PDF.dat<-data.frame(PDF=0,values=0,
                        dist="",par="",pars="",
                        alternative="",width=0,current=FALSE)[0,]}
  
  
  # renderPlot({
  # 
  # plotAvailable<-validInput | previewAnchor
  #   
  if(!is.null(anchor)){
    anchorPDF<-anchor%>%
      mutate(alternative="Anchor")
    anch.lim<-range(anchorPDF$x)
  }else{
    anchorPDF<-data.frame(alternative="",x=0,y=0)[0,]
    anch.lim<-c(NA,NA)
  }
  # 
  # 
  # if(!plotAvailable){
  #   
  #     nullPlot<-TRUE
  #     DF=data.frame(x=0,y=0)[0,]
  #     markDF<-data.frame(x=0,y=0)[0,]
  #     plot.lims<-c(0,0)
  #     
  #   }else{
  # DF<-PDF.dat%>%
  DF<-plyr::rbind.fill(PDF.dat,point.df)%>%
    mutate(x=values,y=PDF,color=alternative)
  
  if(validPDF){
    
    lim<-c(min(PDF.dat$values),max(PDF.dat$values))
    
    
    markDF<-rbind.fill(
      dat.dist%>%
      dplyr::mutate(x=oldMin,alternative=alternative)%>%
      dplyr::select(x,alternative)%>%
      dplyr::mutate(class="range"),
      dat.dist%>%
        dplyr::mutate(x=oldMax,alternative=alternative)%>%
        dplyr::select(x,alternative)%>%
        dplyr::mutate(class="range"))
      
          
    plot.lims<-lim
    
  }else{
    plot.lims<-c(NA,NA)
    markDF<-data.frame(x=0,y=0,alternative="",class="")[0,]
  }
  
  plot.lims[1]<-min(anch.lim[1],plot.lims[1],na.rm=T)
  plot.lims[2]<-max(anch.lim[2],plot.lims[2],na.rm=T)
  
  return(list(DF=DF,
              pointDF=point.df,
              anchor=anchor,
              AnchorLabel=AnchorLabel,
              noAlt=noAlt,
              anchorPDF=anchorPDF%>%
                mutate(y=y/max(anchorPDF$y)),
              markDF=markDF,
              plot.lims=plot.lims))  
  
  
  # })
}


renderPDFplot<-function(pdfPars,
                        PDFpalette,
                        alt_index,
                        altTitles,
                        fullName,
                        odds_sec_axis=FALSE,
                        cex=1.5,
                        str_wrap_size=12,minPDF=-Inf,
                        auxPlot=NULL){
  
  # cat("\n render PDF initialized \n")
  
  tempDF<-pdfPars$DF
  
  tempDF <- tempDF %>% filter(y >= minPDF)
  
  pointDF<-pdfPars$pointDF
  
  
  
  # presentAlts<-unique(c(tempDF$alternative,pointDF$alternative))
  presentAlts<-unique(c(tempDF$alternative))
  
  presentAltTitle<-altTitles[altTitles%in%presentAlts]
  
  alt_index_2<-which(presentAltTitle==altTitles[alt_index])
  
  plotLevels<-c(presentAltTitle[-alt_index_2],
                presentAltTitle[alt_index_2])
  
  ## Refactor alternatives so that the first  one is the active alternative
  tempDF$alternative<-factor(tempDF$alternative,
                             levels=plotLevels)
  
  # if(nrow(pointDF)>0){
  # pointDF$alternative<-factor(pointDF$alternative,
                             # levels=plotLevels)
  # }
  # cat("\n levels of alternative are \n")
  # cat(levels(tempDF$alternative))
  
  PDFpalette<-PDFpalette[plotLevels]
  
  width.default<-1
  
  
  PDFpaletteLoop<-PDFpalette
  PDFpaletteLoop<-PDFpaletteLoop[plotLevels%in%presentAlts]
  
  # trying might need to remove
  names(PDFpaletteLoop) <- plotLevels
  
  label_index<-which(names(PDFpaletteLoop)==presentAltTitle[alt_index_2])
  
  # cat("\n calculated label_index is \n")
  # cat(label_index)
  if(length(label_index)>0){
  
  labels<-names(PDFpaletteLoop)
  
  
  
  # Wrap labels
  wrapped<-str_wrap(labels,width = str_wrap_size)
  # Remove hyphens at the end of lines
  # wrapped <- gsub("-\n", "\n", wrapped)
  
  # Remove hyphens at the beginning of lines
  # wrapped <- gsub("\n-", "\n", wrapped)
  
  # Convert line breaks to HTML
  labels <- gsub("\n", "<br>", wrapped)
  # labels <- gsub("<br> <br>", "<br><br>", wrapped)
  # labels <- gsub("<br><br>", "<br>", wrapped)
  
  ## Bold active alternative
  labels[label_index]<-paste0("**",labels[label_index],"**") 
  
  
  if(nrow(tempDF)>0){
  tempDF$width<-width.default
  tempDF$width[tempDF$alternative==altTitles[alt_index]]<-width.default*1.1
  tempDF$current<-ifelse(tempDF$alternative==altTitles[alt_index],"yes","no")
  }
  
  # if(nrow(pointDF)>0){
  #   pointDF$width<-width.default
  #   pointDF$width[pointDF$alternative==altTitles[alt_index]]<-width.default*1.1
  #   pointDF$current<-ifelse(pointDF$alternative==altTitles[alt_index],"yes","no")
  # }
 
  #
  # cat("\n factor of point plot \n")
  # cat(alt_index)
  # cat("\n is \n")
  # cat(unique(as.character(pointDF$alternative)))
  
  names(PDFpaletteLoop) <- plotLevels
  # Create ggplot
  # cex <- 1.5  # Define a multiplier for text size
  # Wrap labels for better readability
  # wrapped_labels <- stringr::str_wrap(labels, width = str_wrap_size)
  
  num_alternatives <- length(unique(plotLevels))
  
  # cat("\n anchorPDF is\n")
  # cat(str(pdfPars$anchorPDF))
  # cat("\n")

  # pdfPars$anchorPDF$origin <- factor(pdfPars$anchorPDF$origin)
  
  gg <- ggplot() +
    # Plot shaded area of presented anchor
    geom_ribbon(
      data = pdfPars$anchorPDF,
      ymin=0,
      aes(x = x, ymax = y, fill = origin,group = origin),
      color = NA, alpha = .75,lty="dotted"
    ) +
    # Plot PDF as lines - with a thicker width for current alternative
    geom_line(
      data = tempDF %>%
        mutate(width = as.numeric(width)),
      aes(x = x, y = y, color = alternative, size = current)
    ) +
    # Manually change size scale
    scale_size_manual(
      name = NULL,
      values = c("no" = width.default,
                 "yes" = width.default + 1),
      guide = "none"
    ) +
    # Axis Labels
    ylab("Probability Density Function") +
    xlab(str_wrap(fullName,35)) +
    # Add vertical lines to mark reported range
    geom_vline(
      data = pdfPars$markDF %>%
        mutate(width = width.default) %>%
        filter(alternative == altTitles[alt_index]),
      aes(xintercept = x, color = alternative),
      lty = "dashed", show.legend = FALSE
    ) +
    # Change Y scale to start at 0
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    # Aesthetic theme changes
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.title = element_text(size = 12 * cex),  # Scale axis titles
      axis.text = element_text(size = 10 * cex),   # Scale axis text
      legend.text = element_text(size = 10 * cex), # Scale legend text
      # legend.key.height = unit(0.75 * cex, "cm"),  # Increase height of legend keys
      legend.spacing.y = unit(1.5 , "lines"),    # Increase spacing between groups
      plot.title = element_text(size = 14 * cex, face = "bold"), # Scale plot title
      plot.subtitle = element_text(size = 12 * cex) # Scale subtitle, if any
    ) +
    
    scale_color_manual(
      name = NULL,
      values = PDFpaletteLoop,
      labels = labels)+ # Wrap legend labels
    # ) +
    # # Change color palette
    # scale_color_manual(
    #   name = NULL,
    #   values = PDFpaletteLoop[
    #     # order(plotLevels)
    #   ],
    #   labels = labels[
    #     # order(plotLevels)
    #   ]
    # ) +
    theme(legend.text = element_markdown()) +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, .05))
      # ,limits = pdfPars$plot.lims
    ) +

    guides(color = guide_legend(
      byrow=TRUE,
      keywidth = 0.2,
      keyheight = 0.3*cex,
      default.unit = "inch", override.aes = list(size = 3)
    ))
  
  if (num_alternatives == 1 & is.null(pdfPars$anchor)) {
    gg <- gg + theme(legend.position = "none")
  }
  
  }else{
    gg<-create_error_plot()
  }
  
  if(odds_sec_axis){
    
    # library(scales)
    # 
    # log_odds_trans <- trans_new(
    #   name = "log_odds",
    #   transform = function(x) log(x),
    #   inverse = function(x) exp(x),
    #   domain = c(1e-5, Inf)  # avoid 0 or negatives
    # )
    
    
    # gg<-gg+
      # scale_x_log10(
      #   breaks = c(0.25, 0.33, 0.5, 1, 2, 3, 4, 5, 10),
      #   labels = c(
      #     "¼ as likely", "⅓ as likely", "½ as likely", "same odds",
      #     "twice as likely", "three times as likely", "four times as likely",
      #     "five times as likely", "ten times as likely"
      #   )
      # )
      gg <- gg +
        
        # scale_x_continuous(
        #   trans = log_odds_trans,
        #   breaks = c(.01,0.1,0.2,0.25,0.33, 0.5, 1, 2, 3,4,5,10,100),
        #   labels = c(
        #     "a hundred times less likely",
        #     "ten times less likely",
        #     "five times less likely",
        #     "four times less likely",
        #     "three times less likely",
        #     "twice less likely",
        #     "same odds",
        #     "twice more likely",
        #     "three times more likely",
        #     "four times more likely",
        #     "five times more likely",
        #     "ten times more likely",
        #     "a hundred times more likely"
        #   ),
        #   expand = expansion(mult = c(0.05, 0.05))
        # ) +
        # theme(
        #   axis.text.x = element_text(angle = 45, hjust = 1)
        # )
      
      scale_x_log10(
        expand = expansion(mult = c(0.05, .05)),
        sec.axis = dup_axis(
          name = NULL,
          breaks = c(.01,0.1,0.2,0.25,0.33, 0.5, 1, 2, 3,4,5,10,100),
          labels = c(
            "a hundred times less likely",
            "ten times less likely",
            "five times less likely",
            "four times less likely",
            "three times less likely",
            "twice less likely",
            "same odds",
            "twice more likely",
            "three times more likely",
            "four times more likely",
            "five times more likely",
            "ten times more likely",
            "a hundred times more likely"
          )
        )
      )+
      theme(
        axis.text.x.top = element_text(angle = 45, hjust = 0)
      )

  }
  # If there is an anchor
  if(!is.null(pdfPars$anchorPDF)){
    
    # cat("\n Add anchor scale \n")
    # cat(str(pdfPars$anchorPDF))
    # cat('\n')
    
    origins <- unique(pdfPars$anchorPDF$origin)
    grey_shades <- gray.colors(length(origins), start = 0.3, end = 0.9)
    # grey_shades<-c("green","red")
    names(grey_shades) <- origins
    # 
    # # cat("\n grey_shades is\n")
    # cat(str(grey_shades))
    # cat("\n")
    # 

    # 
    # Add scale to represent it
    

    gg<-gg+   scale_fill_manual(
      values = grey_shades,
      breaks = waiver(),
      
      guide=guide_legend(override.aes = list(alpha=.75))
      
    )
    }
  # if there is no alternative, remove legend
if(pdfPars$noAlt){gg<-gg+theme(legend.position = "none")}

if(is.null(auxPlot)){
  resu<-gg
  
}else{
  
  allPlots<-list(gg)
  for(i in 1:length(auxPlot)){
    allPlots[[i+1]]<-auxPlot[[i]]
  }
  resu<-do.call(plot_grid,c(allPlots,list(ncol=2)))
  
}

return(resu)

}
