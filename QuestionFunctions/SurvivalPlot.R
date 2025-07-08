DecayPlotPars<-function(basedf,naturalMin=0,naturalMax=Inf,
                        x_axis_precision=1e4,type="time",
                        time_frame_days=NA,
                        altTitles,
                        question_field=""){
  
  # cat("Started generating surv df")
  
  replace_m_inf<-function(x,y=1){
    x[x==-Inf]<-y
    return(x)
  }
  
  dat<-basedf%>%
    dplyr::filter(question==question_field)%>%
    find_problems_pert(naturalMinEval=naturalMin,
                       naturalMaxEval=naturalMax)%>%
    dplyr::filter(!invalid)

  # cat("\nvalid answers is \n")
  # cat(str(dat))
  # cat("\n")
  if(nrow(dat)>0){
    
    
    if(type=="prob" & is.na(time_frame_days)){
      stop("Must have time frame for probability input")
    }
    
    if(type=="prob" & !is.na(time_frame_days)){
      
      # Make sure there is no probability of 1
      dat$mode   <- pmin(dat$mode,1-1e-6)
      dat$min    <- pmin(dat$min,1-1e-6)
      dat$max    <- pmin(dat$max,1-1e-6)
      dat$trueMax<- pmin(dat$trueMax,1-1e-6)
      dat$trueMin<- pmin(dat$trueMin,1-1e-6)

      
      # convert to daily probability
      dat$mode   <- (dat$mode)^(1/time_frame_days)
      dat$min    <- (dat$min)^(1/time_frame_days)
      dat$max    <- (dat$max)^(1/time_frame_days)
      dat$trueMax<- (dat$trueMax)^(1/time_frame_days)
      dat$trueMin<- (dat$trueMin)^(1/time_frame_days)
      
      # cat("\n daily probability df is \n")
      # cat(str(dat))
      # cat("\n")
      
      dat$mode   <-  1 / -log(dat$mode)
      dat$min    <-  1 / -log(dat$min)
      dat$max    <-  1 / -log(dat$max)
      dat$trueMax<-  1 / -log(dat$trueMax)
      dat$trueMin<-  1 / -log(dat$trueMin)
      
    }
    
    if(type=="time"){} # do nothing
    
    
  max_days<-max(dat$trueMax,na.rm=T)*100
  
  # cat("\ncreating days\n")
  # cat("\nmax days is \n")
  # cat(max_days)
  days<-data.frame(days=unique(c(dat$mode,dat$min,dat$max,dat$trueMin,dat$trueMax,
                                 # seq(from=0,to=max(max_days,365*2),
                                 #     length.out=max(floor(x_axis_precision/10),max(max_days,365*2)))
                                 seq(from=0,to=365*10,by=.25)
                                 )))
  # cat("\ncreated days\n")
  dat_split<-split(dat,dat$alternative)
  
  surv_df<-plyr::rbind.fill(lapply(dat_split,function(df){
    
    df2<-merge(df,days)
    
    resu<-df2%>%
      dplyr::mutate(av_surv=exp((-1/mode)*days),
             lcl_surv=exp((-1/trueMin)*days),
             ucl_surv=exp((-1/trueMax)*days))%>%
      dplyr::select(alternative,question,av_surv,lcl_surv,ucl_surv,days)
      
    return(resu)
    
  }))
  }else{
    surv_df<-data.frame(av_surv=0,ucl_surv=0,lcl_surv=0,days=0,alternative="a")[0,]
  }

  # cat("\nFinished generating surv df\n")
  # cat(str(surv_df))
  # cat("\n")
  return(list(df=surv_df))

}

DecayPlot<-function(basedf,survPars,
                    input,
                    alt_index,
                    altTitles,
                    invertPlot=FALSE,
                    manual_palette,
                    ylab="Survival Probability",
                    xlab="Days",
                    x_factor=1/1,
                    legend_title,
                    fixed_year=FALSE,
                    manual_xmax=NULL,
                    cex=1.5,
                    # manual_palette_names,
                    prob_cutoff=.01){
  
  
  # cat("Start generating surv plot")
  # cat("\n surv_df is \n")
  # cat(str(survPars$df))
  # cat("\n max days on the plot is \n")
  # cat(max(survPars$df$days))
  # 
  user_xmax<-input[[paste("SurvPlotXMax_",alt_index,sep="")]]
  
  plotAllAlts<-input[[paste0("PlotAllAlts_",alt_index,sep="")]]
  
  
  if(length(user_xmax)>0){
    if(!is.na(user_xmax)){
    manual_xmax<-user_xmax
  }}
  
  if(invertPlot){
    survPars$df<-survPars$df%>%
      dplyr::mutate(av_surv=1-av_surv,
                    ucl_surv=1-ucl_surv,
                    lcl_surv=1-lcl_surv)
    
    prob_cutoff<-1-prob_cutoff
    
    
    # cat("\n surv_df is inverted: \n")
    # cat(str(survPars$df))
    # cat("\n")
    
    
    
    
    xlim<-survPars$df%>%
      dplyr::filter(ucl_surv>prob_cutoff)%>%
      dplyr::group_by(alternative)%>%
      dplyr::filter(days==min(days))%>%
      pull(days)%>%
      max()%>%
      c(0)%>%
      rev()
  }else{
    xlim<-survPars$df%>%
      dplyr::filter(ucl_surv<prob_cutoff)%>%
      dplyr::group_by(alternative)%>%
      dplyr::filter(days==min(days))%>%
      pull(days)%>%
      max()%>%
      c(0)%>%
      rev()
    
  }
  
  if(fixed_year){xlim<-c(0,365)}
  
  if(!is.null(manual_xmax)){
    xlim<-c(0,manual_xmax/x_factor)
  }
  
  # cat("\n xlim is \n")
  # cat(str(xlim))
  
  palette<-manual_palette
  # names(palette)<-manual_palette_names

  
  survPars$df<-survPars$df%>%
    mutate(time=days*x_factor)
  
  survPars$df<-survPars$df%>%
    filter(days<=xlim[2]
           ,av_surv>0)
  
  if(!plotAllAlts){
    survPars$df<-survPars$df%>%
      filter(alternative==altTitles[alt_index])
  }
  
  # cat("\n surv_df is filtered: \n")
  # cat(str(survPars$df))
  # cat("\n")
  
  xlim<-xlim*x_factor
  

  
  active_df<-survPars$df%>%
    dplyr::filter(alternative==altTitles[alt_index])
  
  # days_to_mark<-survPars$df%>%
    # dplyr::filter(alternative==unique(active_df$alternative),question==unique(active_df$question))%>%
    # dplyr::select(min,mode,max)
  
  # days_to_mark<-as.vector(unlist(days_to_mark))
  
  # mark_df<-active_df%>%
  #   dplyr::filter(days%in%days_to_mark)%>%
  #   dplyr::arrange(days)%>%
  #   dplyr::mutate(x=days,y=av_surv)%>%
  #   dplyr::select(x,y,alternative)
  
  # extra_y_breaks<-mark_df$y
  # extra_x_breaks<-mark_df$x
  
  # write.csv(mark_df,"mark.csv")
  
  
  
  
  inactive_df<-survPars$df%>%
    dplyr::filter(alternative!=altTitles[alt_index])
  
  gg <- ggplot(survPars$df, aes(x = time, y = av_surv)) +
    geom_ribbon(
      data = inactive_df,
      aes(ymin = lcl_surv, ymax = ucl_surv, fill = alternative),
      color = NA, alpha = .5
    ) +
    geom_line(
      data = inactive_df,
      aes(color = alternative), alpha = .67
    ) +
    geom_ribbon(
      data = active_df,
      aes(ymin = lcl_surv, ymax = ucl_surv, fill = alternative),
      color = NA, alpha = .5
    ) +
    geom_line(
      data = active_df,
      aes(color = alternative), alpha = 1
    ) +
    labs(
      # title = "Mortality Curve for Disease",
      x = xlab,
      fill = legend_title,
      color = legend_title,
      y = ylab
    ) +
    # scale_x_continuous(limits = xlim) +
    scale_x_continuous(expand = expansion(mult=c(0,0)),limits = xlim)+
    scale_fill_manual(values = palette) +
    scale_color_manual(values = palette) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12 * cex),  # Scale axis titles
      axis.text = element_text(size = 10 * cex),   # Scale axis text
      legend.title = element_text(size = 12 * cex), # Scale legend title
      legend.text = element_text(size = 10 * cex), # Scale legend text
      plot.title = element_text(size = 14 * cex, face = "bold"), # Scale plot title
      plot.subtitle = element_text(size = 12 * cex), # Scale subtitle
      legend.key.height = unit(0.75 * cex, "cm"),  # Adjust legend key height
      legend.spacing.y = unit(0.5 * cex, "cm")     # Adjust spacing between legend items
    )
  
  
  # cat("Finished generating surv plot")
  
  return(gg)
  
  
}
