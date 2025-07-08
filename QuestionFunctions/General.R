ElictSummary_multi<-function(responses,altTitles,question_field,fullName){
  
  ui<-fluidPage(
    # tags$head(tags$style(
    # HTML('
    #      #sidebar {
    #         ;
    #     }
    # 
    #     body, label, input, button, select { 
    #       font-family: "Arial";
    #     }')
    # )),
    useShinyjs(),
    div(style="display:inline-block",
        actionButton(inputId = "load",label = "Load question")),
    div(style="display:inline-block",uiOutput("fullRangeInput")),
    uiOutput("FullQuestionUI"))
  
  
  
  server<-function(input,output){
    
    df<-responses%>%
      filter(question==question_field)%>%
      arrange(desc(email))%>%
      mutate(expert=as.factor(as.numeric(as.factor(email))))
    
    
    ### When you press the load button
    observeEvent({input$load},
                 {
                   removeUI("#load")
                   output$FullQuestionUI<-renderUI({navbarPage("Alternatives",
                                                               
                                                               # 
                                                               # Tab for alternative 1
                                                               #
                                                               
                                                               tabPanel(altTitles[1],
                                                                        sidebarLayout(div(sidebarPanel(width = 5,DTOutput(paste("table_1","",sep=""))),
                                                                                          style="background-color:rgba(0, 0, 0, 0)"),
                                                                                      mainPanel(width = 7,plotOutput(paste("plot_1",sep=""))))),
                                                               # 
                                                               # Tab for alternative 2
                                                               #
                                                               
                                                               tabPanel(altTitles[2],
                                                                        sidebarLayout(div(sidebarPanel(width = 5,DTOutput(paste("table_2","",sep=""))),
                                                                                          style="background-color:rgba(0, 0, 0, 0)"),
                                                                                      mainPanel(width = 7,plotOutput(paste("plot_2",sep=""))))),
                                                               # 
                                                               # Tab for alternative 3
                                                               #
                                                               
                                                               
                                                               tabPanel(altTitles[3],
                                                                        sidebarLayout(div(sidebarPanel(width = 5,DTOutput(paste("table_3","",sep=""))),
                                                                                          style="background-color:rgba(0, 0, 0, 0)"),
                                                                                      mainPanel(width = 7,plotOutput(paste("plot_3",sep="")))))
                                                               
                   )
                   })
                 })
    
    
    # Visualizations
    
    output$table_1<-DT::renderDT({
      resu<-df%>%filter(alternative==altTitles[1])%>%
        arrange(expert)%>%
        select(expert,min,mode,max,confidence)
      
      colnames(resu)<-c("Expert","Minimum","Most Likely","Maximum","Confidence")
      
      dtable<-DT::datatable(resu,rownames = FALSE,autoHideNavigation = T,filter = c("none"),options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:4)),
        dom = 't'))%>%
        DT::formatRound(columns=c('Minimum',"Most Likely","Maximum"), digits=3)
      dtable
    })
    
    output$plot_1<-renderPlot({
      
      df0<-df%>%
        filter(alternative==altTitles[1])%>%
        mutate(dist="pert",min0=min,max0=max)%>%
        mutate(min=trueMin,max=trueMax)
      
      df_split<-split(df0,df0$expert)
      
      
      PDF.dat<-priorPDF(L = df_split)%>%
        mutate(expert=par)
      
      
      DF<-PDF.dat%>%
        mutate(x=values,y=PDF,color=expert)  
      
      
      lim<-c(min(PDF.dat$values),max(PDF.dat$values)) 
      
      # markDF<-rbind.fill(dat.dist%>%
      # mutate(x=max,alternative=alternative)%>%
      # select(x,alternative),
      # dat.dist%>%
      # mutate(x=min,alternative=alternative)%>%
      # select(x,alternative))
      
      plot.lims<-lim		
      
      gg<-ggplot(data=DF%>%
                   group_by(expert)%>%
                   mutate(y=y),aes(x=x,y=y,color=expert))+
        ylab("Probability Density Function")+
        xlab(fullName)+
        # geom_area(data=anchorPDF%>%
        # mutate(y=y/max(anchorPDF$y)),aes(x=x,y=y,fill=origin),alpha=.5)+
        # geom_vline(data=markDF,
        # aes(xintercept=x,color=factor(alternative)),
        # lty="dashed")+
        geom_line()+
        scale_color_discrete(name="Expert")+
        scale_y_continuous(expand = expansion(mult=c(0,.05)))+
        # guides()+
        theme()+
        scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
      gg
      
    })
    
    output$table_2<-DT::renderDT({
      resu<-df%>%filter(alternative==altTitles[2])%>%
        arrange(expert)%>%
        select(expert,min,mode,max,confidence)
      
      colnames(resu)<-c("Expert","Minimum","Most Likely","Maximum","Confidence")
      
      dtable<-DT::datatable(resu,rownames = FALSE,autoHideNavigation = T,filter = c("none"),options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:4)),
        dom = 't'))%>%
        DT::formatRound(columns=c('Minimum',"Most Likely","Maximum"), digits=3)
      dtable
    })
    
    output$plot_2<-renderPlot({
      
      df0<-df%>%
        filter(alternative==altTitles[2])%>%
        mutate(dist="pert",min0=min,max0=max)%>%
        mutate(min=trueMin,max=trueMax)
      
      df_split<-split(df0,df0$expert)
      
      
      PDF.dat<-priorPDF(L = df_split)%>%
        mutate(expert=par)
      
      
      DF<-PDF.dat%>%
        mutate(x=values,y=PDF,color=expert)  
      
      
      lim<-c(min(PDF.dat$values),max(PDF.dat$values)) 
      
      # markDF<-rbind.fill(dat.dist%>%
      # mutate(x=max,alternative=alternative)%>%
      # select(x,alternative),
      # dat.dist%>%
      # mutate(x=min,alternative=alternative)%>%
      # select(x,alternative))
      
      plot.lims<-lim		
      
      gg<-ggplot(data=DF%>%
                   group_by(expert)%>%
                   mutate(y=y),aes(x=x,y=y,color=expert))+
        ylab("Probability Density Function")+
        xlab(fullName)+
        # geom_area(data=anchorPDF%>%
        # mutate(y=y/max(anchorPDF$y)),aes(x=x,y=y,fill=origin),alpha=.5)+
        # geom_vline(data=markDF,
        # aes(xintercept=x,color=factor(alternative)),
        # lty="dashed")+
        geom_line()+
        scale_color_discrete(name="Expert")+
        scale_y_continuous(expand = expansion(mult=c(0,.05)))+
        # guides()+
        theme()+
        scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
      gg
      
    })
    
    output$table_3<-DT::renderDT({
      resu<-df%>%filter(alternative==altTitles[3])%>%
        arrange(expert)%>%
        select(expert,min,mode,max,confidence)
      
      colnames(resu)<-c("Expert","Minimum","Most Likely","Maximum","Confidence")
      
      dtable<-DT::datatable(resu,rownames = FALSE,autoHideNavigation = T,filter = c("none"),options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:4)),
        dom = 't'))%>%
        DT::formatRound(columns=c('Minimum',"Most Likely","Maximum"), digits=3)
      dtable
    })
    
    output$plot_3<-renderPlot({
      
      df0<-df%>%
        filter(alternative==altTitles[3])%>%
        mutate(dist="pert",min0=min,max0=max)%>%
        mutate(min=trueMin,max=trueMax)
      
      df_split<-split(df0,df0$expert)
      
      
      PDF.dat<-priorPDF(L = df_split)%>%
        mutate(expert=par)
      
      
      DF<-PDF.dat%>%
        mutate(x=values,y=PDF,color=expert)  
      
      
      lim<-c(min(PDF.dat$values),max(PDF.dat$values)) 
      
      # markDF<-rbind.fill(dat.dist%>%
      # mutate(x=max,alternative=alternative)%>%
      # select(x,alternative),
      # dat.dist%>%
      # mutate(x=min,alternative=alternative)%>%
      # select(x,alternative))
      
      plot.lims<-lim		
      
      gg<-ggplot(data=DF%>%
                   group_by(expert)%>%
                   mutate(y=y),aes(x=x,y=y,color=expert))+
        ylab("Probability Density Function")+
        xlab(fullName)+
        # geom_area(data=anchorPDF%>%
        # mutate(y=y/max(anchorPDF$y)),aes(x=x,y=y,fill=origin),alpha=.5)+
        # geom_vline(data=markDF,
        # aes(xintercept=x,color=factor(alternative)),
        # lty="dashed")+
        geom_line()+
        scale_color_discrete(name="Expert")+
        scale_y_continuous(expand = expansion(mult=c(0,.05)))+
        # guides()+
        theme()+
        scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
      gg
      
    })
    
    
  }
  
  return(shinyApp(ui,server))
  
}

ElictSummary_single<-function(responses,altTitles,question_field,fullName){
  
  ui<-fluidPage(
    # tags$head(tags$style(
    # HTML('
    #      #sidebar {
    #         ;
    #     }
    # 
    #     body, label, input, button, select { 
    #       font-family: "Arial";
    #     }')
    # )),
    useShinyjs(),
    div(style="display:inline-block",
        actionButton(inputId = "load",label = "Load question")),
    div(style="display:inline-block",uiOutput("fullRangeInput")),
    uiOutput("FullQuestionUI"))
  
  
  
  server<-function(input,output){
    
    df<-responses%>%
      filter(question==question_field)%>%
      arrange(desc(email))%>%
      mutate(expert=as.factor(as.numeric(as.factor(email))))
    
    
    ### When you press the load button
    observeEvent({input$load},
                 {
                   removeUI("#load")
                   output$FullQuestionUI<-renderUI({
                     sidebarLayout(div(sidebarPanel(width = 5,DTOutput(paste("table_1","",sep=""))),
                                       style="background-color:rgba(0, 0, 0, 0)"),
                                   mainPanel(width = 7,plotOutput(paste("plot_1",sep=""))))
                     
                     
                   })
                 })
    
    
    # Visualizations
    
    output$table<-DT::renderDT({
      resu<-df%>%filter(alternative==altTitles[1])%>%
        arrange(expert)%>%
        select(expert,min,mode,max,confidence)
      
      colnames(resu)<-c("Expert","Minimum","Most Likely","Maximum","Confidence")
      
      dtable<-DT::datatable(resu,rownames = FALSE,autoHideNavigation = T,filter = c("none"),options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:4)),
        dom = 't'))%>%
        DT::formatRound(columns=c('Minimum',"Most Likely","Maximum"), digits=3)
      dtable
    })
    
    output$plot<-renderPlot({
      
      df0<-df%>%
        filter(alternative==altTitles[1])%>%
        mutate(dist="pert",min0=min,max0=max)%>%
        mutate(min=trueMin,max=trueMax)
      
      df_split<-split(df0,df0$expert)
      
      
      PDF.dat<-priorPDF(L = df_split)%>%
        mutate(expert=par)
      
      
      DF<-PDF.dat%>%
        mutate(x=values,y=PDF,color=expert)  
      
      
      lim<-c(min(PDF.dat$values),max(PDF.dat$values)) 
      
      # markDF<-rbind.fill(dat.dist%>%
      # mutate(x=max,alternative=alternative)%>%
      # select(x,alternative),
      # dat.dist%>%
      # mutate(x=min,alternative=alternative)%>%
      # select(x,alternative))
      
      plot.lims<-lim		
      
      gg<-ggplot(data=DF%>%
                   group_by(expert)%>%
                   mutate(y=y),aes(x=x,y=y,color=expert))+
        ylab("Probability Density Function")+
        xlab(fullName)+
        # geom_area(data=anchorPDF%>%
        # mutate(y=y/max(anchorPDF$y)),aes(x=x,y=y,fill=origin),alpha=.5)+
        # geom_vline(data=markDF,
        # aes(xintercept=x,color=factor(alternative)),
        # lty="dashed")+
        geom_line()+
        scale_color_discrete(name="Expert")+
        scale_y_continuous(expand = expansion(mult=c(0,.05)))+
        # guides()+
        theme()+
        scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
      gg
      
    })
    
    
  }
  
  return(shinyApp(ui,server))
  
}



trainingApp<-function(question_field="",suffix="",defaultDF,
                      plotMin=NULL,plotMax=NULL,queryAdd="",
                      propVizDivisor=1,invertProp=FALSE,fullName="",
                      naturalMin=0,naturalMax=1,
                      dataType=c("proportion","count"),
                      AnchorLabel="Test data",
                      height=460
                      # ,solvePERTprecision=.03,
                      # solvePERTMax=1,solvePERTMin=0
){
  
  ui<-fluidPage(
    useShinyjs(),
    tags$div(id = "placeholder_text",div(id="question1",div(style="display:inline-block",
                                                            HTML("<b>What is the chance of a dropped marmite toast landing on the marmite-covered side  (On as scale from 0 to 1)?</b><br>")))),hr(),
    uiOutput("FullQuestionUI"),
    div(style="display:inline-block",
        actionButton(inputId = "Answer1",label = "Submit answer"))
  )
  
  options <- list(height = height)
  
  server <- function(input, output, session) {
    
    answer_step<-0
    stepEnv<-where("answer_step")
    anchorToggle<-NULL
    anchorToggleEnv<-where("anchorToggle")
    PERTsolved<-list(trueMax=NA,trueMin=NA,RMSE=NA)
    PERTsolvedEnv<-where("PERTsolved")
    
    anchor<-data.frame(mean=55.12/100,sd=9.048/100,dist="norm")
    if(!is.null(anchor)){
      anchorPDF<-priorPDF(list(anchor=anchor),precision=200)%>%
        mutate(origin=AnchorLabel,x=values,y=PDF)}else{
          anchorPDF<-data.frame(origin="",x=0,y=0)[0,]}
    
    output$FullQuestionUI<-renderUI({
      sidebarLayout(
        sidebarPanel =
          PERTSidebar(question_field=question_field,queryAdd=queryAdd,
                      defaultQuestion=defaultDF),
        mainPanel =  mainPanel(
          tags$div(id = "placeholder_plot",plotOutput("plotPDF0")))
      )
      #Uncomment this for phase 2!
      # tabPanel("Answer Summary",
      # tableOutput(paste("recordtable",suffix,sep=""))),
    })
    
    
    ###################################################
    ####Code that automatically saves every changes in 
    ####questionnaire from the user (v1.2)###
    #############################
    
    observeEvent({
      input$Min
      input$Mode
      input$Max
      input$Confid
    },
    {
      
      
      PERTcondition<-all(!sapply(input,is.na))
      PERTcondition<- PERTcondition & 
        na2false(input$Min <= input$Mode) & 
        na2false(input$Mode <= input$Max) &
        na2false(input$Min != input$Max)  &
        na2false(input$Min >= naturalMin*propVizDivisor) &
        na2false(input$Max <= naturalMax*propVizDivisor)
      
      if(PERTcondition){
        assign("PERTsolved",solvePERT(Mode = input$Mode,
                                      Min = input$Min,
                                      Max = input$Max,
                                      Shape = 4,
                                      Conf = input$Confid/100,
                                      naturalMax = naturalMax*propVizDivisor,
                                      naturalMin = naturalMin*propVizDivisor),
               envir = PERTsolvedEnv)
        
        newdf<-data.frame(name=name,email=email,
                          question="Training",
                          min=input$Min,
                          mode=input$Mode,
                          max=input$Max,
                          confidence=input$Confid,
                          finishedQuestion=0,
                          shape=4)
        
        temp<-rbind.fill(basedf,newdf)%>%
          filter(!duplicated(question,fromLast = TRUE))
        
        assign(x = "basedf",value = temp , basedfEnv)  
        
      }else{
        newdf<-data.frame(name=name,email=email,
                          question=question_field,
                          alternative=0,
                          min=input$Min,
                          mode=input$Mode,
                          max=input$Max,
                          trueMin=NA,
                          trueMax=NA,
                          confidence=0,
                          finishedQuestion=input$checkbox,
                          timestamp=now(),
                          shape=0)
        temp<-rbind.fill(basedf,newdf)%>%
          dplyr::arrange(desc(timestamp))%>%
          dplyr::filter(!duplicated(
            data.frame(question,alternative),fromLast = F))
        
        assign(x = "basedf",value = temp , basedfEnv)
      }
      ###########################################
      ###Plot probabilidy density function######
      ##########################################
      output$plotPDF0<-renderPDFPlot_Anchor(input,
                                            fullName=fullName,
                                            anchor=anchorToggle,
                                            question_field=question_field,
                                            naturalMax = naturalMax*propVizDivisor,
                                            trueMax=PERTsolved$trueMax,trueMin=PERTsolved$trueMin,
                                            fullRangePlot = input$useFullRange,
                                            naturalMin = naturalMin*propVizDivisor)     
      
      
      #}
    })
    
    
    
    
    observeEvent({input$Answer1},{
      
      if(answer_step==0){
        assign(x="anchorToggle",
               value = anchorPDF,
               envir = anchorToggleEnv)
        
        showModal(modalDialog(title = NULL,
                              "Thank you for your answer. The next stage in the expert elicitation is comparing your answer to other answers (or data)",
                              size = "s",easyClose = FALSE,fade = FALSE,footer = modalButton("Move to round 2 of this training")))
        
        removeUI(selector="#question1")
        insertUI(selector = "#placeholder_text",ui = div(id="question1",div(style="display:inline-block",
                                                                            HTML("<b>Now that you have seen your answer in comparison with existing data (in this case 25 drops of a piece of toast), do you want to adjust your answer?</b><br>"))))
        
        output$plotPDF0<-renderPDFPlot_Anchor(input,
                                              fullName=fullName,
                                              anchor=anchorToggle,
                                              question_field=question_field,
                                              naturalMax = naturalMax*propVizDivisor,
                                              trueMax=PERTsolved$trueMax,trueMin=PERTsolved$trueMin,
                                              fullRangePlot = input$useFullRange,
                                              naturalMin = naturalMin*propVizDivisor)  
        
      }
      
      
      if(answer_step==1){showModal(modalDialog(title = "",
                                               "Thank you very much for completing this training. Now, we will use expert elicitations in the same fashion to estimate impacts of proposed Kuaka/Whenua Hou Diving Petrel management alternatives on other species. However, instead of communicating real world data following the first round, we will host an online meeting to discuss estimates averaged from all experts, after which you will be able to adjust your answers if you wish.",
                                               size = "s",easyClose = TRUE,fade = FALSE,footer = NULL))
        
      }    
      assign("answer_step",value = answer_step+1,envir = stepEnv)
      
    })
  }
  
  shinyApp(ui = ui,server = server,options=options)
}


renderPDFPlot<-function(input,plotMin=NULL,plotMax=NULL,
                        question_field="",
                        fullName="",
                        naturalMin=-Inf,naturalMax=Inf,
                        trueMax=numeric(),trueMin=numeric(),
                        fullRangePlot=TRUE){
  renderPlot({
    plotAvailable<-all(!sapply(input,is.na))
    plotAvailable<- plotAvailable & 
      na2false(input$Min <= input$Mode) & 
      na2false(input$Mode <= input$Max) &
      na2false(input$Min != input$Max) &
      na2false(input$Min >= naturalMin) &
      na2false(input$Max <= naturalMax) 
    
    
    if(!plotAvailable){
      nullPlot<-TRUE
      DF=data.frame(x=0,y=0)[0,]
      markDF<-data.frame(x=0,y=0)[0,]
      plot.lims<-c(0,0)
    }else{
      
      lim<-qpert(p = c(0,1),
                 min  = trueMin,
                 max  = trueMax,
                 mode = input$Mode,
                 shape=4)  
      
      
      values<-seq(from=min(ifelse(!is.null(plotMin),plotMin,lim[1]),trueMin),
                  to=max(ifelse(!is.null(plotMax),plotMax,lim[2]),trueMax),
                  length.out=1e4)
      plot.lims<-lim
      dens<-dpert(values,min=trueMin,max = trueMax,mode = input$Mode,shape=4)
      DF<-data.frame(x=values,y=dens)
      markDF<-data.frame(x=c(input$Min,input$Mode,input$Max))
      nullPlot=FALSE
    }
    ggplot(data=DF,aes(x=x,y=y))+
      ylab("Probability Density Function")+xlab(fullName)+
      geom_vline(data=markDF,aes(xintercept=x),lty="dashed")+
      geom_line()+
      scale_y_continuous(expand = expansion(mult=c(0,.05)))+
      scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
    
    
  })
}

renderPDFPlot_Anchor<-function(input,suffix="",plotMin=NULL,plotMax=NULL,
                               question_field="",previewAnchor=FALSE,
                               fullName="",
                               anchor=NULL,
                               naturalMin=-Inf,naturalMax=Inf,
                               trueMax=numeric(),trueMin=numeric(),
                               fullRangePlot=TRUE){
  renderPlot({
    
    validInput<-all(!sapply(input,is.na))
    validInput<- (validInput & 
                    na2false(input[[paste("Min",suffix,sep="")]] <=
                               input[[paste("Mode",suffix,sep="")]]) & 
                    na2false(input[[paste("Mode",suffix,sep="")]]
                             <= input[[paste("Max",suffix,sep="")]]) &
                    na2false(input[[paste("Min",suffix,sep="")]]
                             != input[[paste("Max",suffix,sep="")]]) &
                    na2false(input[[paste("Min",suffix,sep="")]] >=
                               naturalMin) &
                    na2false(input[[paste("Max",suffix,sep="")]] <=
                               naturalMax) )
    
    plotAvailable<-validInput | previewAnchor
    
    if(!is.null(anchor)){
      anchorPDF<-anchor
      anch.lim<-range(anchorPDF$x)
    }else{
      anchorPDF<-data.frame(origin="",x=0,y=0)[0,]  
      anch.lim<-c(NA,NA)
    }
    
    
    if(!plotAvailable){
      
      nullPlot<-TRUE
      DF=data.frame(x=0,y=0)[0,]
      markDF<-data.frame(x=0,y=0)[0,]
      plot.lims<-c(0,0)
      
    }else{
      
      
      ### Defining limit of the plot based on the distribution
      # if plotting the three distributions at the same time, things will get messy.
      
      if(validInput){
        lim<-qpert(p = c(0,1),
                   min  = trueMin,
                   max  = trueMax,
                   mode = input$Mode,
                   shape=4) 
        
        values<-seq(from=min(ifelse(!is.null(plotMin),plotMin,lim[1]),
                             trueMin),
                    to=max(ifelse(!is.null(plotMax),plotMax,lim[2]),
                           trueMax),
                    length.out=1e4)
        
        dens<-dpert(values,min=trueMin,max = trueMax,mode = input$Mode,shape=4)
        DF<-data.frame(x=values,y=dens,origin="Expert")
        markDF<-data.frame(x=c(input$Min,input$Mode,input$Max))
        
        plot.lims<-lim
      }else{
        plot.lims<-c(NA,NA)
        DF=data.frame(x=0,y=0)[0,]
        markDF<-data.frame(x=0,y=0)[0,]
      }
      
      plot.lims[1]<-min(anch.lim[1],plot.lims[1],na.rm=T)
      plot.lims[2]<-max(anch.lim[2],plot.lims[2],na.rm=T)
      
      nullPlot=FALSE
    }
    
    # DF<-rbind.fill(DF,anchorPDF)
    
    ggplot(data=DF%>%
             mutate(y=y/max(DF$y)),aes(x=x,y=y))+
      ylab("Probability Density Function")+xlab(fullName)+
      geom_area(data=anchorPDF%>%
                  mutate(y=y/max(anchorPDF$y)),aes(x=x,y=y,fill=origin),alpha=.5)+
      geom_vline(data=markDF,aes(xintercept=x),lty="dashed")+
      geom_line()+
      scale_y_continuous(expand = expansion(mult=c(0,.05)))+
      # guides()+
      theme(legend.title = element_blank())+
      scale_x_continuous(expand = expansion(mult=c(0,.05)),limits = plot.lims)
    
    
  })
}

proportionApp<-function(question_field="",suffix="",defaultDF,
                        plotMin=NULL,plotMax=NULL,
                        queryAdd="",anchor=NULL,
                        useAnchorPDF=F,
                        count2multiplier=1,
                        propVizDivisor=1,invertProp=FALSE,fullName="",
                        naturalMin=0,naturalMax=1,absoluteCount=100,
                        AnchorLabel="Previous estimate",
                        height=500,
                        icon="Adult",
                        PaletteNames,PaletteColors,
                        dataType=c("proportion","count")
                        # ,solvePERTprecision=.03,
                        # solvePERTMax=1,solvePERTMin=0
){
  
  ui<-fluidPage(
    useShinyjs(),
    div(style="display:inline-block",
        actionButton(inputId = "load",label = "Load question")),
    div(style="display:inline-block",uiOutput("fullRangeInput")),
    uiOutput("FullQuestionUI")
  )
  
  options <- list(height = 580)
  
  server <- function(input, output, session) {
    gg<-ggplot(data = data.frame(x=1,y=1)[0,],aes(x=x,y=y))
    ggEnvir<-where(name = "gg")
    
    
    # Reactive list containing plot objects
    plot_reactives<-reactiveValues(PDF=gg,Prop1=gg)
    
    
    #Inverse Link Functions
    ILINK<-list(identity=function(x){x},
                inv.logit=function(x){inv.logit(x)},
                exp=function(x){exp(x)})
    
    
    # Declaring variables
    anchorToggle<-NULL
    anchorToggleEnv<-where("anchorToggle")
    PERTsolved<-list(trueMax=NA,trueMin=NA,RMSE=NA)
    PERTsolvedEnv<-where("PERTsolved")
    
    
    # anchor<-data.frame(mean=55.12,sd=9.048,dist="norm")
    if(!is.null(anchor) & useAnchorPDF){
      anchorPDF<-priorPDF(list(anchor=anchor),precision=200)%>%
        mutate(origin=AnchorLabel,x=values,y=PDF)%>%
        mutate(x=ILINK[[anchor$ilink]](x))}else{
          anchorPDF<-data.frame(origin="",x=0,y=0)[0,]}
    
    
    # List of two possible tabs for different data types
    tab2<-list(
      proportion=tabPanel("Numbers",
                          radioButtons(inputId = paste("plotVizControl_",suffix,sep=""),
                                       label = "",
                                       choices = 
                                         list("Lowest Plausible"="Min",
                                              "Most Likely"="Mode",
                                              "Highest Plausible"="Max"),
                                       selected = "Mode",inline = TRUE),
                          plotOutput(paste("plotVizProp",suffix,sep=""))),
      count2=tabPanel("Numbers",radioButtons(inputId = paste("plotVizControl",suffix,sep=""),
                                             label = "",
                                             choices = 
                                               list("Lowest Plausible"="Min",
                                                    "Most Likely"="Mode",
                                                    "Highest Plausible"="Max"),
                                             selected = "Mode",inline = TRUE),
                      plotOutput(paste("plotVizCount2",suffix,sep=""))),
      count=tabPanel("Numbers",
                     plotOutput(paste("plotVizCount",suffix,sep="")))
    )
    
    
    observeEvent({input$load},
                 {
                   removeUI("#load")
                   # output$fullRangeInput<-renderUI({
                   #    checkboxInput(inputId = "useFullRange",
                   #                    label="Plot distribution over full possible range")
                   #   })
                   output$FullQuestionUI<-renderUI({
                     sidebarLayout(
                       sidebarPanel =
                         PERTSidebar(question_field=question_field,suffix = "",
                                     queryAdd=queryAdd,
                                     defaultQuestion=defaultDF),
                       mainPanel =  mainPanel(
                         tabsetPanel(type = "tabs",
                                     tab2[[dataType]],
                                     tabPanel("Probabilities",
                                              
                                              plotOutput(paste("plotPDF",suffix,sep=""))
                                              
                                     )
                                     # ,
                                     
                                     #Uncomment this for phase 2!
                                     # tabPanel("Answer Summary",
                                     # tableOutput(paste("recordtable",suffix,sep=""))),
                         )))
                     
                   })
                   
                   # output$recordtable<-renderTable({
                   # user_loadedDF<-read.csv("user_loaded.csv")
                   # currentAliasDF<-read.csv("currentAlias.csv")
                   # 
                   # # currentUser<-expert.aliases%>%filter(email%in%email)%>%dplyr::pull(alias)
                   # temp<-full.responses%>%
                   #   dplyr::filter(question==question_field,
                   #                 !alias%in%c("Average",as.character(currentAliasDF$x[1])))%>%
                   #   dplyr::select(alias,min,mode,max,shape,confidence)
                   # colnames(temp)<-c("Expert","Smaller plausible value","Most likely value","Greater plausible value","Shape Parameter","Confidence")
                   # if(user_loadedDF$x & responseSummary){temp}else{
                   #   temp<-data.frame("Not available"="")
                   #   temp
                   #   }
                   
                   # })       
                 })
    
    
    ###################################################
    ####Code that automatically saves every changes in 
    ####questionnaire from the user (v1.2)###
    #############################
    
    observeEvent({
      input$Min
      input$Mode
      input$Max
      input$Confid
      input$checkbox
      input$plotVizControl_
    },
    {
      
      PERTcondition<-all(!sapply(input,is.na))
      PERTcondition<- PERTcondition & 
        na2false(input$Min <= input$Mode) & 
        na2false(input$Mode <= input$Max) &
        na2false(input$Min != input$Max)  &
        na2false(input$Min >= naturalMin*propVizDivisor) &
        na2false(input$Max <= naturalMax*propVizDivisor)
      
      if(PERTcondition){
        assign("PERTsolved",solvePERT(Mode = input$Mode,
                                      Min = input$Min,
                                      Max = input$Max,
                                      Shape = 4,
                                      Conf = input$Confid/100,
                                      naturalMax = naturalMax*propVizDivisor,
                                      naturalMin = naturalMin*propVizDivisor),
               envir = PERTsolvedEnv)
        
        newdf<-data.frame(name=name,email=email,
                          question=question_field,
                          min=input$Min,
                          mode=input$Mode,
                          max=input$Max,
                          trueMax=PERTsolved$trueMax,
                          trueMin=PERTsolved$trueMin,
                          confidence=input$Confid,
                          alternative=0,
                          finishedQuestion=input$checkbox,
                          timestamp=now(),
                          shape=4)
        
        temp<-rbind.fill(basedf,newdf)%>%
          dplyr::arrange(desc(timestamp))%>%
          dplyr::filter(!duplicated(
            data.frame(question,alternative),fromLast = F))
        
        assign(x = "basedf",value = temp , basedfEnv)  
        
      }else{
        newdf<-data.frame(name=name,email=email,
                          question=question_field,
                          alternative=0,
                          min=input$Min,
                          mode=input$Mode,
                          max=input$Max,
                          trueMin=NA,
                          trueMax=NA,
                          confidence=0,
                          finishedQuestion=input$checkbox,
                          timestamp=now(),
                          shape=0)
        temp<-rbind.fill(basedf,newdf)%>%
          dplyr::arrange(desc(timestamp))%>%
          dplyr::filter(!duplicated(
            data.frame(question,alternative),fromLast = F))
        
        assign(x = "basedf",value = temp , basedfEnv)
      }
      ###########################################
      ###Plot probabilidy density function######
      ##########################################
      plot_reactives$PDF<-renderPDFPlot_Anchor_multi(
        basedf=basedf,
        altTitles = 0,
        input=input,
        fullName=fullName,
        anchor=anchorPDF,previewAnchor=TRUE,
        question_field=question_field,
        naturalMax = naturalMax*propVizDivisor,
        trueMax=PERTsolved$trueMax,trueMin=PERTsolved$trueMin,
        fullRangePlot = input$useFullRange,
        naturalMin = naturalMin*propVizDivisor)
      
      plot_reactives$Prop1<-CreatePropPlot(
        input = input,suffix="",icon=icon,propVizDivisor = propVizDivisor,
        PaletteNames = PaletteNames,
        PaletteColors = PaletteColors, 
        # anchor=NULL,
        anchor=anchor,
        invertProp=invertProp,absoluteCount = absoluteCount)
    })
    
    output$plotPDF<-renderPlot(plot_reactives$PDF)
    output$plotVizProp<-renderPlot(plot_reactives$Prop1)
    
    
    observeEvent({
      # input$load
      input$checkbox},{
        HasAnswers<-nrow(basedf)>0
        HasEmail<-email!="" & !is.null(email)
        noErrors<-HasAnswers & HasEmail
        if(noErrors){
          # token <- readRDS("droptoken.rds")
          
          coded_email<-gsub(x=email,"@","_at_")    
          filename<-paste(
            coded_email,"ts",
            round(as.numeric(now())),
            zero_pad(sample(1:(1e4-1),1),4),
            "response_expert.csv",sep="_")
          
          basedf$name<-name
          basedf$email<-email
          basedf<-basedf[,!str_detect(colnames(basedf),"X")]
          
          write.csv(basedf%>%
                      # dplyr::mutate(timestamp=as.numeric(now()),species=species)%>%
                      dplyr::ungroup(),
                    file = filename,row.names = FALSE)
          
          drop_upload(filename, path = "ShinyAppsStorage/Kuaka_Ocean/Answers",dtoken = token)
        }
        # shinyjs::disable("submit")
        
        
      }    
      
    )
  }
  
  shinyApp(ui = ui,server = server,options=list(height=height,port=5085))
}


SlideApp_old<-function(pattern,height=540){
  
  imgs <- list.files("./Images", pattern=pattern, full.names = TRUE)
  ui <- fluidPage(
    wellPanel(slickROutput("slickr",height="100%"))
  )
  
  server <- function(input, output) {
    
    output$slickr <- renderSlickR({
      slickR(obj = imgs)
    })
    
  }
  return(shinyApp(ui = ui, server = server,options=list(height=height)))
  
}


barPlotApp<-function(df,height=550){
  
  ui<-fluidPage(
    tabsetPanel(
      
      tabPanel("Table",DT::DTOutput("DT")),
      tabPanel("Barplot",HTML("Hover over bars for study details"),
               plotOutput("barplot",hover = hoverOpts("plot_hover", delay = 50, delayType = "debounce")),
               uiOutput("hover_info"),
               radioButtons(inputId ="height_one",label = NULL,inline = T,
                            choiceNames=c("Proportion","Absolute"),
                            choiceValues=c(FALSE,TRUE))))
  )  
  
  server<-function(input,output){
    
    
    df.display<-df
    df.display[,str_detect(colnames(df.display),"Pathogen")]<-NULL
    df.display[,str_detect(colnames(df.display),"_gg")]<-NULL
    df.display$ID<-NULL
    
    
    
    gg<-ggplot(data=df)
    
    reactPlot<-reactiveValues(plot=gg)
    
    df.melt<-reactive({
      
      df2<-df%>%
        filter(!is.na(Prevalence_gg))%>%
        # mutate(ID_0=ID,
        #        ID=as.numeric(as.factor(Study)))%>%
        ungroup()
      
      
      df2<-df2%>%arrange(desc(Prevalence_gg))
      
      ID.order<-df2$ID
      ID.order<-ID.order[!duplicated(ID.order)]
      
      resu<-reshape2::melt(df2,
                           id.vars=c("ID"),
                           measure.vars=c("Prevalence_gg","InvPrevalence_gg"),
                           value.name = "value")%>%
        mutate(value=as.numeric(value),ID=factor(ID,levels=ID.order))%>%
        mutate(variable=factor(recode(variable,
                                      Prevalence_gg="Prevalence",InvPrevalence_gg="Total"),
                               levels=c("Total","Prevalence")))
      
      resu<-merge(resu,df2%>%select(ID,Denominator_gg, Study,Species,Setting,Method),by="ID")%>%
        mutate(ID=as.numeric(ID))%>%
        # group_by(ID)%>%
        mutate(value=value*(as.numeric(Denominator_gg)^(input$height_one==T)))%>%
        ungroup%>%
        arrange(ID)
      # ID.order<-resu$ID_0
      # ID.order<-ID.order[!duplicated(ID.order)]
      # resu<-resu%>%
      #     mutate(ID_0=as.numeric(factor(ID_0,levels=ID.order)))
      
      
      return(resu)
    })
    
    df.melt.dummy<-reactive({expand.grid(y=seq(from=0,to=1,length.out=1e2),
                                         ID=unique(df.melt()$ID))%>%
        merge(df.melt()%>%
                filter(variable=="Prevalence")%>%
                select(ID,Denominator_gg, Study,Species,Setting,Method,value),
              by="ID")%>%
        mutate(y=y*(as.numeric(Denominator_gg)^(input$height_one==T)))
    })
    
    ylabs<-c("TRUE"="Numbers","FALSE"="Proportion") 
    
    output$barplot<-renderPlot({
      gg<-ggplot(df.melt(), aes(fill=variable, y=value, x=ID)) + 
        # scale_y_continuous(trans='log10')+
        
        geom_bar(position="stack", stat="identity",alpha=1)+
        geom_point(data=df.melt.dummy(),aes(x=ID,y=y),fill="black",alpha=0)+
        xlab("Study")+
        ylab(ylabs[input$height_one])+
        scale_fill_manual(values=c("Total"="#A8A8A8","Prevalence"="#EE9B00"))+
        theme(legend.title = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      gg
    })
    
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      point <- nearPoints(df.melt.dummy(), hover, xvar="ID",yvar="y",
                          threshold = 3, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", 
                      250,
                      # left_px-10, 
                      "px; top:", 
                      50,
                      # top_px-10, 
                      "px;")
      
      content<-paste0(sep="",point$Study,"<br> Species:",point$Species,
                      "<br> Method: ",point$Method,
                      "<br>Prevalence: ",round((point$value*100)/(as.numeric(point$Denominator_gg)^(input$height_one==T)),1),"%",
                      "<br>Sample Size: ",point$Denominator_gg)
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(content)))
    })
    
    output$DT <- renderDT({
      dtable<-DT::datatable(df.display,rownames = FALSE, 
                            options = list(
                              rowsGroup = list(0,1,2) # merge cells of column 1
                            ),filter = c("top"))
      
      path <- paste("./Libraries/datatables-rowsgroup-master",sep="")
      dep <- htmltools::htmlDependency(
        "RowsGroup", "2.0.0", 
        path, script = "dataTables.rowsGroup.js")
      
      dtable$dependencies <- c(dtable$dependencies, list(dep))
      
      dtable
    }) 
    # output$DT<-renderDT({df.melt()})
  }
  
  
  
  shinyApp(ui=ui,server=server,options=list(height=height))
  
}

upload_answers<-function(globalEnv,showModals=TRUE){    # Retrieve the objects from the global environment
    basedf <- get("basedf", envir = globalEnv)
    name <- get("name", envir = globalEnv)
    email_user <- get("email_user", envir = globalEnv)
    folder_id <- get("folder_id", envir = globalEnv)
    
    # Check if we have valid inputs
    HasAnswers <- nrow(basedf) > 0
    HasEmail <- email_user != "" & !is.null(email_user)
    noErrors <- HasAnswers & HasEmail
    
    if (noErrors) {
      # Prepare filename
      coded_email <- gsub(x = email_user, "@", "_at_")
      filename <- paste(
        "./Private/Answers/Temp/",
        coded_email, "_ts_",
        round(as.numeric(now())), "_",
        zero_pad(sample(1:(1e4 - 1), 1), 4), "_",
        "response_expert.csv", sep = ""
      )
      
      # Set name and email
      basedf$name <- name
      basedf$email <- email_user
      
      # Debugging to remove weird columns
      basedf <- basedf[, !str_detect(colnames(basedf), "X")]
      basedf$ExpertAlias<-NULL
      basedf$ExpertAlias.x<-NULL
      basedf$ExpertAlias.y<-NULL
      # cat(filename)
      
      # Write the dataframe to CSV
      write.csv(
        basedf %>%
          dplyr::ungroup(),
        file = filename, row.names = FALSE
      )
      
      # Upload the CSV to Google Drive
      drive_upload(media = filename, path = as_id(folder_id))
      
      if(showModals){
      showModal(modalDialog(
        title = NULL,
        paste("Answers saved successfully! Click outside this window if you want to close it and change your answers.",sep=""),
        size = "s",easyClose = T,fade = FALSE,footer = NULL))
      }
      # 
      # showModal(modalDialog(
      #   title = tags$div(
      #     "Modal Title",
      #     tags$button(
      #       type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close"
      #     )
      #   ),
      #   paste("Answers saved successfully! Click outside this window if you want to close it and change your answers.", sep = ""),
      #   size = "s",
      #   easyClose = TRUE,
      #   fade = FALSE,
      #   footer = NULL
      # ))
      
      
    }
    if (!HasEmail){
      if(showModals){
        
      showModal(modalDialog(
        title = NULL,
        paste("Could not save answers because no e-mail was provided. Click outside this window if you want to close it and change your answers.",sep=""),
        size = "s",easyClose = T,fade = FALSE,footer = NULL))
      }
      }
}

wrap_observe_copy_button <- function(input, output, session, globalEnv,paste_choices) {
  
  
observeEvent(input$paste_from_option, {
  
  
  if (input$options_to_paste_from == "") {
    showModal(modalDialog(
      title = "Error",
      "You did not select an alternative to copy from.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    
    showModal(modalDialog(
      title = "Confirm Copy",
      paste0("Are you sure you want to copy answers from '", names(paste_choices[paste_choices==input$options_to_paste_from]), "'? This will overwrite all current inputs."),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_paste", "Yes, Copy")
      )
    ))
  }
})
}



wrap_observe_record_button <- function(input, output, session, globalEnv) {
  observeEvent(input$record, 
  #              {
  #   
  #   # Retrieve the objects from the global environment
  #   basedf <- get("basedf", envir = globalEnv)
  #   name <- get("name", envir = globalEnv)
  #   email_user <- get("email_user", envir = globalEnv)
  #   folder_id <- get("folder_id", envir = globalEnv)
  #   
  #   # Check if we have valid inputs
  #   HasAnswers <- nrow(basedf) > 0
  #   HasEmail <- email_user != "" & !is.null(email_user)
  #   noErrors <- HasAnswers & HasEmail
  #   
  #   if (noErrors) {
  #     # Prepare filename
  #     coded_email <- gsub(x = email_user, "@", "_at_")
  #     filename <- paste(
  #       "./Private/Answers/",
  #       coded_email, "_ts_",
  #       round(as.numeric(now())), "_",
  #       zero_pad(sample(1:(1e4 - 1), 1), 4), "_",
  #       "response_expert.csv", sep = ""
  #     )
  #     
  #     # Set name and email
  #     basedf$name <- name
  #     basedf$email <- email_user
  #     
  #     # Debugging to remove weird columns
  #     basedf <- basedf[, !str_detect(colnames(basedf), "X")]
  #     basedf$ExpertAlias<-NULL
  #     basedf$ExpertAlias.x<-NULL
  #     basedf$ExpertAlias.y<-NULL
  #     # cat(filename)
  #     
  #     # Write the dataframe to CSV
  #     write.csv(
  #       basedf %>%
  #         dplyr::ungroup(),
  #       file = filename, row.names = FALSE
  #     )
  #     
  #     # Upload the CSV to Google Drive
  #     drive_upload(media = filename, path = as_id(folder_id))
  #     
  #     showModal(modalDialog(
  #       title = NULL,
  #       paste("Answers saved successfully! Click outside this window if you want to close it and change your answers.",sep=""),
  #       size = "s",easyClose = T,fade = FALSE,footer = NULL))
  #     # 
  #     # showModal(modalDialog(
  #     #   title = tags$div(
  #     #     "Modal Title",
  #     #     tags$button(
  #     #       type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close"
  #     #     )
  #     #   ),
  #     #   paste("Answers saved successfully! Click outside this window if you want to close it and change your answers.", sep = ""),
  #     #   size = "s",
  #     #   easyClose = TRUE,
  #     #   fade = FALSE,
  #     #   footer = NULL
  #     # ))
  #     
  #     
  #   }
  #   if (!HasEmail){
  #     showModal(modalDialog(
  #       title = NULL,
  #       paste("Could not save answers because no e-mail was provided. Click outside this window if you want to close it and change your answers.",sep=""),
  #       size = "s",easyClose = T,fade = FALSE,footer = NULL))
  #   }
  # }
  {upload_answers(globalEnv = globalEnv)}
  )
}

wrap_observe_expertise <- function(input, globalEnv,question_field){
  observeEvent({input$self_report_expertise_init},{
    # cat("\nobserve expertise\n")
    
    basedf<-get("basedf",envir = globalEnv)
    
    self_report_expertiseLoop<-input$self_report_expertise_init
    if(length(self_report_expertiseLoop)==0){
      self_report_expertiseLoop<-NA
    }
    
    q_df<-basedf%>%
      dplyr::filter(question==question_field)%>%
      dplyr::mutate(timestamp=as.numeric(now()),
                    self_reported_expertise=self_report_expertiseLoop)
    c_df<-basedf%>%
      dplyr::filter(question!=question_field)
    
    basedf<-rbind(q_df,c_df)
    
    
    # cat("\nanswerable!\n")
    if(tolower(as.character(input$self_report_expertise_init))%in%c("true","yes")){
      # cat("\nanswerable!\n")
      
      shinyjs::enable("load")
    }else{
    
    # if(input$self_report_expertise_init==FALSE){
      shinyjs::disable("load")
    }
    
    
    
    assign(x = "basedf",value = basedf,envir = globalEnv)
    upload_answers(globalEnv = globalEnv,showModals = FALSE)
    
  })
  
}

wrap_comment_observers<-function(input,
                                 # output,
                                 # session,
                                 globalEnv,serverEnv,
                                 alt_index,
                                 question_field,
                                 altTitles,
                                 defaultDF,
                                 InputCommentObserveName,
                                 CommentButtonName,
                                 InputCommentReactListName,
                                 InputCommentSubmitObserveName,
                                 CommentSubmitButtonName,
                                 CommentTextName){
  
  p<-alt_index
  email_user<-get("email_user",globalEnv)
  obsInputList<-get("obsInputList",serverEnv)
  reactInputList<-get("reactInputList",serverEnv)
  
  # Dynamically Creating Observers of Input Comment Open Dialog Change
  if(is.null(obsInputList[[InputCommentObserveName]])){
    obsInputList[[InputCommentObserveName]]<-observeEvent({
      input[[CommentButtonName]]
    },{
      
      # cat("Comment button clicked")
      
      # cat(olddf$comment)
      
      prev_comment<-reactInputList[[InputCommentReactListName]]()
      # print(str(prev_comment))
      # prev_comment<-defaultDF%>%
      #   dplyr::filter(question==question_field)%>%
      #   pull(comment)
      # cat(prev_comment)
      
      if(is.null(prev_comment) | is.na(prev_comment)){prev_comment<-""}
      if(prev_comment==""){
        placeholder<-"Type your answer here"
      }else{placeholder<-NULL}
      # cat()
      
      showModal(modalDialog(
        title = "Add your Comment",
        textAreaInput(inputId = CommentTextName, 
                      label = "Comment:",value = prev_comment,placeholder = placeholder, rows = 5, width = '100%'),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(paste0("ExplanationSubmit_",p,sep=""), "Submit")
        )
      ))
      
      # lapply(allInputs,shinyjs::enable)
      
    } #end of observer
    )
  }
  
  # Dynamically Creating Observers of Input Comment Submit Dialog Change
  if(is.null(obsInputList[[InputCommentSubmitObserveName]])){
    obsInputList[[InputCommentSubmitObserveName]]<-observeEvent({
      input[[CommentSubmitButtonName]]
    },{
      
      # cat("\nSubmit button clicked\n")
      # cat(altTitles[p])
      basedf<-get("basedf",envir = globalEnv)
      defaultAnswers<-get("defaultAnswers",envir = globalEnv)
      # cat(str(basedf))
      # cat("\n loaded email is \n")
      # cat(email_user)
      # cat("\n question is \n")
      # cat(question_field)
      # cat("\n alternative is \n")
      # cat(altTitles[p])
      
      olddf<-basedf%>%
        dplyr::filter(question==question_field,
                      email==email_user,
                      alternative==altTitles[p])%>%
        dplyr::arrange(desc(timestamp))%>%
        head(1)
      
      newdf<-olddf
      newdf$comment  <-input[[CommentTextName]]
      newdf$timestamp<-as.numeric(now())
      
      basedf<-rbind(newdf,basedf)%>%
        dplyr::arrange(desc(timestamp))%>%
        dplyr::filter(!duplicated(
          data.frame(question,alternative),fromLast = F))%>%
        dplyr::ungroup()
      
      
      temp<-rbind.fill(newdf,defaultAnswers)%>%
        dplyr::arrange(question,desc(timestamp))%>%
        dplyr::filter(!duplicated(
          data.frame(question,alternative)))
      
      reactInputList[[InputCommentReactListName]](input[[CommentTextName]])
      
      assign(x = "basedf",value = basedf , globalEnv)
      assign(x = "defaultAnswers",value = temp , globalEnv)
      removeModal()
      # lapply(allInputs,shinyjs::enable)
      
    } #end of observer
    )
  }
  
  # Dynamic reactive list of comments
  if(is.null(reactInputList[[InputCommentReactListName]])){
    
    prev_comment<-defaultDF%>%
      dplyr::filter(question==question_field,
                    alternative==altTitles[p])%>%
      pull(comment)
    
    if(is.null(prev_comment) | 
       length(prev_comment)==0){prev_comment<-""}
    
    reactInputList[[InputCommentReactListName]]<-reactiveVal(value = prev_comment)
    
    
  }
  
  assign(x = "obsInputList",value=obsInputList,serverEnv)
  assign(x = "reactInputList",value=reactInputList,serverEnv)
}


# wrap_observe_input_table<-function(input)

# Function to render the question UI, regardless of what type of app it is working on
renderQuestionUI<-function(outputEnv,tabs,copyUI=FALSE,paste_choices=character()
                           # ,self_reported_value
){
  
  output<-get("output",outputEnv)
  
  if(copyUI){
    ui_copy<-fluidRow(column(4,
                    selectInput(
                      inputId = "options_to_paste_from",
                      label = "Same answers as", 
                      choices = paste_choices,
                      selected =  ""
                    )
    ),
    column(4,
           div(style = "margin-top: 25px;",  # aligns the button with the dropdown vertically
               actionButton(inputId = "paste_from_option", label = "Copy answers"))
    ))
    
  }else{
    ui_copy<-""
  }

  output$FullQuestionUI<-renderUI({
    tagList(
    tags$head(
      tags$style(HTML("
            .selectize-dropdown {
            z-index: 3000 !important;
            }
            .selectize-input {
            z-index: 3000 !important;
            position: relative;
            }
            ")),
  tags$style(HTML("
    .modal {
      z-index: 5000 !important;
    }
    .modal-backdrop {
      z-index: 4999 !important;
    }
  ")),
    ),
    div(
    ui_copy,
    do.call(navbarPage,tabs),
             actionButton(inputId = "record", label = HTML("<b>Record provided answers</b>"))
      ))
    })
  
  assign(value = output,x = "output",envir = outputEnv)
  
  
}


self_report_UI<-function(selected=character(),label="Do you think the weka population is likely to change under this management strategy?"){
  
  radioButtons(inputId="self_report_expertise_init",
               label = label,
               choices = c("Yes"=TRUE,"No"=FALSE,"Unable to Answer"=NA),
               selected = selected,inline = TRUE)
}

expected_change_UI<-function(selected=character(),label="Do you think the weka population is likely to change under this management strategy?"){
  
  radioButtons(inputId="self_report_expertise_init",
               label = label,
               choices = c("Yes"="Yes","No"="No","Unable to Answer"="Unable to Answer"),
               selected = selected,inline = TRUE)
}


start_UI<-function(
    FUN=expected_change_UI,
    follow_up="If so, how many adult weka do you think will be on Kapiti by 2050 under this management strategy?",
    checkExpertise){
  
  if(checkExpertise){
    resu<- div(
             div(
               FUN(),
               follow_up,
               HTML("<br>")
             ),
      div(style="display:inline-block",
          shinyjs::disabled(
            actionButton(inputId = "load",label = "Load question"))
      )
      ,
      uiOutput("FullQuestionUI"))
    
  }else{
    
    resu<- div(
      div(style="display:inline-block",
          actionButton(inputId = "load",label = "Load question"))
      ,
      uiOutput("FullQuestionUI"))
    
  }
  
  return(resu)
}


updateAnswers<-function(allInputs,
                        serverEnv,
                        globalEnv,
                        alt_index,
                        altTitles,
                        MinName,
                        ModeName,
                        MaxName,
                        ConfidName,
                        InputCommentReactListName,
                        propVizDivisor,
                        naturalMin,
                        naturalMax,
                        question_field){
  
  p<-alt_index
  
  input<-get("input",envir = serverEnv)
  PERTsolved<-get("PERTsolved",envir = serverEnv)
  reactInputList<-get(x = "reactInputList",envir = serverEnv)
  basedf<-get(x = "basedf",envir = globalEnv)
  email_user<-get(x = "email_user",envir = globalEnv)
  
  # cat("\nObserver activated\n")
  lapply(allInputs,shinyjs::disable)
  
  PERTcondition0<-all(!sapply(input,is.na))
  PERTcondition<- PERTcondition0 &
    na2false(input[[MinName]] <= input[[ModeName]]) &
    na2false(input[[ModeName]] <= input[[MaxName]]) &
    na2false(input[[MinName]] != input[[MaxName]])  &
    na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
    na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
  # cat(PERTcondition)
  
  # cat("\nConvert reactive comments to static list\n")
  
  comment_list<-reactiveValuesToList(reactInputList)
  
  # cat(str(comment_list))
  
  self_report_expertiseLoop<-input$self_report_expertise_init
  if(length(self_report_expertiseLoop)==0){
    self_report_expertiseLoop<-NA
  }
  
  if(PERTcondition){
    PERTsolved[[p]]<-solvePERT(
      Mode = input[[ModeName]],
      Min = input[[MinName]],
      Max = input[[MaxName]],
      Shape = 4,
      Conf = input[[ConfidName]]/100,
      naturalMax = naturalMax*propVizDivisor,
      naturalMin = naturalMin*propVizDivisor)
    
    newdf<-data.frame(name="fooName",email=email_user,
                      question=question_field,
                      alternative=altTitles[p],
                      min=input[[MinName]],
                      mode=input[[ModeName]],
                      max=input[[MaxName]],
                      trueMin=PERTsolved[[p]]$trueMin,
                      trueMax=PERTsolved[[p]]$trueMax,
                      confidence=input[[ConfidName]],
                      comment = reactInputList[[InputCommentReactListName]](),
                      self_reported_expertise=self_report_expertiseLoop,
                      # finishedQuestion=input$checkbox,
                      timestamp=now(),
                      shape=4)
    
    basedf<-rbind.fill(basedf,newdf)%>%
      dplyr::arrange(desc(timestamp))%>%
      dplyr::filter(!duplicated(
        data.frame(question,alternative),fromLast = F)
      )%>%
      dplyr::ungroup()
    
    assign(x = "basedf",value = basedf , globalEnv)
    assign(x = "PERTsolved",value = PERTsolved , serverEnv)
    
    # }
  }else{
    newdf<-data.frame(name="fooName",email=email_user,
                      question=question_field,
                      alternative=altTitles[p],
                      min=input[[MinName]],
                      mode=input[[ModeName]],
                      max=input[[MaxName]],
                      trueMin=NA,
                      trueMax=NA,
                      confidence=0,
                      comment = reactInputList[[InputCommentReactListName]](),
                      # finishedQuestion=0,
                      timestamp=now(),
                      shape=0)
    
    basedf<-rbind.fill(basedf,newdf)%>%
      dplyr::arrange(desc(timestamp))%>%
      dplyr::filter(!duplicated(
        data.frame(question,alternative),fromLast = F))
    
    assign(x = "basedf",value = basedf , globalEnv)
    
    
  }
  lapply(allInputs,shinyjs::enable)
  
}



questionAppPreamble<-function(navbarFontSize,tabPanelFontSize,height,checkExpertise){
  
  styleCSS<-paste0(".navbar .navbar-nav {font-size: ",navbarFontSize,"px} 
                    .navbar .navbar-brand {font-size: ",navbarFontSize+2,"px}
                    .nav-tabs {font-size: ",tabPanelFontSize,"px}"
  )
  # Create UI
  ui<-fluidPage(
    tags$head(
      tags$style(type='text/css',styleCSS)),
    useShinyjs(),
    start_UI(checkExpertise = checkExpertise)
    # uiOutput("dummy")
  )
  
  options <- list(height = height)
  
  return(list(ui=ui,options=options))
}


find_problems_pert<-function(df,naturalMinEval,naturalMaxEval){
  df2<-df%>%
    mutate(invalid=is.na(min) | 
             is.na(mode) | 
             is.na(max) | 
             is.na(trueMax) | 
             is.na(trueMin) |
             min<naturalMinEval |
             max>naturalMaxEval |
             max<min | 
             max<mode |
             mode<min )%>%
    mutate(point=mode==max & mode==min)
  return(df2)
}


# Dummy tippy function

tippy_new<-function(text,tooltip,fontsize=14,allowHTML=TRUE){
  id<-paste("a",sample(1:1e5,1),sep="")
  tooltip2<-paste(
    "<span style='font-size:",fontsize,"px;'><p>",tooltip,"</p><span>",sep="")
  text<-paste("<span id=",id,"><u>",text,"</u></span>",sep="")
  
  return(tippy::tippy(
    # elementId = paste("#",id,sep=""),
    text=text,
    tooltip=tooltip2,
    allowHTML=TRUE))
}



round_to_nearest <- function(x, factor) {
  round(x / factor) * factor
}


# Function to output ggplot with error message
create_error_plot <- function(cex = 1, wrapping_factor = 40) {
  # Create a data frame with the error message
  df <- data.frame(x = 1, y = 1,
                   label = stringr::str_wrap("There is something wrong with the parameters you've entered",
                                             width = wrapping_factor))
                   
                   # Generate the plot
                   error_plot <- ggplot(df, aes(x = x, y = y)) +
                     geom_text(aes(label = label), size = 6 * cex, color = "red", hjust = 0.5, vjust = 0.5) +
                     theme_void() +
                     # labs(title = "Error Message")+
                     theme(plot.background = element_rect(fill = "white", color = "black"))
                   
                   # Return the plot
                   return(error_plot)
                   
                   
                   
}



# A helper to apply ilink functions
apply_ilink <- function(x, ilink_name) {
  switch(ilink_name,
         "inv.logit" = plogis(x),
         "exp" = exp(x),
         "identity" = x,
         stop(paste("Unknown ilink:", ilink_name)))
}

# Quick PERT fitting from samples
fit_pert_from_sample <- function(samples) {
  min_val <- min(samples)
  max_val <- max(samples)
  mode_val <- as.numeric(density(samples)$x[which.max(density(samples)$y)])
  return(list(min = min_val, mode = mode_val, max = max_val))
}

# Main function
refit_to_pert <- function(df, size = 50000, seed = NULL) {
  split_list <- split(df, df$label)
  
  results <- lapply(split_list, function(subdf) {
    param_list <- setNames(lapply(seq_len(nrow(subdf)), function(i) {
      dist <- subdf$dist[i]
      mean_val <- subdf$mean[i]
      sd_val <- subdf$sd[i]
      data.frame(
        mean = mean_val,
        sd = sd_val,
        dist = dist,
        stringsAsFactors = FALSE
      )
    }), nm = paste0("param", seq_len(nrow(subdf))))
    
    samples <- priorSampling(param_list, size = size, seed = seed)
    
    ilink_fun <- subdf$ilink[1]
    transformed_samples <- apply_ilink(unlist(samples), ilink_fun)
    
    pert_fit <- fit_pert_from_sample(transformed_samples)
    
    mean_pert <- (pert_fit$min + 4 * pert_fit$mode + pert_fit$max) / 6
    sd_pert <- sqrt(((pert_fit$max - pert_fit$min)^2) / 36)
    
    data.frame(
      # mean = mean_pert,
      min = pert_fit$min,
      mode = pert_fit$mode,
      max = pert_fit$max,
      shape = 4,
      # sd = sd_pert,
      dist = "pert",
      label = subdf$label[1],
      ilink = "identity",
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(results)
}


refit_to_pert2 <- function(df, size = 50000, seed = NULL,transFUN=function(x){x}) {
  if (!is.null(seed)) set.seed(seed)
  
  split_list <- split(df, df$label)
  
  results <- lapply(split_list, function(subdf) {
    # Determine distribution type (assuming all rows have same dist and ilink per label)
    dist_type <- subdf$dist[1]
    ilink_fun <- subdf$ilink[1]
    
    samples <- unlist(lapply(seq_len(nrow(subdf)), function(i) {
      if (dist_type == "norm") {
        mean_val <- subdf$mean[i]
        sd_val <- subdf$sd[i]
        rnorm(size, mean = mean_val, sd = sd_val)
      } else if (dist_type == "beta") {
        shape1 <- subdf$shape1[i]
        shape2 <- subdf$shape2[i]
        rbeta(size, shape1 = shape1, shape2 = shape2)
      } else {
        stop(paste("Unsupported distribution type:", dist_type))
      }
    }))
    
    samples<- transFUN(samples)
    # Apply inverse link function
    transformed_samples <- apply_ilink(samples, ilink_fun)
    
    # Fit pert distribution
    pert_fit <- fit_pert_from_sample(transformed_samples)
    
    data.frame(
      min = pert_fit$min,
      mode = pert_fit$mode,
      max = pert_fit$max,
      shape = 4,
      dist = "pert",
      label = subdf$label[1],
      ilink = "identity",
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(results)
}

