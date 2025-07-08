oddsApp_multi<-function(question_field="",suffix="",defaultDF,
                           copyUI=FALSE,allQuestions=character(),
                           plotMin=NULL,plotMax=NULL,
                           altTitles=letters[1:3],
                           altTitlesShort=NULL,
                           allCopyCandidates=character(),
                           baseProb=100*(2/3),
                           odds_sec_axis=FALSE,
                           queryAdd="",
                           count2multiplier=1,
                           checkExpertise=FALSE,
                           PaletteNames,PaletteColors,
                           propVizDivisor=1,invertProp=FALSE,
                           fullName="",
                           PDFpalette=NULL,
                           naturalMin=0,naturalMax=Inf,
                           absoluteCount=100,
                           
                           anchor=data.frame(min=0.5,max=2,dist="unif",ilink="identity"),
                           AnchorLabel=character(),
                           
                           
                           height=720,
                           plotHeight=400,
                           navbarFontSize=12,
                           succLabel="Dead",
                           failLabel="Alive",
                           tabPanelFontSize=12,
                           SideBarWidth=4,
                           icon="Adult",
                           dataType=c("proportion","count"),
                           navBarTitle="Alternatives",
                           auxPlot = NULL,
                           globalEnv=globalenv(),
                           # nameEnv,
                           extraTabs = NULL
                           # ,solvePERTprecision=.03,
                           # solvePERTMax=1,solvePERTMin=0
){
  
  
  question_elements<-questionAppPreamble(navbarFontSize = navbarFontSize,
                                         tabPanelFontSize=tabPanelFontSize,
                                         # copyUI=copyUI,
                                         checkExpertise=checkExpertise,
                                         height = height)
  
  paste_choices = c("Please select an option" = "", allCopyCandidates)
  paste_choices <- paste_choices[paste_choices!=question_field]
  
  server <- function(input, output, session) {
    
    
    ILINK<-list(identity=function(x){x},
                inv.logit=function(x){inv.logit(x)},
                exp=function(x){exp(x)})
    #Create palette for PDF plot
    
    # PDFpalette<-viridis(length(altTitles),option="G")
    names(PDFpalette)<-altTitles
    
    # addResourcePath("images", "Data/Images")
    
    #Create Observer List
    obsInputList<-list()
    reactInputList<-list()
    
    #Create list of solved values for 4 to 3 elicitation 
    PERTsolved<-list()
    
    ##dummy plot
    gg<-ggplot(data = data.frame(x=1,y=1)[0,],aes(x=x,y=y))
    
    serverEnvir<-environment()
    ggEnvir<-where(name = "gg")
    
    plot_reactives<-reactiveValues(PDF=gg)
    
    #Create subtitle for panel 
    for(p in 1:length(altTitles)){  
      output[[paste0("Subtitle",p)]]<-renderText(HTML(paste0("<b>",altTitles[p],"</b>")))
    }
    
    # Declaring variables
    anchorToggle<-NULL
    anchorToggleEnv<-where("anchorToggle")
    PERTsolved<-list(trueMax=NA,trueMin=NA,RMSE=NA)
    PERTsolvedEnv<-where("PERTsolved")
    
    # anchor<-data.frame(mean=55.12,sd=9.048,dist="norm")
    if(!is.null(anchor)){
      anchorPDF<-priorPDF(list(anchor=anchor),precision=200)%>%
        mutate(origin=AnchorLabel,x=values,y=PDF)%>%
        mutate(x=ILINK[[anchor$ilink]](x))
      
      # cat("\n Anchor PDF df is \n")
      # cat(str(anchorPDF))
      
    }else{
      anchorPDF<-data.frame(origin="",x=0,y=0)[0,]
    }
    
    sharedInput <- numericInput("oddsBaseProb_shared", 
                            label = "Reference probability (%)",
                            value = baseProb,
                            min = 0, max = 100)
    
    observeEvent({input$load},
                 {
                   
                   # cat("Trying to get global variables")
                   email_user <- get("email_user", envir = globalEnv)
                   basedf     <- get("basedf",     envir = globalEnv)
                   validEmail <- get("validEmail", envir = globalEnv)
                   
                   
                   if(!validEmail){
                     showModal(modalDialog(
                       title = "Error",
                       paste("Cannot load question unless you
                             provide a valid email",sep=""),
                       size = "s",easyClose = T,fade = FALSE,footer = NULL))
                     
                   }else{
                     
                     # cat("email valid, check downward \n")
                     
                     
                     # cat(str(basedf))
                     
                     # Extract self reported
                     self_reported<-basedf%>%
                       filter(email==email_user,
                              question==question_field)%>%
                       pull(self_reported_expertise)%>%
                       unique()
                     
                     # cat(str(self_reported))
                     
                     if (is.null(self_reported)) {
                       self_reported <- character()
                     }
                     
                     if (length(self_reported) == 0) {
                       self_reported <- character()
                     }else{
                       
                       if (is.na(self_reported)) {
                         self_reported <- character()
                       }
                       
                     }
                     
                     
                     # cat("Imported self reported info")
                     
                     removeUI("#load")
                     number_of_tabPages <- length(altTitles)
                     
                     #make a list of all the arguments you want to pass to the navbarPage function
                     tabs<-list()
                     
                     if(all(altTitles==0)){navBarTitle<-""}
                     #first element will be the title, empty in your example
                     tabs[[1]]<-navBarTitle
                     # if(all(altTitles==0)){tabs[[1]]<-""}
                     
                     #add all the tabPanels to the list
                     
                     for (j in 2:(number_of_tabPages+1)){
                       
                       # J and I are related indexes, not different looping structures
                       i<-j-1
                       
                       SubTitle<-ifelse(is.null(altTitlesShort),
                                        "",paste0("<b>",altTitles[i],"</b>"))
                       
                       tabs_second<-list()
                       tabs_second[[1]]<-OddsPanel(i=i,plotHeight = plotHeight,baseValue=baseProb)
                       tabs_second[[2]]<-tabPanel("Distributions",
                                                  plotOutput(paste("plotPDF_",i,sep=""),height = plotHeight)
                       )
                       
                       if(!is.null(extraTabs)){
                         for(jj in 1:length(extraTabs)){
                           tabs_second[[2+jj]]<-extraTabs[[jj]]
                         }}
                       
                       tabs[[j]]=tabPanel(title = ifelse(altTitles[i]!=0,
                                                         ifelse(is.null(altTitlesShort),
                                                                altTitles[i],altTitlesShort[i]),
                                                         ""),
                                          HTML(SubTitle),
                                          sidebarLayout(
                                            sidebarPanel = 
                                              PERTSidebar(SideBarWidth = SideBarWidth,
                                                          question_field=question_field,
                                                          queryAdd=queryAdd,suffix=paste0("_",i),
                                                          defaultQuestion=defaultDF%>%
                                                            filter(alternative==altTitles[i])),
                                            mainPanel = mainPanel(
                                              do.call(tabsetPanel,tabs_second)
                                            )
                                          )
                       )
                       
                       # Set old comment into   
                       # commentLoop<-basedf%>%
                       #   dplyr::filter(email==email_user,
                       #                 question==question_field,
                       #                 alternative==altTitles[i])%>%
                       #   dplyr::pull(comment)
                       # reactInputList[[paste0("InputCommentReact_",i)]](commentLoop)
                     }
                     
                     
                     renderQuestionUI(outputEnv = environment(),
                                      copyUI=copyUI,paste_choices = paste_choices,
                                      tabs = tabs
                                      # ,self_reported_value = self_reported
                     )
                     
                     # if(6==9){
                     # output$FullQuestionUI<-renderUI({div(
                     #   do.call(navbarPage,tabs),
                     #   fluidRow(
                     #     column(width = 6,
                     #            checkboxInput(inputId = "self_report_expertise", value = self_reported,
                     #                          label = "I believe I have sufficient theoretical and/or practical expertise to provide an informed answer to this question")
                     #     ),
                     #     column(width = 6,
                     #            actionButton(inputId = "record", label = HTML("<b>Record provided answers</b>"))
                     #     )
                     #    )
                     # )})
                     # }
                     
                     # Creating a dynamic number of observers
                     
                     # For updating the results to memory and updating plots
                     allInputs<-unlist(lapply(1:number_of_tabPages,function(p){
                       c(paste0("Mode_",p),
                         paste0("Min_",p),
                         paste0("Max_",p),
                         paste0("Confid_",p),
                         paste0("oddsBaseProb_",p),
                         paste0("plotVizControl_",p))
                     }))
                     
                     lapply(1:number_of_tabPages,function(p){
                       ModeName<-paste0("Mode_",p)
                       MinName<-paste0("Min_",p)
                       MaxName<-paste0("Max_",p)
                       ConfidName<-paste0("Confid_",p)
                       
                       BaseOddsName<-paste0("oddsBaseProb_",p)

                       
                       PVCName<-paste0("plotVizControl_",p)
                       PVCAdjName<-paste0("plotVizControlAdj_",p)
                       PVCcheckboxName<-paste0("adjPlotCheckbox_",p,sep="")
                       
                       CommentTextName<-paste0("ExplanationText_",p)
                       CommentButtonName<-paste0("Explanation_",p)
                       CommentSubmitButtonName<-paste0("ExplanationSubmit_",p)
                       
                       InputObserveName<-paste0("InputObs_",p)
                       InputCommentObserveName<-paste0("InputCommentObs_",p)
                       InputCommentSubmitObserveName<-paste0("InputCommentSubmitObs_",p)
                       InputCommentReactListName<-paste0("InputCommentReact_",p)
                       
                       # Dynamically Creating Observers of Input Change
                       if(is.null(obsInputList[[InputObserveName]])){
                         obsInputList[[InputObserveName]]<<-observeEvent({
                           input[[ModeName]]
                           input[[MinName]]
                           input[[MaxName]]
                           input[[BaseOddsName]]
                           input[[PVCName]]
                           input[[PVCcheckboxName]]
                           input[[ConfidName]]
                           # input$self_report_expertise
                           # input[[CommentSubmitButtonName]]
                         },{
                           
                           if(9==9){
                             # cat("Observer activated")
                             lapply(allInputs,shinyjs::disable)
                             
                             self_report_expertiseLoop<-input$self_report_expertise_init
                             if(length(self_report_expertiseLoop)==0){
                               self_report_expertiseLoop<-NA
                             }
                             PERTcondition<-TRUE

                             if(PERTcondition){
                               # cat("\n input is \n")
                               # cat(str(input))
                               # cat("\n")
                               
                               PERTsolved[[p]]<<-solvePERT(
                                 link="log",
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
                               
                               basedf<-get(x = "basedf",envir = globalEnv)
                               
                               basedf<-rbind.fill(basedf,newdf)%>%
                                 dplyr::arrange(desc(timestamp))%>%
                                 dplyr::filter(!duplicated(
                                   data.frame(question,alternative),fromLast = F)
                                 )%>%
                                 dplyr::ungroup()
                               
                               assign(x = "basedf",value = basedf , globalEnv)
                               
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
                               
                               basedf<-get(x = "basedf",envir = globalEnv)
                               
                               basedf<-rbind.fill(basedf,newdf)%>%
                                 dplyr::arrange(desc(timestamp))%>%
                                 dplyr::filter(!duplicated(
                                   data.frame(question,alternative),fromLast = F))
                               
                               assign(x = "basedf",value = basedf , globalEnv)
                               
                               
                             }
                             lapply(allInputs,shinyjs::enable)
                           }
                           #########################
                           ###UPDATE PDF PLOT ######
                           #########################
                           # cat(sum(basedf$mode))
                           
                           
                           # Generate values for common PDF plot
                           # cat("Generate values for common PDF plot")
                           
                           pdfPars<-generatePDFplotInfo(
                             basedf=basedf,
                             input=input,
                             fullName=fullName,
                             altTitles=altTitles,
                             anchor=anchorPDF,
                             AnchorLabel=AnchorLabel,
                             previewAnchor=TRUE,
                             envir=ggEnvir,
                             question_field=question_field,
                             naturalMax = naturalMax*propVizDivisor,
                             propVizDivisor=propVizDivisor,
                             fullRangePlot = input$useFullRange,
                             naturalMin = naturalMin*propVizDivisor)
                           
                           oddsPars<-WafflePlotOdds_pars(basedf=basedf,
                                                                  input=input,
                                                                  question_field=question_field,
                                                                  altTitles=altTitles,
                                                                  absoluteCount=100,
                                                                  succLabel=succLabel,
                                                                  failLabel=failLabel,
                                                                  propVizDivisor=propVizDivisor,
                                                                  naturalMin=naturalMin*propVizDivisor,
                                                                  naturalMax=naturalMax*propVizDivisor,
                                                                  anchor=NULL,
                                                                  invertProp=FALSE)
                           # cat("\n oddsPars is \n")
                           # cat(str(oddsPars))
                           # cat("\n")
                           
                           # Redraw plots for all alternatives 
                           # cat("Redraw plots for all alternatives ")
                           lapply(1:length(altTitles),function(j){
                             # for(j in ){
                             
                             output[[paste0("plotPDF_",j)]]<<-renderPlot({
                               
                               renderPDFplot(pdfPars = pdfPars,
                                             PDFpalette = PDFpalette,
                                             odds_sec_axis=odds_sec_axis,
                                             alt_index = j,
                                             altTitles = altTitles,
                                             fullName = fullName)
                             })
                             
                             

                            
                            
                            
                           })
                           
                           output[[paste0("plotVizOdds_",p)]]<<-renderPlot({WafflePlotOdds(
                             waffle_pars=oddsPars,
                             confAdj=FALSE,
                             input = input,
                             altTitles=altTitles,
                             suffix=p,icon=icon,
                             propVizDivisor = propVizDivisor,
                             PaletteNames = PaletteNames,
                             PaletteColors = PaletteColors,
                             # anchor=NULL,
                             anchor=anchor,
                             invertProp=invertProp,
                             absoluteCount = absoluteCount)})
                           
                         }#end of observer
                         # } 
                         
                         )
                       }
                       
                       wrap_comment_observers(input=input,
                                              globalEnv = globalEnv,
                                              serverEnv = environment(),
                                              alt_index = p,
                                              question_field = question_field,
                                              defaultDF=defaultDF,
                                              altTitles = altTitles,
                                              InputCommentObserveName = InputCommentObserveName,
                                              CommentButtonName = CommentButtonName,
                                              InputCommentReactListName =InputCommentReactListName ,
                                              InputCommentSubmitObserveName = InputCommentSubmitObserveName,
                                              CommentSubmitButtonName = CommentSubmitButtonName,
                                              CommentTextName = CommentTextName)
                       if(6==9){
                         # Dynamically Creating Observers of Input Comment Open Dialog Change
                         if(is.null(obsInputList[[InputCommentObserveName]])){
                           obsInputList[[InputCommentObserveName]]<<-observeEvent({
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
                           obsInputList[[InputCommentSubmitObserveName]]<<-observeEvent({
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
                           
                           reactInputList[[InputCommentReactListName]]<<-reactiveVal(value = prev_comment)
                           
                           
                         }
                       }
                     })
                     
                     
                     
                     
                   }
                 })
    
    # observeEvent({input$self_report_expertise_init},{
    #   
    #   cat("\nself report detected\n")
    #   # cat(tolower(as.character(input$self_report_expertise_init)))
    #   if(tolower(as.character(input$self_report_expertise_init))%in%c("true","yes")){
    #     shinyjs::enable("load")
    #   }else{
    #     
    #     # if(input$self_report_expertise_init==FALSE){
    #     shinyjs::disable("load")
    #   }
    #   
    #   
    # })
    
    observeEvent(input$confirm_paste, {
      removeModal()  # Close the modal
      
      basedf <- get("basedf", envir = globalEnv)
      
      answers_to_paste <- basedf %>%
        filter(question == input$options_to_paste_from)
      
      # altTitles
      
      for (i in seq_along(altTitles)) {
        updateNumericInput(session, inputId = paste0("Min_", i), value = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(min))
        updateNumericInput(session, inputId = paste0("Mode_", i), value = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(mode))
        updateNumericInput(session, inputId = paste0("Max_", i), value = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(max))
        updateNumericInput(session, inputId = paste0("Confid_", i), value = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(confidence))
        # updateTextInput(session, inputId = paste0("Comment_", i), value = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(comment))
        # updateRadioButtons(session, inputId = paste0("toggle_", i), selected = answers_to_paste %>% filter(alternative == altTitles[i]) %>% pull(self_reported_expertise))
      }
    })
    
    
    wrap_observe_expertise(input=input,globalEnv=globalEnv,question_field=question_field)
    
    wrap_observe_copy_button(input, output, session,globalEnv = globalEnv,paste_choices=paste_choices)
    
    wrap_observe_record_button(input, output, session,globalEnv = globalEnv)
    
  }
  
  shinyApp(ui = question_elements$ui,server = server,options=question_elements$options)
}