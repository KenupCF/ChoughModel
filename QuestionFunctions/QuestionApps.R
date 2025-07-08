proportionApp_multi3<-function(question_field="",suffix="",defaultDF,
                               plotMin=NULL,plotMax=NULL,
                               altTitles=letters[1:3],
                               altTitlesShort=NULL,
                               odds_sec_axis=FALSE,
                               queryAdd="",anchor=NULL,
                               count2multiplier=1,
                               PaletteNames,PaletteColors,
                               checkExpertise=TRUE,
                               propVizDivisor=1,
                               invertProp=FALSE,fullName="",
                               PDFpalette=NULL,
                               naturalMin=0,naturalMax=1,absoluteCount=100,
                               AnchorLabel="Previous estimate",
                               height=720,
                               plotHeight=400,
                               navbarFontSize=12,
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
  
  if(6==9){
  styleCSS<-paste0(".navbar .navbar-nav {font-size: ",navbarFontSize,"px} 
                    .navbar .navbar-brand {font-size: ",navbarFontSize+2,"px}")
  # Create UI
  ui<-fluidPage(
    tags$head(
      tags$style(type='text/css',styleCSS)),
    useShinyjs(),
    start_UI(checkExpertise=checkExpertise)
    # div(style="display:inline-block",
        # actionButton(inputId = "load",label = "Load question")),
    # uiOutput("FullQuestionUI"),
    # uiOutput("dummy")
    )
  options <- list(height = height)
  
  }
  
  question_elements<-questionAppPreamble(navbarFontSize = navbarFontSize,
                                         tabPanelFontSize=tabPanelFontSize,
                                         checkExpertise=checkExpertise,
                                         height = height)
  
  
  server <- function(input, output, session) {
    
    
    ILINK<-list(identity=function(x){x},
                inv.logit=function(x){inv.logit(x)},
                exp=function(x){exp(x)})
    #Create palette for PDF plot
    
    # PDFpalette<-viridis(length(altTitles),option="G")
    names(PDFpalette)<-altTitles
    
    
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
      
      if(is.null(anchor$ilink)) {anchor$ilink<-"identity"}
      
      if(class(anchor)=="data.frame"){
        
        anchor.list<-split(anchor,anchor$label)
        
      }
      
      if(class(anchor)=="list"){
        
        anchor.list<-anchor
        
      }
      
      # temp<-priorSampling(anchor.list)
      # cat(str(temp))
      
      anchorPDF<-priorPDF(anchor.list,precision=200)%>%
        dplyr::mutate(origin=par,x=values,y=PDF)%>%
        dplyr::left_join(anchor%>%
                           dplyr::select(label,ilink)%>%
                           dplyr::rename(origin=label))%>%
        dplyr::group_by(origin)%>%
        dplyr::mutate(x=ILINK[[unique(ilink)]](x))
      
      
        }else{
          anchorPDF<-data.frame(origin="",x=0,y=0)[0,]
          }
    
    observeEvent({input$load},
                 {
                  
                   # cat("Trying to get global variables")
                   email_user <- get("email_user", envir = globalEnv)
                   basedf     <- get("basedf",     envir = globalEnv)
                   validEmail <- get("validEmail", envir = globalEnv)
                  
                   # cat("\n basedf is \n")
                   # cat(str(basedf))
                   # cat("\n")
                   
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
                     self_reported <- FALSE
                   }
                   
                   if (length(self_reported) == 0) {
                     self_reported <- FALSE
                   }
                   
                   if (is.na(self_reported)) {
                     self_reported <- FALSE
                   }
                   
                   # cat("\n self_reported is \n")
                   # cat(str(self_reported))
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
                     tabs_second[[1]]<-NumbersPanel(i=i,plotHeight = plotHeight)
                     tabs_second[[2]]<-tabPanel("Distributions",
                                                plotOutput(paste("plotPDF_",i,sep=""),height = plotHeight)
                     )
                     # tabs_second[[3]]<-NumbersPanel(i=i,plotHeight = plotHeight,adj=TRUE)
                     # tabs_second[[3]]<-tabPanel("Probability",
                                # plotOutput(paste("SurvPlot_",i,sep=""),height = plotHeight)
                     
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
                       paste0("plotVizControl_",p),
                       paste0("adjPlotCheckbox_",p))
                   }))
                   
           lapply(1:number_of_tabPages,function(p){
               ModeName<-paste0("Mode_",p)
               MinName<-paste0("Min_",p)
               MaxName<-paste0("Max_",p)
               # CBName<-paste0("checkbox_",p)
               ConfidName<-paste0("Confid_",p)
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
                   # input[[CBName]]
                   input[[PVCName]]
                   input[[PVCAdjName]]
                   input[[PVCcheckboxName]]
                   input[[ConfidName]]
                   # input$self_report_expertise_init
                   # input[[CommentSubmitButtonName]]
                 },{

                 if(9==9){
                 # cat("Observer activated")
                 lapply(allInputs,shinyjs::disable)
                 
                 PERTcondition<-TRUE
                 
                 self_report_expertiseLoop<-input$self_report_expertise_init
                 if(length(self_report_expertiseLoop)==0){
                   self_report_expertiseLoop<-NA
                 }
                 
                 # PERTcondition0<-all(!sapply(input,is.na))
                 # PERTcondition<- PERTcondition0 &
                   # na2false(input[[MinName]] <= input[[ModeName]]) &
                   # na2false(input[[ModeName]] <= input[[MaxName]]) &
                   # na2false(input[[MinName]] != input[[MaxName]])  &
                   # na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
                   # na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
                 # cat(PERTcondition)
                 
                 if(PERTcondition){
                   # cat("solving PERT")
                   PERTsolved[[p]]<<-solvePERT(
                     Mode = input[[ModeName]],
                     Min = input[[MinName]],
                     Max = input[[MaxName]],
                     Shape = 4,
                     Conf = input[[ConfidName]]/100,
                     naturalMax = naturalMax,
                     naturalMin = naturalMin)
                           
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
                           naturalMax = naturalMax,
                           propVizDivisor=propVizDivisor,
                           fullRangePlot = input$useFullRange,
                           naturalMin = naturalMin)
                         
                         wafflePars<-WafflePlot_pars(basedf = basedf,
                                                     anchor = anchor,
                                                     input = input,
                                                     question_field=question_field,
                                                     invertProp=invertProp,
                                                     altTitles=altTitles,
                                                     naturalMin = naturalMin,
                                                     naturalMax = naturalMax,  
                                                     propVizDivisor = propVizDivisor,
                                                     absoluteCount = absoluteCount)
                        
                          
                       # Redraw plots for all alternatives 
                       # cat("Redraw plots for all alternatives ")
                         
                       lapply(1:length(altTitles),function(j){
                         # for(j in ){
                         
                         output[[paste0("plotPDF_",j)]]<<-renderPlot({
                           
                           renderPDFplot(pdfPars = pdfPars,
                                         PDFpalette = PDFpalette,
                                         alt_index = j,
                                         altTitles = altTitles,
                                         fullName = fullName)
                         })

                       })
                       
                       output[[paste0("plotVizProp_",p)]]<-renderPlot({
                         WafflePlot2(
                           waffle_pars=wafflePars,
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
                       
                       output[[paste0("plotVizPropAdj_",p)]]<-renderPlot({
                         WafflePlot2(
                           waffle_pars=wafflePars,
                           confAdj=TRUE,
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
                       } #end of observer
                 
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
    
    observeEvent({input$self_report_expertise_init},{
      
      # cat(str(input$self_report_expertise_init))
      
      if(tolower(as.character(input$self_report_expertise_init)%in%c("true","yes"))){
        shinyjs::enable("load")
      }else{
        
        # if(input$self_report_expertise_init==FALSE){
        # shinyjs::disable("load")
      }
      
      
    })
    
    wrap_observe_expertise(input=input,globalEnv=globalEnv,question_field=question_field)
    
    wrap_observe_copy_button(input, output, session,globalEnv = globalEnv,paste_choices=paste_choices)
    
    wrap_observe_record_button(input, output, session,globalEnv = globalEnv)
    
  }
  
  shinyApp(ui = question_elements$ui,server = server,options=question_elements$options)
}


rateApp_multi<-function(question_field="",suffix="",defaultDF,
                               plotMin=NULL,plotMax=NULL,
                               altTitles=letters[1:3],
                               altTitlesShort=NULL,
                               queryAdd="",anchor=NULL,
                               count2multiplier=1,
                               PaletteNames,PaletteColors,
                               propVizDivisor=1,invertProp=FALSE,fullName="",
                               PDFpalette=NULL,
                               naturalMin=0,naturalMax=Inf,
                               absoluteCount=100,
                               survPlot_ylab="Survival Probability",
                               survPlot_reverseY=FALSE,
                               survPlot_xlab="Days",
                               survPlot_x_factor=1/1,
                               survPlot_start_xmax=60,
                               AnchorLabel="Previous estimate",
                               height=720,
                               plotHeight=400,
                               navbarFontSize=12,
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
  
  if(6==9){
    
  styleCSS<-paste0(".navbar .navbar-nav {font-size: ",navbarFontSize,"px} 
                    .navbar .navbar-brand {font-size: ",navbarFontSize+2,"px}
                    .nav-tabs > li > a {font-size : ",tabPanelFontSize,"px !important;}"
)
  

  
  # Create UI
  ui<-fluidPage(
    tags$head(
      tags$style(type='text/css',styleCSS)),
    useShinyjs(),
    start_UI()
    # uiOutput("dummy")
  )
  
  options <- list(height = height)
  }
  
  question_elements<-questionAppPreamble(navbarFontSize = navbarFontSize,
                                         tabPanelFontSize=tabPanelFontSize,
                                         checkExpertise=checkExpertise,
                                         height = height)
  
  server <- function(input, output, session) {
    
    
    ILINK<-list(identity=function(x){x},
                inv.logit=function(x){inv.logit(x)},
                exp=function(x){exp(x)})
    #Create palette for PDF plot
    
    # PDFpalette<-viridis(length(altTitles),option="G")
    names(PDFpalette)<-altTitles
    
    
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
        mutate(x=ILINK[[anchor$ilink]](x))}else{
          anchorPDF<-data.frame(origin="",x=0,y=0)[0,]}
    
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
                       self_reported <- FALSE
                     }
                     
                     if (length(self_reported) == 0) {
                       self_reported <- FALSE
                     }
                     
                     if (is.na(self_reported)) {
                       self_reported <- FALSE
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
                       tabs_second[[1]]<-tabPanel("Distributions",
                                                  plotOutput(paste("plotPDF_",i,sep=""),height = plotHeight)
                       )
                       tabs_second[[2]]<-ProbabilityPanel(i = i,plotHeight = plotHeight,survPlot_start_xmax=survPlot_start_xmax)
                       
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
                         paste0("plotVizControl_",p))
                     }))
                     
                     lapply(1:number_of_tabPages,function(p){
                       ModeName<-paste0("Mode_",p)
                       MinName<-paste0("Min_",p)
                       MaxName<-paste0("Max_",p)
                       # CBName<-paste0("checkbox_",p)
                       ConfidName<-paste0("Confid_",p)
                       PVCName<-paste0("plotVizControl_",p)
                       SurvPlotXMaxName<-paste0("SurvPlotXMax_",p)
                       
                       
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
                           # input[[CBName]]
                           input[[PVCName]]
                           input[[ConfidName]]
                           input[[SurvPlotXMaxName]]
                           # input$self_report_expertise
                           # input[[CommentSubmitButtonName]]
                         },{
                           
                           if(9==9){
                             # cat("Observer activated")
                             lapply(allInputs,shinyjs::disable)
                             # PERTcondition0<-all(!sapply(input,is.na))
                             # PERTcondition<- PERTcondition0 &
                             #   na2false(input[[MinName]] <= input[[ModeName]]) &
                             #   na2false(input[[ModeName]] <= input[[MaxName]]) &
                             #   na2false(input[[MinName]] != input[[MaxName]])  &
                             #   na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
                             #   na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
                             # cat(PERTcondition)
                             PERTcondition<-TRUE
                             
                             self_report_expertiseLoop<-input$self_report_expertise_init
                             if(length(self_report_expertiseLoop)==0){
                               self_report_expertiseLoop<-NA
                             }
                             if(PERTcondition){
                               PERTsolved[[p]]<<-solvePERT(
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
                           if(PERTcondition){
                           # Generate values for common PDF plot
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
                           
                           survPars<-DecayPlotPars(basedf = get("basedf",envir = globalEnv),
                                                   # input=input,
                                         altTitles = altTitles,
                                         question_field = question_field)
                           
                           # Redraw plots for all alternatives 
                           lapply(1:length(altTitles),function(j){
                             # for(j in ){
                             
                             output[[paste0("plotPDF_",j)]]<<-renderPlot({
                               
                               renderPDFplot(pdfPars = pdfPars,
                                             PDFpalette = PDFpalette,
                                             alt_index = j,
                                             altTitles = altTitles,
                                             fullName = fullName)
                             })
                           
                           output[[paste0("SurvPlot_",j)]]<<-renderPlot({
                             DecayPlot(basedf = basedf,
                                       survPars = survPars,
                                       input=input,
                                       invertPlot = survPlot_reverseY,
                                       xlab=survPlot_xlab,
                                       x_factor=survPlot_x_factor,
                                       ylab = survPlot_ylab,
                                       manual_palette = PDFpalette,
                                       legend_title=navBarTitle,
                                       altTitles = altTitles,
                                       alt_index = j)}) 
                           })
                          }
                         } #end of observer
                         
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
    
    
    wrap_observe_expertise(input=input,globalEnv=globalEnv,question_field=question_field)
    
    wrap_observe_record_button(input, output, session,globalEnv = globalEnv)
    
    
  }
  
  shinyApp(ui = question_elements$ui,server = server,options=question_elements$options)
}


survApp_multi<-function(question_field="",suffix="",defaultDF,
                               plotMin=NULL,plotMax=NULL,
                               altTitles=letters[1:3],
                               altTitlesShort=NULL,
                               queryAdd="",anchor=NULL,
                               count2multiplier=1,
                               PaletteNames,PaletteColors,
                               propVizDivisor=1,
                               prob_time_frame_days,
                               invertProp=FALSE,fullName="",
                               PDFpalette=NULL,
                               naturalMin=0,naturalMax=1,absoluteCount=100,
                               AnchorLabel="Previous estimate",
                               survPlot_ylab="Survival Probability",
                               survPlot_xlab="Days",
                               survPlot_x_factor=1/1,
                               survPlot_reverseY=FALSE,
                               survPlot_start_xmax=60,
                               height=720,
                               plotHeight=400,
                               navbarFontSize=12,
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
                                         checkExpertise=checkExpertise,
                                         height = height)
  if(6==9){
  styleCSS<-paste0(".navbar .navbar-nav {font-size: ",navbarFontSize,"px} 
                    .navbar .navbar-brand {font-size: ",navbarFontSize+2,"px}")
  # Create UI
  ui<-fluidPage(
    tags$head(
      tags$style(type='text/css',styleCSS)),
    useShinyjs(),
    start_UI()
    # div(style="display:inline-block",
    # actionButton(inputId = "load",label = "Load question")),
    # uiOutput("FullQuestionUI"),
    # uiOutput("dummy")
  )
  
  options <- list(height = height)
  }
  server <- function(input, output, session) {
    
    
    ILINK<-list(identity=function(x){x},
                inv.logit=function(x){inv.logit(x)},
                exp=function(x){exp(x)})
    #Create palette for PDF plot
    
    # PDFpalette<-viridis(length(altTitles),option="G")
    names(PDFpalette)<-altTitles
    
    
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
        mutate(x=ILINK[[anchor$ilink]](x))}else{
          anchorPDF<-data.frame(origin="",x=0,y=0)[0,]}
    
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
                       self_reported <- FALSE
                     }
                     
                     if (length(self_reported) == 0) {
                       self_reported <- FALSE
                     }
                     
                     if (is.na(self_reported)) {
                       self_reported <- FALSE
                     }
                     
                     # cat("\n self_reported is \n")
                     # cat(str(self_reported))
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
                       tabs_second[[1]]<-NumbersPanel(i=i,plotHeight = plotHeight)
                       tabs_second[[2]]<-tabPanel("Distributions",
                                                  plotOutput(paste("plotPDF_",i,sep=""),height = plotHeight)
                       )
                       # tabs_second[[3]]<-NumbersPanel(i=i,plotHeight = plotHeight,adj=TRUE)
                       tabs_second[[3]]<-ProbabilityPanel(i = i,plotHeight = plotHeight,survPlot_start_xmax=survPlot_start_xmax)

                       
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
                         paste0("plotVizControl_",p))
                     }))
                     
                     lapply(1:number_of_tabPages,function(p){
                       ModeName<-paste0("Mode_",p)
                       MinName<-paste0("Min_",p)
                       MaxName<-paste0("Max_",p)
                       # CBName<-paste0("checkbox_",p)
                       ConfidName<-paste0("Confid_",p)
                       PVCName<-paste0("plotVizControl_",p)
                       PVCAdjName<-paste0("plotVizControlAdj_",p)
                       SurvPlotXMaxName<-paste0("SurvPlotXMax_",p)
                       
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
                           # input[[CBName]]
                           input[[PVCName]]
                           input[[PVCAdjName]]
                           input[[ConfidName]]
                           input[[SurvPlotXMaxName]]
                           # input$self_report_expertise_init
                           # input[[CommentSubmitButtonName]]
                         },{
                           
                           if(9==9){
                             # cat("Observer activated")
                             lapply(allInputs,shinyjs::disable)
                             
                             PERTcondition<-TRUE
                             
                             self_report_expertiseLoop<-input$self_report_expertise_init
                             if(length(self_report_expertiseLoop)==0){
                               self_report_expertiseLoop<-NA
                             }
                             
                             # PERTcondition0<-all(!sapply(input,is.na))
                             # PERTcondition<- PERTcondition0 &
                             # na2false(input[[MinName]] <= input[[ModeName]]) &
                             # na2false(input[[ModeName]] <= input[[MaxName]]) &
                             # na2false(input[[MinName]] != input[[MaxName]])  &
                             # na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
                             # na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
                             # cat(PERTcondition)
                             
                             if(PERTcondition){
                               PERTsolved[[p]]<<-solvePERT(
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
                           
                           wafflePars<-WafflePlot_pars(basedf = basedf,
                                                       anchor = anchor,
                                                       input = input,
                                                       question_field=question_field,
                                                       invertProp=invertProp,
                                                       altTitles=altTitles,
                                                       naturalMin = naturalMin,
                                                       naturalMax = naturalMax,  
                                                       propVizDivisor = propVizDivisor,
                                                       absoluteCount = absoluteCount)
                           
                           survPars<-DecayPlotPars(basedf = get("basedf",envir = globalEnv),
                                                   type="prob",time_frame_days=prob_time_frame_days,
                                                   altTitles = altTitles,
                                                   question_field = question_field)
                           # Redraw plots for all alternatives 
                           # cat("Redraw plots for all alternatives ")
                           
                           lapply(1:length(altTitles),function(j){
                             # for(j in ){
                             
                             output[[paste0("plotPDF_",j)]]<<-renderPlot({
                               
                               renderPDFplot(pdfPars = pdfPars,
                                             PDFpalette = PDFpalette,
                                             alt_index = j,
                                             altTitles = altTitles,
                                             fullName = fullName)
                             })
                             output[[paste0("SurvPlot_",j)]]<<-renderPlot({
                               DecayPlot(basedf = basedf,
                                         survPars = survPars,
                                         input=input,
                                         invertPlot = survPlot_reverseY,
                                         xlab=survPlot_xlab,
                                         x_factor=survPlot_x_factor,
                                         ylab = survPlot_ylab,
                                         manual_palette = PDFpalette,
                                         legend_title=navBarTitle,
                                         altTitles = altTitles,
                                         alt_index = j)}) 
                             
                           })
                           
                           output[[paste0("plotVizProp_",p)]]<-renderPlot({
                             WafflePlot2(
                               waffle_pars=wafflePars,
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
                           
                           output[[paste0("plotVizPropAdj_",p)]]<-renderPlot({
                             WafflePlot2(
                               waffle_pars=wafflePars,
                               confAdj=TRUE,
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
                         } #end of observer
                         
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
    
    observeEvent({input$self_report_expertise_init},{
      
      # cat(str(input$self_report_expertise_init))
      if(tolower(as.character(input$self_report_expertise_init)%in%c("true","yes"))){
        shinyjs::enable("load")
      }else{
        
        # if(input$self_report_expertise_init==FALSE){
        # shinyjs::disable("load")
      }
      
      
    })
    
    wrap_observe_expertise(input=input,globalEnv=globalEnv,question_field=question_field)
    
    wrap_observe_copy_button(input, output, session,globalEnv = globalEnv,paste_choices=paste_choices)
    
    wrap_observe_record_button(input, output, session,globalEnv = globalEnv)
    
  }
  
  shinyApp(ui = question_elements$ui,server = server,options=question_elements$options)
}

generalApp_multi<-function(question_field="",suffix="",defaultDF,
                           copyUI=FALSE,allQuestions=character(),
                           plotMin=NULL,plotMax=NULL,
                           altTitles=letters[1:3],
                           altTitlesShort=NULL,
                           allCopyCandidates=character(),
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
                           
                           anchor=data.frame(min=2600,max=3000,dist="unif",ilink="identity"),
                           AnchorLabel=character(),
                           
                           
                           height=720,
                           plotHeight=400,
                           navbarFontSize=12,
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
                       tabs_second[[1]]<-tabPanel("Distributions",
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
                         paste0("plotVizControl_",p))
                     }))
                     
                     lapply(1:number_of_tabPages,function(p){
                       ModeName<-paste0("Mode_",p)
                       MinName<-paste0("Min_",p)
                       MaxName<-paste0("Max_",p)
                       # CBName<-paste0("checkbox_",p)
                       ConfidName<-paste0("Confid_",p)
                       PVCName<-paste0("plotVizControl_",p)
                       
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
                           # input[[CBName]]
                           input[[PVCName]]
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
                             # PERTcondition0<-all(!sapply(input,is.na))
                             # PERTcondition<- PERTcondition0 &
                             # na2false(input[[MinName]] <= input[[ModeName]]) &
                             # na2false(input[[ModeName]] <= input[[MaxName]]) &
                             # na2false(input[[MinName]] != input[[MaxName]])  &
                             # na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
                             # na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
                             # cat(PERTcondition)
                             
                             if(PERTcondition){
                               PERTsolved[[p]]<<-solvePERT(
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



TableInputApp <- function(RowNames,
                          RowColors,
                          question_field,
                          RowLinks = NULL,
                          globalEnv=globalenv(),
                          allCopyCandidates=character(),
                          naturalMin=0,
                          naturalMax=100,
                          question_phrase = "Do you think any of these species are likely to be affected by the described strategy?") {
  require(dplyr)
  require(shinyjs)
  require(stringr)
  
  RowCodes<-seq_along(RowNames)
  
  ui <- fluidPage(
    useShinyjs(),
    div(
      # class = "sticky-top-header",
      # fluidRow(
      # column(2, 
      actionButton("load_ui", "Click here to load ecosystem question")
      # ),
      # column(4, selectInput(inputId = "options_to_paste_from", label = "Same answers as", choices = allCopyCandidates)),
      # column(2, actionButton(inputId = "paste_from_option", label = "Copy answers")),
      # column(2, shinyjs::disabled(actionButton("record", "Record answers")))
      # )
    ),
    uiOutput("loaded_ui"),
  )
  
  server <- function(input, output, session) {
    
    updateObservers<-TRUE
    
    #Create Observer List
    obsInputList<-list()
    reactInputList<-list()
    
    paste_choices = c("Please select an option" = "", allCopyCandidates)
    paste_choices <- paste_choices[paste_choices!=question_field]
    
    #Create list of solved values for 4 to 3 elicitation 
    PERTsolved<-list()
    
    allInputs<-unlist(lapply(seq_along(RowCodes),function(p){
      c(paste0("Mode_",p),
        paste0("Min_",p),
        paste0("Max_",p),
        paste0("Confid_",p),
        paste0("Comment_",p),
        paste0("toggle_",p))
    }))
    
    
    observeEvent(input$load_ui, {
      
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
        # shinyjs::disable("load_ui")
        shinyjs::hide("load_ui")
        
        # cat("\n get basedf\n")
        email_user <- get("email_user", envir = globalEnv)
        basedf     <- get("basedf",     envir = globalEnv)
        validEmail <- get("validEmail", envir = globalEnv)
        
        # cat("\n render UI\n")
        # updateObservers<-FALSE
        output$loaded_ui <- renderUI({
          
          basedf<-get(x = "basedf",envir = globalEnv)
          
          toggles <- basedf %>%
            dplyr::filter(question == question_field) %>%
            dplyr::pull(self_reported_expertise)
          
          names(toggles)<-basedf %>%
            dplyr::filter(question == question_field) %>%
            dplyr::pull(alternative)
          
          
          toggles<-toggles[RowCodes]
          
          toggles[is.na(toggles)]<-"Unable to answer"
          
          # cat("\n toggles is \n");cat(toggles);cat("\n")
          
          tagList(
            tags$head(
              tags$style(HTML("
            .selectize-dropdown {
            z-index: 1000 !important;
            }
            .selectize-input {
            z-index: 1000 !important;
            position: relative;
            }
            ")),
              tags$style(HTML(
                ".sticky-header {
                 position: -webkit-sticky;
                 position: sticky;
                 top: 0;
                 background-color: white;
                 z-index: 1000;
                 padding-top: 10px;
                 padding-bottom: 10px;
                 border-bottom: 1px solid #ddd;
               }
               .tall-input {
                 display: flex;
                 align-items: center;
                 justify-content: center;
                 height: 100%;
               }
               .tall-input input {
                 height: 34px;
                 margin: 0;
               }"
              ))
            ),
            div(class = "sticky-header",
                fluidRow(
                  column(4,
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
                  ),
                  column(2,
                         div(style = "margin-top: 25px;",
                             actionButton("record", "Record progress", class = "btn-primary"))
                  )
                ),
                fluidRow(
                  column(2, strong(question_phrase)),
                  column(2, strong("Minimum plausible probability of extinction (%)")),
                  column(2, strong("Maximum plausible probability of extinction (%)")),
                  column(2, strong("Most likely probability of extinction (%)")),
                  column(2, strong("Your confidence that the true value is captured by your estimates? (%)")),
                  column(2, strong("Further comments"))
                )),
            tagList(
              lapply(seq_along(RowCodes), function(i) {
                species <- RowNames[i]
                color <- RowColors[i]
                link <- RowLinks[i]
                if(link == "") { link <- NA }
                
                first_line <- sub("(.*)\\s\\(.*$", "\\1", species)
                second_line <- sub(".*(\\(.*\\))$", "\\1", species)
                
                
                species_label <- tagList(
                  div(style = paste0("color:", color, ";"),
                      if (!is.na(link)) {
                        tags$a(href = link, target = "_blank", style = paste0("text-decoration: underline; color:", color, ";"), strong(first_line))
                      } else {
                        strong(first_line)
                      },
                      br(),
                      em(second_line)
                  )
                )
                
                # species_label <- tippy_new(
                #   text = as.character(species_label),  # Convert tagList to HTML string
                #   tooltip = "<img src='/Data/Images/ThreatLegend2.png' alt='Threat Legend' width='200' />",
                #   fontsize = 14,
                #   allowHTML = TRUE
                # )                
                toggle_id <- paste0("toggle_", i)
                input_container_id <- paste0("inputs_ui_", i)
                
                # cat("\n toggle",i," is ",toggles[i])
                
                tagList(
                  fluidRow(
                    style = "border-bottom: 1px solid #f0f0f0; margin-bottom: 2px; padding: 4px 0;",
                    column(2,
                           species_label,
                           radioButtons(
                             inputId = toggle_id,
                             label = NULL,
                             choices = c("Yes", "No", "Unable to answer"),
                             selected = toggles[i],
                             inline = TRUE
                           )
                    ),
                    uiOutput(input_container_id)
                  )
                )
              })
            )
          )
        })
        # updateObservers<-TRUE
        
        ### Creating observers
        lapply(seq_along(RowCodes),function(i){
          
          ModeName<-paste0("Mode_",i)
          MinName<-paste0("Min_",i)
          MaxName<-paste0("Max_",i)
          ConfidName<-paste0("Confid_",i)
          ToggleName<-paste0("toggle_", i)
          CommentButtonName<-paste0("Comment_",i)
          input_container_id <- paste0("inputs_ui_", i)
          
          InputObserveName<-paste0("InputObs_",i)
          ToggleObserveName<-paste0("ToggleObs_",i)
          
          # Dynamically Creating Observers of Input Change
          if(is.null(obsInputList[[InputObserveName]])){
            
            obsInputList[[InputObserveName]]<<-observeEvent({
              input[[ModeName]]
              input[[MinName]]
              input[[MaxName]]
              input[[ConfidName]]
              input[[CommentButtonName]]
              input[[ToggleName]]
            },{
              
              if(updateObservers){
                # cat("Observer activated")
                lapply(allInputs,shinyjs::disable)
                
                
                self_report_expertiseLoop<-input[[ToggleName]]
                
                if(length(self_report_expertiseLoop)==0){
                  self_report_expertiseLoop<-NA
                }
                
                minLoop<-input[[MinName]]
                
                if(length(minLoop)==0){
                  minLoop<-NA
                }
                
                PERTcondition0<-TRUE
                # PERTcondition0<-all(!sapply(input,is.na))
                PERTcondition<- PERTcondition0 &
                  na2false(input[[MinName]] <= input[[ModeName]]) &
                  na2false(input[[ModeName]] <= input[[MaxName]]) &
                  na2false(input[[MinName]] != input[[MaxName]])  &
                  na2false(input[[ConfidName]]) 
                #   na2false(input[[MinName]] >= naturalMin*propVizDivisor) &
                #   na2false(input[[MaxName]] <= naturalMax*propVizDivisor)
                # cat(PERTcondition)
                
                # PERTcondition<-PERTcondition
                
                all_filled<- length(input[[MinName]])*length(input[[ModeName]])*length(input[[MaxName]])*
                  length(input[[ConfidName]])*length(input[[CommentButtonName]]) > 0
                
                if(all_filled){  
                  if(PERTcondition){
                    PERTsolved[[i]]<<-solvePERT(
                      Mode = input[[ModeName]],
                      Min = input[[MinName]],
                      Max = input[[MaxName]],
                      Shape = 4,
                      Conf = input[[ConfidName]]/100,
                      naturalMax = naturalMax,
                      naturalMin = naturalMin)
                    
                    newdf<-data.frame(name="",email=email_user,
                                      question=question_field,
                                      alternative=RowCodes[i],
                                      min=input[[MinName]],
                                      mode=input[[ModeName]],
                                      max=input[[MaxName]],
                                      trueMin=PERTsolved[[i]]$trueMin,
                                      trueMax=PERTsolved[[i]]$trueMax,
                                      confidence=input[[ConfidName]],
                                      comment = input[[CommentButtonName]],
                                      self_reported_expertise=input[[ToggleName]],
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
                                      alternative=RowCodes[i],
                                      min=input[[MinName]],
                                      mode=input[[ModeName]],
                                      max=input[[MaxName]],
                                      trueMin=NA,
                                      trueMax=NA,
                                      confidence=input[[ConfidName]],
                                      comment = input[[CommentButtonName]],
                                      self_reported_expertise=self_report_expertiseLoop,
                                      timestamp=now(),
                                      shape=0)
                    
                    basedf<-get(x = "basedf",envir = globalEnv)
                    
                    basedf<-rbind.fill(basedf,newdf)%>%
                      dplyr::arrange(desc(timestamp))%>%
                      dplyr::filter(!duplicated(
                        data.frame(question,alternative),fromLast = F))
                    
                    write.csv(basedf,file = "temp.csv")
                    assign(x = "basedf",value = basedf , globalEnv)
                    
                    
                  }
                }  
                lapply(allInputs,shinyjs::enable)
              }
              #########################
              ###UPDATE PDF PLOT ######
              #########################
              # cat(sum(basedf$mode))
            } #end of observer
            
            )
          }
          
          if(is.null(obsInputList[[ToggleObserveName]])){
            obsInputList[[ToggleObserveName]]<<-observeEvent({
              input[[ToggleName]]},{
                
                basedf     <- get("basedf",     envir = globalEnv)
                
                
                output[[input_container_id]] <- renderUI({
                  
                  display_values <- basedf %>%
                    dplyr::filter(question == question_field, alternative == RowCodes[i]) %>%
                    dplyr::select(min, max, mode, confidence,comment) %>%
                    unlist()
                  
                  show_inputs <- !is.null(input[[ToggleName]]) && input[[ToggleName]] == "Yes"
                  # 
                  # cat("\n display values is \n");cat(display_values);cat("\n")
                  
                  tags$div(
                    style = if (show_inputs) "" else "display:none;",
                    column(2, div(class = "tall-input", numericInput(inputId = paste0("Min_", i), label = NULL, value = display_values["min"]))),
                    column(2, div(class = "tall-input", numericInput(inputId = paste0("Max_", i), label = NULL, value = display_values["max"]))),
                    column(2, div(class = "tall-input", numericInput(inputId = paste0("Mode_", i), label = NULL, value = display_values["mode"]))),
                    column(2, div(class = "tall-input", numericInput(inputId = paste0("Confid_", i), label = NULL, value = display_values["confidence"]))),
                    column(2, div(class = "tall-input", textInput(inputId = paste0("Comment_", i), label = NULL, value = display_values["comment"])))
                  )
                })
                
                
              })
            
          }
        })
        
        
        # cat("\n observe values\n")
        
        
        #     
        #     
        #   }
        #      
        #   }
        #   )
        #   
      }
    })
    
    
    observeEvent(input$confirm_paste, {
      removeModal()  # Close the modal
      
      basedf <- get("basedf", envir = globalEnv)
      
      answers_to_paste <- basedf %>%
        filter(question == input$options_to_paste_from)
      
      for (i in seq_along(RowCodes)) {
        updateNumericInput(session, inputId = paste0("Min_", i), value = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(min))
        updateNumericInput(session, inputId = paste0("Mode_", i), value = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(mode))
        updateNumericInput(session, inputId = paste0("Max_", i), value = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(max))
        updateNumericInput(session, inputId = paste0("Confid_", i), value = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(confidence))
        updateTextInput(session, inputId = paste0("Comment_", i), value = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(comment))
        updateRadioButtons(session, inputId = paste0("toggle_", i), selected = answers_to_paste %>% filter(alternative == RowCodes[i]) %>% pull(self_reported_expertise))
      }
    })
    
    wrap_observe_copy_button(input, output, session,globalEnv = globalEnv,paste_choices=paste_choices)
    
    wrap_observe_record_button(input, output, session,globalEnv = globalEnv)
  }
  
  shinyApp(ui = ui, server = server,options = list(height=950))
}