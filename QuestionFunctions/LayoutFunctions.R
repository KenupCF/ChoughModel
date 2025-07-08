#Layout building blocks

PERTSidebar<-function(
    suffix="",question_field="",defaultQuestion,queryAdd="(0-1)",naturalMin=-Inf,naturalMax=Inf,
    SideBarWidth=4,inputWindowProp=6/12){
  
  if(!question_field%in%defaultQuestion$question){
    defaultQuestion<-rbind.fill(defaultQuestion,
                                data.frame(question=question_field,
                                           min=0,mode=0,max=0,shape=4,confidence=100,priority=0))
    
  }
  
  unit_html<-HTML(paste0("<span style='display: inline-block; vertical-align: middle; padding-top: 6px;'>",queryAdd,"</span>"))
  
  inputWindowLength<-round(12*inputWindowProp)
  unitLength<-12-inputWindowLength
  
  defaultQuestion<-defaultQuestion%>%
    dplyr::filter(question==question_field)
  
  # cat("\n defaultQuestion is\n")
  # cat(str(defaultQuestion))
  cat("\n")
  
  UI<-sidebarPanel(width = SideBarWidth,
                   tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),      
                   tags$div(
                     tags$span("Lowest plausible value", style = "margin-right: 10px; font-weight: bold;"),
                     fluidRow(
                       column(inputWindowLength,
                              numericInput(
                                inputId = paste("Min", suffix, sep=""),
                                label = NULL,
                                value = defaultQuestion$min,
                                min = naturalMin,
                                max = naturalMax
                              )
                       ),
                       column(unitLength,unit_html)
                     )
                   )
                   
    ,
                   # fluidRow(column(8,),column(4,"dummy")),
    tags$div(
      tags$span("Highest plausible value", style = "margin-right: 10px; font-weight: bold;"),
      fluidRow(
        column(inputWindowLength,
               numericInput(
                 inputId = paste("Max", suffix, sep=""),
                 label = NULL,
                 value = defaultQuestion$max,
                 min = naturalMin,
                 max = naturalMax
               )
        ),
        column(unitLength,unit_html)
        )
      ),
    tags$div(
      tags$span("Most likely value", style = "margin-right: 10px; font-weight: bold;"),
      fluidRow(
        column(inputWindowLength,
               numericInput(
                 inputId = paste("Mode", suffix, sep=""),
                 label = NULL,
                 value = defaultQuestion$mode,
                 min = naturalMin,
                 max = naturalMax
               )
        ),
        column(unitLength,unit_html)
      )
    ),                   
                   sliderInput(inputId = paste("Confid",suffix,sep=""),
                               label = "Confidence true value is within range (%)",
                               value=defaultQuestion$confidence,min = 50,max=100,step=1)
                   ,actionButton(paste0("Explanation",suffix,sep=""),
                                 HTML(paste0(strwrap(
                                   "Provide any comments or reasoning for your answer here (<i>Optional</i>)",width=25), collapse="</br>")))
                   
                   # ,textInput(inputId = paste("ExplainerText",suffix,sep=""),label = "Provide the reasoning that led to these values")
                   # ,actionButton(, ", style = "height: 60px; font-size: 16px;")
                   # ,
                   #     checkboxInput(inputId = paste("checkbox",sep=""),
                   #         label=HTML("<b>Record provided answers</b>"),
                   #         value = F
                   #               )
  )
  
  return(UI)
}


SlideApp <- function(imgs, caps, height = 600,max.width=400) {
    
    # Use addResourcePath to make the local folder accessible to Shiny
    # addResourcePath("localImages", "Images")
    
    # Update image paths to use the resource path
    html_images <- lapply(seq_along(imgs), function(i) {
      tagList(
        div(
          style = "text-align: center;",
          img(
            src = paste0("localImages/Traps/", basename(imgs[i])),
            style = "max-width:100%; height:auto; display:block; margin:auto;"
          ),
          p(HTML(caps[i]), style = "font-size: 16px; color: #333; margin-top: 5px;")
        )
      )
    })
    
    # Define UI
    ui <- fluidPage(
      div(style = paste0("max-width: ",max.width,"px; margin: auto;"),  # Limit the overall carousel width for better display
          wellPanel(
            style = "padding: 0; border: none;",  # Remove padding and borders for a cleaner look
            slickROutput("slickr", height = "auto")  # Set height to auto to adapt to content
          )
      )
    )
    
    # Define server
    server <- function(input, output, session) {
      
      output$slickr <- renderSlickR({
        # Pass the HTML images list to slickR, instead of treating as simple paths
        slickR(obj = html_images, slideId = 'captioned_slider') + settings(adaptiveHeight = TRUE)
      })
      
    }
    
    # Return shinyApp
    return(shinyApp(ui = ui, server = server, options = list(height = height)))
  }



ProbabilityPanel_old <- function(i, plotHeight, survPlot_start_xmax) {
  tabPanel("Probability",
           fluidPage(
             plotOutput(paste("SurvPlot_", i, sep = ""), height = plotHeight),
             fluidRow(
               column(2,
                      numericInput(paste("SurvPlotXMax_", i, sep = ""),
                                   label = "Plotted region",
                                   value = survPlot_start_xmax, min = 1
                      )
               ),
               column(10,
                      checkboxInput(paste0("PlotAllAlts_", i), label = "Plot other alternatives", value = FALSE)
               )
             )
           )
  )
}



ProbabilityPanel <- function(i, plotHeight, survPlot_start_xmax) {
  tabPanel("Probability",
           fluidPage(
             tags$head(
               tags$style(HTML("
          .inline-label .form-group {
            display: flex;
            align-items: center;
          }
          .inline-label .form-group label {
            margin-right: 10px; /* Adjust the spacing between the label and input here */
          }
        "))
             ),
             plotOutput(paste("SurvPlot_", i, sep = ""), height = plotHeight),
             fluidRow(
               column(4,
                      div(class = "inline-label",
                          numericInput(paste("SurvPlotXMax_", i, sep = ""),
                                       label = "Plotted region",
                                       value = survPlot_start_xmax, min = 1
                          )
                      )
               ),
               column(8,
                      checkboxInput(paste0("PlotAllAlts_", i), label = "Plot other alternatives", value = FALSE)
               )
             )
           )
  )
}



NumbersPanel<-function(i,plotHeight,adj=FALSE){
  
  if(!adj){
    
    tabTitle<-"Numbers"
    hvText<-"This tab displays the proportions you inputted as the expected number of results out of 100 trials"
    pvName<-paste0("plotVizControl_",i)
    plotName<-paste("plotVizProp_",i,sep="")
    checkboxName<-paste0("adjPlotCheckbox_",i,sep="")
    }else{
    tabTitle<-"Numbers (Adjusted)"
    hvText<- "This tab displays the proportions you inputted as the expected number of results out of 100 trials.
    <br>
    However, the lowest and highest most plausible values are adjusted according to your reported confidence."
    pvName<-paste0("plotVizControlAdj_",i)
    plotName<-paste("plotVizPropAdj_",i,sep="")
    checkboxName<-paste0("adjPlotCheckboxAdj_",i,sep="")
    
  }
  checkboxHT<-"By checking this box, the lowest and highest plausible values are adjusted for the level of confidence you provided."
  tabPanel(tippy_new(text = tabTitle,
                   # fontsize = 20,
                   tooltip = 
                     hvText),
         radioButtons(inputId = pvName,
                      label = "",
                      choices =
                        list("Lowest Plausible"=paste0("Min_",i),
                             "Most Likely"=paste0("Mode_",i),
                             "Highest Plausible"=paste0("Max_",i)),
                      selected = paste0("Mode_",i),inline = TRUE),
         plotOutput(plotName,height = plotHeight-50)
         ,checkboxInput(input=checkboxName,label = tippy_new("Adjust for confidence",checkboxHT),value = FALSE)
  )
}


OddsPanel<-function(i,plotHeight,adj=FALSE,baseValue=50){
  
  if(!adj){
    
    tabTitle<-"Numbers"
    hvText<-"This tab displays the proportions you inputted as the expected number of results out of 100 trials"
    pvName<-paste0("plotVizControl_",i)
    baseOdds<-paste0("oddsBaseProb_",i)
    plotName<-paste("plotVizOdds_",i,sep="")
    checkboxName<-paste0("adjPlotCheckbox_",i,sep="")
  }else{
    tabTitle<-"Numbers (Adjusted)"
    hvText<- "This tab displays the proportions you inputted as the expected number of results out of 100 trials.
    <br>
    However, the lowest and highest most plausible values are adjusted according to your reported confidence."
    pvName<-paste0("plotVizControlAdj_",i)
    plotName<-paste("plotVizOddsAdj_",i,sep="")
    checkboxName<-paste0("adjPlotCheckboxAdj_",i,sep="")
    
  }
  checkboxHT<-"By checking this box, the lowest and highest plausible values are adjusted for the level of confidence you provided."
  tabPanel(tippy_new(text = tabTitle,
                     # fontsize = 20,
                     tooltip = 
                       hvText),fluidRow(
           column(width = 6,
           radioButtons(inputId = pvName,
                        label = "",
                        choices =
                          list("Lowest Plausible"=paste0("Min_",i),
                               "Most Likely"=paste0("Mode_",i),
                               "Highest Plausible"=paste0("Max_",i)),
                        selected = paste0("Mode_",i),inline = TRUE)),column(width = 6,
                                                                            numericInput(inputId = baseOdds,
                                                                                         label = "Reference probability (%)",
                                                                                         value = baseValue,
                                                                                         min = 0,max = 100))),
           plotOutput(plotName,height = plotHeight-50)
           ,checkboxInput(input=checkboxName,label = tippy_new("Adjust for confidence",checkboxHT),value = FALSE)
  )
}