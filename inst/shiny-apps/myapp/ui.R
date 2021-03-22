#########################################
######### shiny dashboard UI ############
#########################################

dashboardPage(title="ModViz POP",

 ## Dashboard header
 dashboardHeader(title = tags$div(HTML("<b>ModViz</b> <font size=\"2\">POP</font>")),
                 tags$li(class = "dropdown",
                         tags$li(class = "dropdown", textOutput("loggedUser"), style = "padding-top: 15px; padding-bottom: 15px; padding-right: 20px; color: #ffffff;"),
                         tags$li(class= "dropdown", actionLink("login", "Logout")))
   ), #dashboardHeader

 ## Dashboard sidebar menu
 dashboardSidebar(
   includeCSS("styles.css"),

         sidebarMenu(id = "sidebarmenu",

            div(class="parambox",

            actionButton("modelSelect", "Model Selection", class="smButton", icon = icon("area-chart", class="fa fa-area-chart fa-fw", lib="font-awesome")),

            actionButton("fixedSelect", "Fixed Effects", class="smButton", icon = icon("user-circle-o", class="fa fa-user-circle-o fa-fw", lib="font-awesome")),

            actionButton("randomSelect", "Random Effects", class="smButton", icon = icon("users", class="fa fa-users fa-fw", lib="font-awesome")),

            actionButton("doseSelect", "Dosing Options", class="smButton", icon = icon("adjust", class="fa fa-adjust fa-rotate-90 fa-fw", lib="font-awesome")),

            actionButton("simSelect", "Simulation Options", class="smButton",  icon = icon("sliders", class="fa fa-sliders fa-fw", lib="font-awesome")),

            actionButton("advSelect", "Advanced Options", class="smButton", icon = icon("rocket", class="fa fa-rocket fa-fw", lib="font-awesome")) #,

            )) #sidebarMenu

   ), #dashboardSidebar

   ## Dashboard body
   dashboardBody(
    useShinyjs(),
        fluidRow(
          box(solidHeader = TRUE,
            tags$div(class="parambox",
              tags$div(class="header1_col",
                actionButton("info","Help",icon("info-circle")),
                actionButton("runsim","Run Simulation",icon=icon("play"))),
            tags$div(class="header2_col",
                conditionalPanel(condition="input.Body=='Plots'",
                       actionButton("customPlotOptions","Plot Options",
                                    icon = icon("area-chart", lib="font-awesome")),
                       actionButton("updatePlot","Plot Simulation",icon = icon("image", lib="font-awesome"))),
                conditionalPanel(condition="input.Body=='PK-NCA'",
                       actionButton("calculateNCA","Calculate NCA", icon = icon("calculator", lib="font-awesome"))),
                bsModal("modalwin", "Documentation", "info", size = "large",
                      HTML(readLines("www/About.html"))))))),
        fluidRow(
           tabItem(tabName = "models",
            bsModal("modelPop", "Model Options", "modelSelect", size = "large",
              tags$div(class="parambox",
                tags$div(class="left2_col",
                    selectizeInput("sources", "Select Source",
                                  c("PKPD ODE Library"="LIBODE","Project Library"="PROJ", "User Defined"="USER"),
                                  selected="LIBODE"),

                    conditionalPanel("input.sources == 'LIBODE' ",

                       selectizeInput("route", "Select Route",
                                      c("Select","IV Bolus"="IVB","IV Infusion"="IVI","1st Order Absorption"="ABS"),
                                      selected="Select"),

                       selectizeInput("cmpt", "Select Number of Compartments",
                                      c("Select", "One Compartment"="1CMT","Two Compartment"="2CMT"),
                                      selected="Select"),

                       selectizeInput("PDmodel", "Select PD Model",
                                      c("Select","None","Emax" = "EMAX", "Effect Compartment" = "EFF", "Indirect Response I"="IRM1", "Indirect Response II"="IRM2",
                                        "Indirect Response III"="IRM3", "Indirect Response IV"="IRM4"),
                                      selected="Select"),
                       br(), br(), br(), br(),
                       br(), br(), br(), br(),
                       br(),
                       br()),

                    conditionalPanel("input.sources == 'USER'",
                       fileInput('userModelFile',label=NULL, accept = c('.cpp')),
                       br(),
                       br()),

                    conditionalPanel("input.sources == 'PROJ'",
                       br(),
                       shinyFilesButton(id = "inFile",
                                        label = "Choose File",
                                        title = "Choose a model file",
                                        multiple = FALSE),
                       br(),
                       br()),
                    actionButton("loadModel","Load Model",
                                 icon = icon("shopping-cart", lib="font-awesome")),
                    uiOutput("modelLoadMessageO")
                    ),
                  tags$div(class="right2_col",
                      radioButtons("eventData", "Provide Custom Event Input or Individual Dataset as Input",
                                        c("Yes","No"), "No", inline=T),
                      br(),
                     conditionalPanel("input.eventData=='Yes'",
                     fileInput('inputEventData',label=NULL, accept = c('.csv')))
            )))), #tabItem models

            tabItem(tabName = "param",

               uiOutput("fixedPop"),

               bsModal("randomPopUp", "Random Parameters Selection", "randomSelect",
                       size = "large",
                       tags$div(class="parambox",
                       radioButtons("randomOptions", "Variability",
                                     c("None","Between Subject Only","Residual Only","Both"), "None", inline=T),
                       conditionalPanel("input.randomOptions == 'Between Subject Only' || input.randomOptions == 'Both'",
                       h5("Between Subject Variability - Covariance Martix"),
                       rHandsontableOutput("omegaTab"),
                       br()),
                       conditionalPanel("input.randomOptions == 'Residual Only' || input.randomOptions == 'Both'",
                       h5("Residual Variability - Covariance Martix"),
                       rHandsontableOutput("sigmaTab"),
                       br())
                       ))), #tabItem paramPop

            tabItem(tabName = "dosing",
              bsModal("dosePop", "Dosing Options", "doseSelect", size = "large",
                  tags$div(class="parambox",
                        tags$div(class="leftDose_col",
                                 textInput("doses", "Dose(s)", "100, 200"),


                                 conditionalPanel("input.route == 'IVI' && input.sources!='USER' && input.sources!='PROJ'",
                                                  textInput("Ltinf", "Duration of Infusion", 0.5)),

                                 conditionalPanel("input.sources == 'USER' || input.sources=='PROJ'",
                                                  textInput("Utinf", "Duration of Infusion", 0)),

                                 textInput("ii", "Select Dosing Interval", 24),

                                 textInput("nDoses", "Select Number of doses", 1),

                                 conditionalPanel("input.sources=='USER' || input.sources=='PROJ'",
                                                  textInput("cmt", "Dosing Compartment", 1))),

                        tags$div(class="rightDose_col",
                                 textInput("doseUnits", "Dose Units", "mg"),

                                 conditionalPanel("input.route == 'IVI' && input.sources!='USER' && input.sources!='PROJ'",
                                                  textInput("LtinfUnits", "Duration of Infusion Units", "hr")),

                                 conditionalPanel("input.sources == 'USER' || input.sources=='PROJ'",
                                                  textInput("UtinfUnits", "Duration of Infusion Units", "hr")),

                                 textInput("iiUnits", "Dosing Interval Units", "hr")))

           )), #tabItem dosing

          tabItem(tabName = "simOptions",
            uiOutput("simOptions")
           ), #tabItem simOptions

          tabItem(tabName = "advOptions",
            bsModal("advPop", "Advanced Options", "advSelect", size = "large",
                tags$div(class="parambox",
                  tags$div(class="leftDose_col",
                      selectizeInput("advOptions"," Select Option",
                                     c("None","Overlay External Data","Set As Reference","Parameter Sweep"),"None"),

                      conditionalPanel("input.advOptions == 'Overlay External Data'",
                                       fileInput('inputData',label=NULL, accept = c('.csv'))),

                      conditionalPanel("input.advOptions == 'Parameter Sweep'",
                                       uiOutput("parSweep")),

                      conditionalPanel("input.advOptions == 'Parameter Sweep'",
                                       textInput("parSweepVal", "Select Values", "1, 5")),

                      br(), br(), br(), br(),
                      br(), br(), br(), br())))),

          uiOutput("plotOptions"),

          tabBox(id="Body",
            tabPanel("Simulated Data",
              column(width=12,
                column(width=10,
                  br(),
                 htmlOutput("parCheckE"),
		              conditionalPanel("input.runsim",
                    DT::dataTableOutput("simData")%>%
                      withSpinner(type=getOption("spinner.type", 8),
                                  color="#136c68"))))),
            tabPanel("Plots",
             column(width=12,
               column(width=10,
                htmlOutput("plotOptionsCheckE"),
                conditionalPanel("input.updatePlot",
                                 plotOutput("outPlot") %>%
                                 withSpinner(type=getOption("spinner.type", 8),
                                             color="#136c68")))
               )), #end tabPanel-Plots
            tabPanel("PK-NCA",
              column(width=12,
                column(width=10,
                      htmlOutput("derivedPKOutput") %>%
                          withSpinner(type=getOption("spinner.type", 8),
                                      color="#136c68")
                      ))),
            tabPanel("Model",
                column(width=12,
                  column(width=10,
                     tags$blockquote(strong("Initial Conditions"), style="border: 0px invisible;
                                     border-left: 0px solid #eee;
                                     padding: 4px;
                                     font-size: 16px;
                                     background-color: #43b3a5;"),
                     tableOutput("initOutputTable"),
                     tags$blockquote(strong("Differential Equations"), style="border: 0px invisible;
                                     border-left: 0px solid #eee;
                                     padding: 4px;
                                     font-size: 16px;
                                     background-color: #43b3a5;"),
                     htmlOutput("odePrintOut"),
                     tags$blockquote(strong("Model Code"), style="border: 0px invisible;
                                     border-left: 0px solid #eee;
                                     padding: 4px;
                                     font-size: 16px;
                                     background-color: #43b3a5;"),
                    uiOutput("modelCode")))),
            tabPanel("Inputs",
                column(width=12,
                  column(width=10,
                      tags$blockquote(strong("Dosing Scheme"), style="border: 0px invisible;
                                                              border-left: 0px solid #eee;
                                                              padding: 4px;
                                                              font-size: 16px;
                                                              background-color: #43b3a5;"),
                      tableOutput("dosingOutputTable"),
                      br(),
                      tags$blockquote(strong("Fixed Effects Parameters"), style="border: 0px invisible;
                                                                                 border-left: 0px solid #eee;
                                                                                 padding: 4px;
                                                                                 font-size: 16px;
                                                                                 background-color: #43b3a5;"),
                      tableOutput("paramOutputTable"),
                      br(),
                      conditionalPanel("input.randomOptions!='None'",
                      tags$blockquote(strong("Random Effects Parameters"), style="border: 0px invisible;
                                                                                 border-left: 0px solid #eee;
                                                                                 padding: 4px;
                                                                                 font-size: 16px;
                                                                                 background-color: #43b3a5;"),
                      h5(strong("Between Subject Variability (Covariance Matrix)")),
                      tableOutput("omgOutputTable"),
                      br(),
                      conditionalPanel("input.advOptions=='Set As Reference'",
                      h5(strong("Between Subject Variability (Covariance Matrix) - Reference")),
                      tableOutput("omgRefOutputTable"),
                      br()),
                      h5(strong("Residual Variability (Covariance Matrix)")),
                      tableOutput("sigOutputTable"),
                      br(),
                      conditionalPanel("input.advOptions=='Set As Reference'",
                      h5(strong("Residual Variability (Covariance Matrix) - Reference")),
                      tableOutput("sigRefOutputTable"),
                      br())),
                      tags$blockquote(strong("Simulation Options"), style="border: 0px invisible;
                                                                      border-left: 0px solid #eee;
                                                                      padding: 4px;
                                                                      font-size: 16px;
                                                                      background-color: #43b3a5;"),
                      tableOutput("simOptionsOutputTable"),
                      br()))),
            tabPanel("Downloads",
               column(width=12,
                  column(width=8,
                     br(),
                     tags$div(downloadButton("dwnSimData","Simulated Data", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnPlot","Simlation Plot", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnNCARaw","NCA Individual", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnNCASumm","NCA Summary", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnModel","Model", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnReport","Report", class="downloadButton")),
                     br(),
                     tags$div(downloadButton("dwnSession","Session", class="downloadButton")))))

            ) #tabBox
          ) #fluidRow for Body
      )  #dashboardBody
) #end ui
