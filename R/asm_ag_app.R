#' Shiny app for use with MCOTEA Analysis Sychronization Matrix (ASM)
#'
#' \code{asm_ag_app} provides a Shiny application for importing and viewing data
#'   from the MCOTEA ASM datFormTable.xlsx output.
#'
#' @return A Shiny Application.
#'
#' @seealso 
#'
#' @examples
#' # asm_ag_app(
#' #   filedir = "directory containing datFormProgram.xlsx"
#' #   launch.browser = TRUE
#' # )
#'
#' @export
# Run the application 
asm_ag_app <- function(filedir, launch.browser = TRUE){

# Load Packages
#library(shiny)
#library(readxl)
#library(data.table)
#library(ggplot2)
#library(magrittr)
#library(lubridate)
#library(plotly)
#library(htmlwidgets)
#library(DT)
#library(shinyjs)
#library(RODBC)
#library(shinyFiles)
#library(shinyBS)
#library(timevis)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Set ggplot2 theme
theme_set(theme_bw())


# Define UI
ui <- fluidPage(
  
  # Coloring the timevis bars based on EventType
  tags$style(
    ".AA { background: #ff0000; }
       .CTT { background: #ff6666; }
       .CVPA { background: #ff3333; }
       .DT { background: #4169e1; }
       .DTE { background: #4169e1; }
       .EOA { background: #ffe866; }
       .FOTE { background: #ccad00; }
       .IOTE { background: #ffd700; }
       .LF { background: #ffa500; }
       .MOTE { background: #008000; }
       .OA { background: #ffe033; }"
  ),
  
  tags$head(
    tags$style(HTML("

    #loadmessage {
      /*position: fixed;
      top: 0px;
      left: 0px;*/
      width: 100%;
      padding: 5px 0px 5px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 14pt;
      color: #000000;
      background-color: #CCFF66;
      z-index: 105;
    }

    #message {
      font-size: 14pt;
      color: red;
      font-weight: bold;
    }

    table.dataTable thead th{
        font-size: 8pt;
        /*background-color: #fff700;*/
    }

    table.dataTable {
        line-height: .75;
    }
    
    table.dataTable tbody tr{
        font-size: 8pt;
    }

    .form-control-feedback {
        line-height: 22px;
    }

    table.dataTable tbody tr.even{
        font-size: 8pt;
        background-color: #f5f5f5;
    }
    
    table.dataTable tbody tr:hover {
        background-color: #fff700;
    }
    
    .navbar{
        margin-bottom: 5px;
    }
    
    .selectize-control.multi .selectize-input.has-items {
        padding-left: 9px;
        padding-right: 9px;
        padding-top: 2px;
        padding-bottom: 2px;
    }
    
    .well{
      padding-left: 15px;
      padding-right: 15px;
      padding-top: 5px;
      padding-bottom: 5px;
    }
    
    .form-group{
        margin-bottom: 2px;
        line-height: 1;
    }
    
    table.dataTable thead input {
        font-size: 8pt;
        width: 70px !important;
    }
    
    label {
        font-size: 8pt;
        line-height: 1;
        margin-bottom: 0px;
        font-weight: 500;
    }
    
    .form-control{
        font-size: 8pt;
        line-height: 10px;
        height: 20px;
    }
    
    body{
        font-size: 10pt;
    }

    .form-group {
        margin-bottom: 5px;
    }
    
    .selectize-control {
        margin-bottom: 5px;
    }
    
    .btn {
       font-size: 8pt;
       height: 20px;
       padding: 0px 10px;
       margin-bottom: 5px;
    }
    
    .progress{
      height: 15px;
      margin-bottom: 0px;
    }

    .progress-bar{
      line-height: 15px;
      margin-bottom: 0px;
      font-size: 8pt;
    }

    .col-sm-4 {
        width: 20%;
    }

    .selectize-dropdown, .selectize-input, .selectize-input input {
    color: #333333;
    font-family: inherit;
    font-size: 10px;
    line-height: 6px;
    min-height: 10px;
    -webkit-font-smoothing: inherit;
    }
    
    .selectize-dropdown-content{
      line-height: 10px;
    }
                        
    .shiny-output-error-validation{
        font-size: 12pt;
        color: red;
        font-weight: bold;
    }
    
    hr {
        border: 1px solid #000000;
    }

 /*#########################################
    display {
        width: 50%;
    }
    
 .container-fluid {
        background-color: ghostwhite;
 }
###########################################*/  
    "))),
  
  
  useShinyjs(),
  navbarPage(title = "MCOTEA", windowTitle = "MCOTEA ASM Outlook App",
             
             
             #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             tabPanel(title = "Program Outlook",
                      div(id = "Sidebar",
                          sidebarPanel(
                            
                            #fileInput("dataFile", "Choose Excel File Containing 'datFormProgram' table data from the ASM", multiple = FALSE, accept = c(".xls", "xlsx")),
                            HTML("<p style = 'font-size: 8pt; margin: 0 0 0px;'>Choose Excel File Containing 'datFormProgram' table data from the ASM</p>"),
                            shinyFilesButton("dataFile", "File select", "Please select an Excel .xlsx file", multiple = FALSE),
                            
                            numericInput("retrospectiveLook", "Retrospective Days", value = 10, min = 0),
                            numericInput("outlook", "Outlook Days", value = 180, min = 0),
                            textInput("vertical", "Vertical Lines at Given Days", value = "0, 30, 60, 90"),
                            bsTooltip(id = "vertical",
                                      title = "Input numeric values separated by commas to add vertical lines to the plot.", trigger = "hover"),
                            
                            selectInput("step", "Select Steps to Display", choices = "No Steps to display.", multiple = TRUE),
                            selectInput("event", "Select Event to Display", choices = "No Events to display.", multiple = TRUE),
                            selectInput("eventtype", "Select Event Types to Display", choices = "No Event Types to display.", multiple = TRUE),
                            
                            selectInput("product", "Select Products to Display", choices = "No Products to Display", multiple = TRUE),
                            
                            selectInput("division", label = "Select Divisions to Display", choices = "No divisions to display.", multiple = TRUE),
                            selectInput("program", label = "Select Programs to Display", choices = "No programs to display.", multiple = TRUE),
                            selectInput("OTPO", label = "Select OTPOs to Display", choices = "No OTPOs to display.", multiple = TRUE),
                            selectInput("ORSA", label = "Select ORSAs to Display", choices = "No ORSAs to display.", multiple = TRUE),
                            selectInput("MS", label = "Select MSs to Display", choices = "No MSs to display.", multiple = TRUE),
                            selectInput("TM", label = "Select TMs to Display", choices = "No TMs to display.", multiple = TRUE),
                            selectInput("DM", label = "Select DMs to Display", choices = "No DMs to display.", multiple = TRUE),
                            selectInput("CA", label = "Select CAs to Display", choices = "No CAs to display.", multiple = TRUE),
                            
                            selectInput("colorby", "Color Bars By:",
                                        choices = c(
                                          "Division","Event","EventType","Step","Product",
                                          "OTPO","ORSA","MS","TM","DM","CA"),
                                        selected = "EventType"),
                            
                            numericInput("height", "Plot Height", value = 650, min = 0),
                            numericInput("width", "Plot Width", value = 1200, min = 0)
                          )
                      ),
                      
                      mainPanel(
                        verbatimTextOutput("path"),
                        tableOutput("tbl"),
                        actionButton("toggleSidebar", "Toggle Sidebar"),
                        
                        textOutput("message"),
                        
                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                         tags$div("Loading... Please wait.",id="loadmessage")),
                        
                        textOutput("message1"),
                        br(),
                        textOutput("message1a"),
                        textOutput("message1b"),
                        textOutput("message1c")
                        ,
                        br(),
                        textOutput("message2"),
                        br(),
                        textOutput("message3"),
                        br(),
                        fluidRow(
                          plotlyOutput("plotly", width = "auto", height = "auto")
                        ),
                        br(),
                        hr(),
                        fluidRow(
                          DTOutput("my_table")
                        )
                      )
             ),
             #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
             
             
             #======================================================================================               
             tabPanel(title = "Data Issues",
                      p("The following table shows rows from the input file that had issues regarding dates
                   (blank dates, or invalid dates such as Start Date > Stop Date).
                   The issue is reported in the 'Issue' column."),
                      
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$div("Loading... Please wait.",id="loadmessage")),
                      
                      DTOutput("data_issues")
             ),
             #======================================================================================
             
             
             #--------------------------------------------------------------------------------------
             tabPanel(title = "AG Outlook",
                      
                      div(id = "Sidebar_tv",
                          sidebarPanel(
                            
                            numericInput("retrospectiveLook_tv", "Retrospective Days", value = 10, min = 0),
                            numericInput("outlook_tv", "Outlook Days", value = 180, min = 0),
                            
                            
                            selectInput("step_tv", "Select Steps to Display", choices = "No Steps to display.", multiple = TRUE),
                            selectInput("event_tv", "Select Event to Display", choices = "No Events to display.", multiple = TRUE),
                            selectInput("eventtype_tv", "Select Event Types to Display", choices = "No Event Types to display.", multiple = TRUE),
                            
                            selectInput("product_tv", "Select Products to Display", choices = "No Products to display.", multiple = TRUE),
                            
                            selectInput("primary_tv", "Select Role to Display", choices = c("Primary", "Secondary", "Both"), multiple = FALSE),
                            selectInput("ag_div_tv", label = "Select Divisions to Display", choices = c("ORSA", "MS", "TM", "DM", "Cyber", "LF"), multiple = FALSE),
                            
                            selectInput("personnel_tv", "Select Personnel to Display", choices = "No personnel to display.", multiple = TRUE),
                            
                            hr(),
                            
                            textInput("vertical_tv", "Vertical Lines at Given Days", value = "0, 30, 60, 90"),
                            bsTooltip(id = "vertical_tv",
                                      title = "Input numeric values separated by commas to add vertical lines to the plot.", trigger = "hover"),                              
                            
                            selectInput("colorby_tv", "Color Bars By:",
                                        choices = c(
                                          "Division","Event","EventType","Step","Product",
                                          "OTPO","ORSA","MS","TM","DM","CA"),
                                        selected = "EventType"),
                            
                            numericInput("height_tv", "Bottom Plot Height", value = 650, min = 0),
                            numericInput("width_tv", "Bottom Plot Width", value = 1200, min = 0)
                            
                            
                          ) # End of sidebarPanel
                      ),
                      
                      mainPanel(
                        fluidRow(
                          timevisOutput("tv", width = "100%", height = "auto")
                        ),
                        br(),
                        hr(),
                        br(),
                        fluidRow(
                          plotlyOutput("plotly2", width = "auto", height = "auto")
                        ),
                        br(),
                        br(),
                        br(),
                        hr(),
                        fluidRow(
                          DTOutput("my_table_tv")
                        ),
                        br(),
                        br(),
                        br(),
                        hr(),
                        fluidRow(
                          verbatimTextOutput("sked_output")
                        ),
                        br(),
                        br(),
                        br(),
                        hr(),
                        fluidRow(
                          verbatimTextOutput("dt_output")
                        )
                      )
                      
             )
             #--------------------------------------------------------------------------------------
             
             
             
  ) # End of navbarPage
) #End of fluidPage



##################################################################################################
# Define server logic
server <- function(input, output, session) {
  
  #######################################
  session$onSessionEnded(function() {   #
    stopApp()                           #
  })                                    #
  #######################################    
  
  
  ########################################
  observeEvent(input$toggleSidebar, {    #
    shinyjs::toggle(id = "Sidebar")    #
  })                                     #
  ########################################
  
  
  dataFile <- reactive({
    #roots <- c(wd = file.path("C:/Users", Sys.getenv("USERNAME"), "Documents/ASM, PSWG, CUB, RRPC/ASM"))
    #roots <- c(wd = file.path("C:/Users/jacob.j.warren/OneDrive - United States Marine Corps/Desktop/jacob.j.warren_Outputs"))
    roots <- c(wd = file.path(filedir))
    
    shinyFileChoose(input, "dataFile", roots = roots, session = session, filetypes=c('', 'xlsx'))
    req(input$dataFile)
    if (is.null(input$dataFile))
      return(NULL)
    return(parseFilePaths(roots = roots, input$dataFile)$datapath)
  })
  
  # For debugging file reading and whatnot
  ###########################################################################################
  #    output$path <- renderText({                                                           #
  #      inFile <- dataFile() #d_dataFile2()                                                 #
  #      inFile                                                                              #
  #    })                                                                                    #
  #    #d_dataFile2 <- debounce(dataFile, 1000)                                              #
  #                                                                                          #
  #    output$tbl <- renderTable({                                                           #
  #      inFile <- dataFile() #d_dataFile2()                                                 #
  #                                                                                          #
  #        connection <- odbcConnectAccess2007(                                              #
  #          inFile, uid = "jacob.j.warren", pwd = "AdminOTAD")                              #
  #                                                                                          #
  #        dt <- sqlFetch(                                                                   #
  #          connection, "qrytblConsolidatedList", colnames = FALSE, rownames = FALSE,       #
  #            na.strings = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"),       #
  #            as.is = TRUE, stringsAsFactors = FALSE)                                       #
  #                                                                                          #
  #        odbcCloseAll()                                                                    #
  #        setDT(dt)                                                                         #
  #        dt <- dt[Current == "Y"]                                                          #
  #                                                                                          #
  #          dt[, StartDate := format(as.Date(StartDate), format = "%Y-%m-%d")]              #
  #            dt[, StopDate := format(as.Date(StopDate), format = "%Y-%m-%d")]              #
  #        dt                                                                                #
  #    })                                                                                    #
  ###########################################################################################
  
  #==================================================================#
  #==================================================================#
  # debounce input to delay reactivity - so the page doesn't try     #
  # to update as ifteb while inputs are being changed                #
  #
  #    dataFile <- reactive({                                             #
  #        input$dataFile                                                 #
  #    })                                                                 #
  #    d_dataFile <- debounce(dataFile, 5000)                             #
  #
  rlook <- reactive({                                                #
    input$retrospectiveLook                                        #
  })                                                                 #
  d_rlook <- debounce(rlook, 2000)                                   #
  #
  vert <- reactive({                                                 #
    input$vertical                                                 #
  })                                                                 #
  d_vert <- debounce(vert, 2000)                                     #
  #
  outlook <- reactive({                                              #
    input$outlook                                                  #
  })                                                                 #
  d_outlook <- debounce(outlook, 2000)                               #
  #
  step <- reactive({                                                 #
    input$step                                                     #
  })                                                                 #
  d_step <- debounce(step, 2000)                                     #
  #
  event <- reactive({                                                #
    input$event                                                    #
  })                                                                 #
  d_event <- debounce(event, 2000)                                   #
  #
  eventtype <- reactive({                                            #
    input$eventtype                                                #
  })                                                                 #
  d_eventtype <- debounce(eventtype, 2000)                           #
  #
  product <- reactive({                                              #
    input$product                                                  #
  })                                                                 #
  d_product <- debounce(product, 2000)                               #
  #
  division <- reactive({                                             #
    input$division                                                 #
  })                                                                 #
  d_division <- debounce(division, 2000)                             #
  #
  program <- reactive({                                              #
    input$program                                                  #
  })                                                                 #
  d_program <- debounce(program, 2000)                               #
  #
  otpo <- reactive({                                                 #
    input$OTPO                                                     #
  })                                                                 #
  d_otpo <- debounce(otpo, 2000)                                     #
  #
  orsa <- reactive({                                                 #
    input$ORSA                                                     #
  })                                                                 #
  d_orsa <- debounce(orsa, 2000)                                     #
  #
  ms <- reactive({                                                   #
    input$MS                                                       #
  })                                                                 #
  d_ms <- debounce(ms, 2000)                                         #
  #
  tm <- reactive({                                                   #
    input$TM                                                       #
  })                                                                 #
  d_tm <- debounce(tm, 2000)                                         #
  #
  dm <- reactive({                                                   #
    input$DM                                                       #
  })                                                                 #
  d_dm <- debounce(dm, 2000)                                         #
  #
  ca <- reactive({                                                   #
    input$CA                                                       #
  })                                                                 #
  d_ca <- debounce(ca, 2000)                                         #
  #
  height <- reactive({                                               #
    input$height                                                   #
  })                                                                 #
  d_height <- debounce(height, 2000)                                 #
  #
  width <- reactive({                                                #
    input$width                                                    #
  })                                                                 #
  d_width <- debounce(width, 2000)                                   #
  
  #*******************************************************************
  # AG timevis *******************************************************
  
  rlook_tv <- reactive({                                             #
    input$retrospectiveLook_tv                                       #
  })                                                                 #
  d_rlook_tv <- debounce(rlook_tv, 2000)                             #
  
  outlook_tv <- reactive({                                           #
    input$outlook_tv                                                 #
  })                                                                 #
  d_outlook_tv <- debounce(outlook_tv, 2000)                         #
  
  vert_tv <- reactive({                                              #
    input$vertical_tv                                              #
  })                                                                 #
  d_vert_tv <- debounce(vert_tv, 2000)                               #
  
  primary_tv <- reactive({                                           #
    input$primary_tv                                                 #
  })                                                                 #
  d_primary_tv <- debounce(primary_tv, 2000)                         #
  
  ag_div_tv <- reactive({                                            #
    input$ag_div_tv                                                  #
  })                                                                 #
  d_ag_div_tv <- debounce(ag_div_tv, 2000)                           #
  
  step_tv <- reactive({                                              #
    input$step_tv                                                  #
  })                                                                 #
  d_step_tv <- debounce(step_tv, 2000)                               #
  
  event_tv <- reactive({                                             #
    input$event_tv                                                 #
  })                                                                 #
  d_event_tv <- debounce(event_tv, 2000)                             #
  
  eventtype_tv <- reactive({                                         #
    input$eventtype_tv                                             #
  })                                                                 #
  d_eventtype_tv <- debounce(eventtype_tv, 2000)                     #
  
  product_tv <- reactive({                                           #
    input$product_tv                                               #
  })                                                                 #
  d_product_tv <- debounce(product_tv, 2000)                         #
  
  height_tv <- reactive({                                            #
    input$height_tv                                                #
  })                                                                 #
  d_height_tv <- debounce(height_tv, 2000)                           #
  
  width_tv <- reactive({                                             #
    input$width_tv                                                 #
  })                                                                 #
  d_width_tv <- debounce(width_tv, 2000)                             #
  
  personnel_tv <- reactive({                                         #
    input$personnel_tv                                             #
  })                                                                 #
  d_personnel_tv <- debounce(personnel_tv, 2000)                     #
  #==================================================================#    
  #==================================================================#
  
  
  #------------------------------------------------------------------#
  #------------------------------------------------------------------#
  # Setting up selectInput choices based on the loaded file          #
  #
  steps <- reactive({                                                #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$Step)))                                  #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "step",                                                    #
      label = "Select Steps to Display",                         #
      choices = c("All", steps()),                               #
      selected = "Event Execution"                               #
    )                                                              #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "step_tv",                                                #
      label = "Select Steps to Display",                         #
      choices = c("All", steps()),                               #
      selected = "Event Execution"                               #
    )                                                              #
  })                                                                 #
  #*******************************************************************
  #
  events <- reactive({                                               #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$Event)))                                 #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "event",                                                   #
      label = "Select Events to Display",                        #
      choices = c("All", events()),                              #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "event_tv",                                               #
      label = "Select Events to Display",                        #
      choices = c("All", events()),                              #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #*******************************************************************
  #
  eventtypes <- reactive({                                           #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$EventType)))                             #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "eventtype",                                               #
      label = "Select Event Types to Display",                   #
      choices = c("All", eventtypes()),                          #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "eventtype_tv",                                           #
      label = "Select Event Types to Display",                   #
      choices = c("All", eventtypes()),                          #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #*******************************************************************
  #
  products <- reactive({                                             #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$Product)))                               #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "product",                                                 #
      label = "Select Products to Display",                      #
      choices = c("All", products()),                            #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "product_tv",                                             #
      label = "Select Products to Display",                      #
      choices = c("All", products()),                            #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #*******************************************************************
  #
  divisions <- reactive({                                            #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$Division)))                              #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "division",                                                #
      label = "Select Divisions to Display",                     #
      choices = c("All", divisions()),                           #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  programs <- reactive({                                             #
    input$dataFile                                                 #
    # input$outlook                                                #
    # input$step                                                   #
    # input$event                                                  #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$Program)))                               #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "program",                                                 #
      label = "Select Programs to Display",                      #
      choices = c("All", programs()),                            #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  otpos <- reactive({                                                #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$OTPO)))                                  #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "OTPO",                                                    #
      label = "Select OTPOs to Display",                         #
      choices = c("All", otpos()),                               #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  orsas <- reactive({                                                #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$ORSA)))                                  #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "ORSA",                                                    #
      label = "Select ORSAs to Display",                         #
      choices = c("All", orsas()),                               #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  mss <- reactive({                                                  #
    input$dataFile                                             #
    dt <- dt()                                                 #
    setDT(dt)                                                  #
    return(sort(unique(dt$MS)))                                #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "MS",                                                      #
      label = "Select MSs to Display",                           #
      choices = c("All", mss()),                                 #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  tms <- reactive({                                                  #
    input$dataFile                                             #
    dt <- dt()                                                 #
    setDT(dt)                                                  #
    return(sort(unique(dt$TM)))                                #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "TM",                                                      #
      label = "Select TMs to Display",                           #
      choices = c("All", tms()),                                 #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  dms <- reactive({                                                  #
    input$dataFile                                             #
    dt <- dt()                                                 #
    setDT(dt)                                                  #
    return(sort(unique(dt$DM)))                                #
  })                                                             #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "DM",                                                      #
      label = "Select DMs to Display",                           #
      choices = c("All", dms()),                                 #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #
  cas <- reactive({                                                  #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    return(sort(unique(dt$CA)))                                    #
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "CA",                                                      #
      label = "Select CAs to Display",                           #
      choices = c("All", cas()),                                 #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  
  
  
  
  
  personnel <- reactive({                                            #
    input$dataFile                                                 #
    dt <- dt()                                                     #
    setDT(dt)                                                      #
    if(input$ag_div_tv == "ORSA"){
      return( c("All", sort(unique(dt$ORSA)) ))
    } else if(input$ag_div_tv == "MS"){
      return( c("All", sort(unique(dt$MS)) ))
    } else if(input$ag_div_tv == "TM"){
      return( c("All", sort(unique(dt$TM)) ))
    } else if(input$ag_div_tv == "DM"){
      return( c("All", sort(unique(dt$DM)) ))
    } else if(input$ag_div_tv == "Cyber"){
      return( c("All", sort(unique(dt$CA)) ))
    } else if(input$ag_div_tv == "LF"){
      return( c("All") )
    }
  })                                                                 #
  #
  observe({                                                          #
    updateSelectInput(                                             #
      session,                                                   #
      "personnel_tv",                                            #
      label = "Select Personnel to Display",                     #
      choices = personnel(),                                     #
      selected = "All"                                           #
    )                                                              #
  })                                                                 #
  #------------------------------------------------------------------#
  #------------------------------------------------------------------#
  
  
  #######################################################################################
  data_issues <- reactive({                                                             #
    req(dataFile())                                                                     #
    inFile <- dataFile()                                                                #
    #
    if(is.null(inFile)){                                                                #
      return(NULL)                                                                      #
    }                                                                                   #
    else{                                                                               #
      #dt <- read_xlsx(inFile$datapath, 1, na = c("", "NA", "N/A"))                     #
      #
      #        connection <- odbcConnectAccess2007(                                              #
      #          inFile, uid = "jacob.j.warren", pwd = "AdminOTAD")                              #
      #                                                                                          #
      #        dt <- sqlFetch(                                                                   #
      #          connection, "datFormProgram", colnames = FALSE, rownames = FALSE,               #
      #            na.strings = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"),       #
      #            as.is = TRUE, stringsAsFactors = FALSE)                                       #
      #                                                                                          #
      #        odbcCloseAll()                                                                    #
      dt <- read_xlsx(inFile, sheet = 1,
                      na = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"))
      #
      setDT(dt)                                                                         #
      dt <- dt[Current == "Y"]                                                          #
      #
      # Turn StartDate and StopDate into date objects                                   #
      dt[, StartDate := format(as.Date(StartDate), format = "%Y-%m-%d")]                #
      dt[, StopDate := format(as.Date(StopDate), format = "%Y-%m-%d")]                  #
      dt[, StartDate := as.Date(StartDate)]                                             #
      dt[, StopDate := as.Date(StopDate)]                                               #
      #
      # Select and Rename columns.                                                      #
      dt <- dt[(StopDate < StartDate) | is.na(StartDate) | is.na(StopDate),             #
               .(ID, Program, Division, Step, Event, Product,                                  #
                 StartDate, StopDate, EventType, OTPO = OTPOPrimary,                           #
                 OTPO2 = OTPOSecondary, ORSA = ORSAPrimary, ORSA2 = ORSASecondary,             
                 MS = MSPrimary, MS2 = MSSecondary, TM = TMPrimary,                            
                 TM2 = TMSecondary, DM = DMPrimary, DM2 = DMSecondary,                         #
                 LFA = LFAnalyst, LCA, CA)                                                     #
      ]                                                                                 #
      #
      dt[is.na(StartDate) | is.na(StopDate), Issue := "Blank Date Column"]              #
      dt[(StopDate < StartDate), Issue := "Start Date > Stop Date"]                     #
      setcolorder(dt, c("ID", "Issue"))                                                 #
      dt                                                                                #
    }                                                                                   #
  })                                                                                    #
  #######################################################################################
  
  
  #-------------------------------------------------#
  output$data_issues <- renderDT(                   #
    data_issues(),                                  #
    class = "cell-border compact nowrap",           #
    #filter = list(position = "top", clear = TRUE), #
    selection = "multiple",                         #
    #editable = TRUE,                               #
    options = list(                                 #
      autoWidth = TRUE,                             #
      pageLength = 50,                              #
      order = list(list(1,"asc"))                   #, initComplete = JS('function(setting, json) { alert("done"); }')   #
    )                                               #
  )                                                 #
  #-------------------------------------------------#
  
  
  ###########################################################################################
  dt <- reactive({                                                                          #
    validate(                                                                             #
      need(                                                                               #
        dataFile(),                                                                       #
        "Please select an Excel .xlsx file using the 'File select' button."                #
      )                                                                                   #
    )                                                                                     #
    #
    # input$dataFile will be NULL initially. After the user selects                       #
    # and uploads a file, it will be a data frame with 'name',                            #
    # 'size', 'type', and 'datapath' columns. The 'datapath'                              #
    # column will contain the local filenames where the data can                          #
    # be found.                                                                           #
    #
    inFile <- dataFile()                                                                  #
    #
    if(is.null(inFile)){                                                                  #
      return(NULL)                                                                      #
    }                                                                                     #
    else{                                                                                 #
      #dt <- read_xlsx(inFile$datapath, 1, na = c("", "NA", "N/A"))                     #
      #
      #            connection <- odbcConnectAccess2007(                                              #
      #              inFile, uid = "jacob.j.warren", pwd = "AdminOTAD")                              #
      #                                                                                              #
      #            dt <- sqlFetch(                                                                   #
      #            connection, "datFormProgram", colnames = FALSE, rownames = FALSE,                 #
      #              na.strings = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"),         #
      #              as.is = TRUE, stringsAsFactors = FALSE)                                         #
      #                                                                                              #
      #            odbcCloseAll()                                                                    #
      dt <- read_xlsx(inFile, sheet = 1,
                      na = c("NA", "N/A", "", "na", "Na", "n/a", "N/a", "NaN", "nA"))
      #
      setDT(dt)                                                                         #
      dt <- dt[Current == "Y"]                                                          #
      #
      # Turn StartDate and StopDate into date objects                                   #
      dt[, StartDate := format(as.Date(StartDate), format = "%Y-%m-%d")]                #
      dt[, StopDate := format(as.Date(StopDate), format = "%Y-%m-%d")]                  #
      dt[, StartDate := as.Date(StartDate)]                                             #
      dt[, StopDate := as.Date(StopDate)]                                               #
      #
      n_nostart <<- dt[is.na(StartDate), .N]                                            #
      n_nostop <<- dt[is.na(StopDate), .N]                                              #
      n_invalid <<- dt[StopDate < StartDate, .N]                                        #
      #
      dt <- dt[!(StopDate < StartDate)]                                                 #
      #
      # Add the Start and Stop Weekday to the dt                                        #
      dt[, StartDOW := weekdays(StartDate)]                                             #
      dt[, StopDOW := weekdays(StopDate)]                                               #
      #
      # For those records that don't have a stop date,                                  #
      # set it to the end_date                                                          #
      #dt[is.na(StopDOW), ]                                                             #
      today <- Sys.Date()                                                               #
      #outlook <- input$outlook                                                         #
      #end_date <- as.Date(today + outlook)                                             #
      end_date <- dt[!is.na(StopDate), max(StopDate)]                                   #
      dt[is.na(StopDOW), StopDate := end_date]                                          #
      #
      dt[, DaysToStart := difftime(StartDate, today, units = "days")]                   #
      #
      # Select and Rename columns.                                                      #
      dt <- dt[,                                                                        #
               .(Program, DaysToStart, Division, Step, Event, Product,                         #
                 StartDate, StopDate, EventType, OTPO = OTPOPrimary,                           #
                 OTPO2 = OTPOSecondary, ORSA = ORSAPrimary, ORSA2 = ORSASecondary,             
                 MS = MSPrimary, MS2 = MSSecondary, TM = TMPrimary,                            
                 TM2 = TMSecondary, DM = DMPrimary, DM2 = DMSecondary,                         #
                 LFA = LFAnalyst, LCA, CA)                                                     #
      ]                                                                                 #
      #
      # Calculate duration                                                              #
      dt[,Duration := StopDate - StartDate]                                             #
      #
      dt[is.na(OTPO), OTPO := "NA"]                                                     #
      dt[is.na(OTPO2), OTPO2 := "NA"]                                                   #
      dt[is.na(ORSA), ORSA := "NA"]                                                     #
      dt[is.na(ORSA2), ORSA2 := "NA"]                                                   
      dt[is.na(MS), MS := "NA"]                                                         #
      dt[is.na(MS2), MS2 := "NA"]                                                       
      dt[is.na(TM), TM := "NA"]                                                         #
      dt[is.na(TM2), TM2 := "NA"]                                                       #
      dt[is.na(DM), DM := "NA"]                                                         #
      dt[is.na(DM2), DM2 := "NA"]                                                       #
      dt[is.na(LFA), LFA := "NA"]                                                       #
      dt[is.na(LCA), LCA := "NA"]                                                       #
      dt[is.na(CA), CA := "NA"]                                                         #
      #
      dt                                                                                #
    }                                                                                     #
  })                                                                                        #
  ###########################################################################################
  
  
  #######################################
  # output$programs <- renderText({     #
  #     if("All" %in% input$program){   #
  #         programs()                  #
  #     } else {                        #
  #         input$program               #
  #     }                               #
  # })                                  #
  #######################################
  
  
  ########################################################################################  
  table_filter1 <- reactive({                                                            #
    #req(dataFile())                                                                   #
    dt <- dt()                                                                         #
    #
    today <- Sys.Date()                                                                #
    outlook <- d_outlook()                                                             #
    end_date <- as.Date(today + outlook)                                               #
    #date_range <- seq(today - d_rlook(), end_date, by = "day")                                     #
    #
    #############################################                                      #
    if("All" %in% d_step()){                                                           #
      steps <- steps()                                                               #
    } else {                                                                           #
      steps <- d_step()                                                              #
    }                                                                                  #
    #
    if("All" %in% d_event()){                                                          #
      events <- events()                                                             #
    } else {                                                                           #
      events <- d_event()                                                            #
    }                                                                                  #
    #
    if("All" %in% d_eventtype()){                                                      #
      eventtypes <- eventtypes()                                                     #
    } else {                                                                           #
      eventtypes <- d_eventtype()                                                    #
    }                                                                                  #
    #
    if("All" %in% d_product()){                                                        #
      products <- products()                                                         #
    } else {                                                                           #
      products <- d_product()                                                        #
    }                                                                                  #
    #
    if("All" %in% d_division()){                                                       #
      divisions <- divisions()                                                       #
    } else {                                                                           #
      divisions <- d_division()                                                      #
    }                                                                                  #
    #
    if("All" %in% d_program()){                                                        #
      programs <- programs()                                                         #
    } else {                                                                           #
      programs <- d_program()                                                        #
    }                                                                                  #
    #
    if("All" %in% d_otpo()){                                                           #
      otpos <- otpos()                                                               #
    } else {                                                                           #
      otpos <- d_otpo()                                                              #
    }                                                                                  #
    #
    if("All" %in% d_orsa()){                                                           #
      orsas <- orsas()                                                               #
    } else {                                                                           #
      orsas <- d_orsa()                                                              #
    }                                                                                  #
    #
    if("All" %in% d_ms()){                                                             #
      mss <- mss()                                                                   #
    } else {                                                                           #
      mss <- d_ms()                                                                  #
    }                                                                                  #
    #
    if("All" %in% d_tm()){                                                             #
      tms <- tms()                                                                   #
    } else {                                                                           #
      tms <- d_tm()                                                                  #
    }                                                                                  #
    #
    if("All" %in% d_dm()){                                                             #
      dms <- dms()                                                                   #
    } else {                                                                           #
      dms <- d_dm()                                                                  #
    }                                                                                  #
    #
    if("All" %in% d_ca()){                                                             #
      cas <- cas()                                                                   #
    } else {                                                                           #
      cas <- d_ca()                                                                  #
    }                                                                                  #
    #############################################                                      #
    #
    dt <- dt[                                                                          #
      Step %in% steps                                                                  #
      & Event %in% events                                                              #
      & EventType %in% eventtypes                                                      #
      & Product %in% products                                                          #
      & Division %in% divisions                                                        #
      & Program %in% programs                                                          #
      & (OTPO %in% otpos | OTPO2 %in% otpos)                                           #
      & (ORSA %in% orsas | ORSA2 %in% orsas)                                            
      & (MS %in% mss | MS2 %in% mss)                                                    
      & (TM %in% tms | TM2 %in% tms)                                                   #
      & (DM %in% dms | DM2 %in% dms)                                                   #
      & (CA %in% cas | LCA %in% cas)                                                   #
      ,                                                                                #
      .(Program, DaysToStart, Division, Step, Event,                                   #
        Product, StartDate, StopDate, Duration, EventType,                             #
        OTPO, OTPO2, ORSA, ORSA2, MS, MS2, TM, TM2, DM, DM2, LFA, LCA, CA)             
    ]                                                                                  #
    #
    setorder(dt, StartDate)                                                            #
    #
    # Turn variables into factors                                                      #
    dt[, Division := factor(Division)]                                                 #
    dt[, Program := factor(Program)]                                                   #
    dt[, Event := factor(Event)]                                                       #
    dt[, EventType := factor(EventType)]                                               #
    dt[, Product := factor(Product)]                                                   #
    dt[, Step := factor(Step)]                                                         #
    #
    return(dt)                                                                         #
  })                                                                                     #
  ########################################################################################
  
  
  #############################################################################
  table_filter2 <- reactive({                                                 #
    req(dataFile())                                                         #
    dt <- table_filter1()                                                   #
    #
    today <- Sys.Date()                                                     #
    outlook <- d_outlook()                                                  #
    #
    dt <- dt[                                                               #
      StartDate <= today+outlook                                            #
      ,                                                         #
      .(Program, DaysToStart, Division, Step, Event,                        #
        Product, StartDate, StopDate, Duration, EventType,                  #
        OTPO, OTPO2, ORSA, ORSA2, MS, MS2, TM, TM2, DM, DM2, LFA, LCA, CA)  
    ]                                                                       #
    #
    setorder(dt, StartDate)                                                 #
    return(dt)                                                              #
  })                                                                          #
  #############################################################################
  
  
  ######################################################################################
  output$my_table <- renderDT(server = FALSE, {                                        #
    datatable(                                                                         #
      table_filter2(),                                                                 #
      extensions = "Buttons",                                                          #
      selection = "multiple",                                                          #
      class = "cell-border compact nowrap",                                            #
      options = list(                                                                  #
        buttons = list(                                                                #
          list(extend = "excel", exportOptions = list(modifier = list(page = "all"))), #
          list(extend = "csv", exportOptions = list(modifier = list(page = "all"))),   #
          list(                                                                        #
            extend = "pdf",                                                            #
            orientation = "landscape",                                                 #
            exportOptions = list(modifier = list(page = "all"))                        #
          ),                                                                           #
          list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),  #
          list(extend = "print", exportOptions = list(modifier = list(page = "all")))  #
        ),                                                                             #
        dom = "Bfrtip",                                                                #
        autowidth = TRUE,                                                              #
        pageLength = 50,                                                               #
        order = list(list(7,"asc")),                                                   #
        searchCols = list(                                                             #
          NULL, NULL, NULL, NULL,                                                      #
          NULL, NULL, NULL, NULL,                                                      #
          NULL, NULL, NULL, NULL,                                                      #
          NULL, NULL, NULL, NULL                                                       #
        )                                                                              #
      )                                                                                #
    ) }                                                                                #
  )                                                                                    #
  ######################################################################################
  
  
  
  
  
  ##############################################################################################
  output$plotly <- renderPlotly({                                                              #
    #input$dataFile                                                                          #
    #
    validate(                                                                                #
      need(                                                                                  #
        dataFile(),                                                                          #
        "Please select an Excel .xlsx file using the 'File select' button."                   #
      )                                                                                      #
    )                                                                                        #
    #
    validate(                                                                                #
      need(                                                                                  #
        d_outlook(),                                                                         #
        "Please input a positive numeric value into the 'Outlook Days' interface."           #
      )                                                                                      #
    )                                                                                        #
    #
    validate(                                                                                #
      need(                                                                                  #
        d_program(),                                                                         #
        "Please select a value using the 'Select Programs to Display' interface."            #
      )                                                                                      #
    )                                                                                        #
    #
    validate(                                                                                #
      need(                                                                                  #
        d_step(),                                                                            #
        "Please select a value using the 'Select Steps to Display' interface."               #
      )                                                                                      #
    )                                                                                        #
    #
    dt <- table_filter1() #dt()                                                              #
    #
    dt <- dt[                                                                                #
      #Step %in% d_step()                                                                    #
      #& Event %in% d_event()                                                                #
      #& Program %in% programs                                                               #
      #& OTPO %in% otpos                                                                     #
      ,                                                                                      #
      .(Program, Division, Step, Event,                                                      #
        Product, StartDate, StopDate, EventType,                                             #
        OTPO, OTPO2, ORSA, ORSA2, MS, MS2, TM, TM2, DM, DM2, LFA, LCA, CA)                   
    ]                                                                                        #
    #
    setorder(dt, StartDate)                                                                  #
    # 
    outlook <- d_outlook()                                                                   #
    today <- Sys.Date()                                                                      #
    end_date <- as.Date(today + outlook)                                                     #
    #
    # Order y-axis by start date by turning program into a factor                            #
    # and specifying the levels based on StartDate                                           #
    program_by_start_date2 <- dt[, .(StartDate = min(StartDate)), Program]                   #
    setorder(program_by_start_date2, StartDate)                                              #
    program_start_order2 <- program_by_start_date2$Program                                   #
    dt[, Program := factor(Program, levels = program_start_order2)]                          #
    #
    # Calculate duration                                                                     #
    dt[,Duration := StopDate - StartDate]                                                    #
    #
    dt[, id := 1:nrow(dt)]                                                                   #
    #
    dt_long <- dt[ , .(                                                                      #
      id, Program, Division, Step, Event,                                                  #
      Product, StartDate, StopDate, Duration, EventType,                                   #
      OTPO, OTPO2, ORSA, ORSA2, MS, MS2, TM, TM2, DM, DM2, LFA, LCA, CA,                   
      Date =                                                                               #
        #seq(today, end_date, by = "day")                                                #
        seq(min(StartDate), max(StopDate), by = "day")                                   #
    ), by = 1:nrow(dt)]                                                                      #
    #
    # Remove rows that are outside the StartDate and StopDate for the given program          #
    final_dt <- dt_long[StartDate <= Date & StopDate >= Date,]                               #
    #
    ymax <- length(final_dt[StartDate <= end_date, unique(Program)])                         #
    #
    # Verticals Numeric and Date formatted                                                   #
    vertN <- d_vert() %>% strsplit(",") %>% unlist%>% as.numeric                             #
    #vertD <- vertN + today                                                                   #
    #
    vert_df <- expand.grid(                                                                  #
      vertN = vertN,                                                                         #
      y = final_dt[, seq(min(as.numeric(Program))-1, max(as.numeric(Program))+1, length.out = 3*(max(as.numeric(Program))-min(as.numeric(Program))+2) )]         #                      # final_dt[,max(id]
    )                                                                                        #
    vert_df$vertD <- vert_df$vertN + today                                                   #
    #
    execution_ribbon <- final_dt %>%                                                         #
      ggplot() +                                                                           #
      geom_ribbon(                                                                         #
        aes(x = as.Date(Date),                                        #
            ymin = as.numeric(Program)-.25,                                              #
            ymax = as.numeric(Program)+.25,                                              #
            fill = eval(parse(text=input$colorby)),                                      #
            group = interaction(id,Program),                                             #
            text = paste(                                                                #
              'Program:', Program,                                                     #
              '</br></br>Division:', Division,                                         #
              '</br>Step:', Step,                                                      #
              '</br>Event:', Event,                                                    #
              '</br>Product:', Product,                                                #
              '</br>Start Date:', StartDate,                                           #
              '</br>End Date:', StopDate,                                              #
              '</br>Duration:', Duration,                                              #
              '</br>Event Type:', EventType,                                           #
              '</br>OTPO:', OTPO,                                                      #
              '</br>OTPO2:', OTPO2,                                                    #
              '</br>ORSA:', ORSA,                                                      #
              '</br>ORSA2:', ORSA2,                                                    
              '</br>MS:', MS,                                                          #
              '</br>MS2:', MS2,                                                        
              '</br>TM:', TM,                                                          #
              '</br>TM2:', TM2,                                                        #
              '</br>DM:', DM,                                                          #
              '</br>DM:', DM2,                                                         #
              '</br>LFA:', LFA,                                                        #
              '</br>LCA:', LCA,                                                        #
              '</br>CA:', CA                                                           #
            ))) +                                                                        #
      scale_y_continuous(                                                                  #
        breaks = 1:nlevels(final_dt$Program),                                              #
        labels = levels(final_dt$Program)                                                  #
      ) +                                                                                  #
      theme(                                                                               #
        panel.grid.minor.x = element_blank(),                                            #
        panel.grid.minor.y = element_blank(),                                            #
        legend.title = element_blank()                                                   #
      ) +                                                                                  #
      labs(x = "Date", y = "") +                                                           #
      geom_line(                                                                           #
        data = vert_df,                                                                    #
        aes(                                                                               #
          x = as.Date(vertD, format = "%Y-%m-%d"),                     #
          y = y,                                                                           #
          group = vertD,                                                                   #
          text = paste('Date:', vertD, '</br></br>Outlook Days:', vertN)                   #
        ),                                                                                 #
        linetype = 2                                                                       #
      ) +
      scale_x_date(breaks = "1 month")
    #
    #            geom_vline(                                                                          #
    #              data = data.frame(vertN, vertD),                                                   #
    #              aes(                                                                               #
    #                xintercept = as.Date(vertD),              #
    #                text = paste('Date:', vertD, '</br></br>Outlook Days:', vertN)                   #
    #              ),                                                                                 #
    #              linetype = 2                                                                       #
    #            )                                                                                    #
    #        execution_ribbon                                                                        #
    #
    execution_ribbon_plotly <- execution_ribbon %>% ggplotly(                                #
      height = d_height(), width = d_width(),                                              #
      tooltip = c("text")#, "fill")                                                        #
    ) %>%                                                                                    #
      layout(                                                                              #
        yaxis = list(range = c(1-.35,ymax+.35)),                                         #
        xaxis = list(                                                                    #
          range = c(                                                                   #
            as.numeric(as.Date(today - d_rlook())),                                  #
            as.numeric(as.Date(today+outlook))                                       #
          )),
        legend = list(title = list(text = NULL))                                         #
      )                                                                                #
    #
    execution_ribbon_plotly                                                                  #
  })                                                                                           #
  ##############################################################################################
  
  
  output$message1 <- renderText({###############################################################
    req(dataFile())                                                                          #
    req(d_outlook())                                                                         #
    #
    dt <- dt()                                                                               #
    setDT(dt)                                                                                #
    #
    (today <- Sys.Date())                                                                    #
    outlook <- d_outlook()                                                                   #
    end_date <- as.Date(today + outlook)                                                     #
    #
    # Turn StartDate and StopDate into date objects                                          #
    dt[, StartDate := as.Date(StartDate)]                                                    #
    dt[, StopDate := as.Date(StopDate)]                                                      #
    #
    #date_range <- seq(today, end_date, by = "day")                                           #
    #
    # Add the Start and Stop Weekday to the dt                                               #
    dt[, StartDOW := weekdays(StartDate)]                                                    #
    dt[, StopDOW := weekdays(StopDate)]                                                      #
    #
    # For those records that don't have a stop date,                                         #
    # set it to the end_date                                                                 #
    #dt[is.na(StopDOW), ]                                                                    #
    dt[is.na(StopDOW), StopDate := end_date]                                                 #
    #
    big_date <- max(dt$StartDate, dt$StopDate)                                               #
    big_outlook_days <- difftime(big_date, today, units = "days")                            #
    #
    paste0(                                                                                  #
      "The largest date in the file is ", format(big_date, "%d %B %Y"),                    #
      ". This is ", big_outlook_days, " days from today, (",                               #
      formatC(big_outlook_days/365.25, digits = 4), " years)."                             #
    )                                                                                        #
  })############################################################################################
  
  
  output$message1a <- renderText({##############################################################
    req(dataFile())                                                                          #
    #
    if(n_nostart == 1){                                                                      #
      "There was 1 record with no start date."                                             #
    } else                                                                                   #
      if(n_nostart > 1){                                                                       #
        paste0("There were ", n_nostart, " records with no start date.",                     #
               " See the 'Data Issues' tab.")                                                     #
      } else {                                                                                 #
        NULL                                                                                 #
      }                                                                                        #
  })############################################################################################
  
  
  output$message1b <- renderText({##############################################################
    req(dataFile())                                                                          #
    #
    if(n_nostop == 1){                                                                       #
      paste0(                                                                              #
        "There was 1 record with no stop date. ",                                        #
        "A stop date was added for this record setting ",                                #
        "it equal the maximum stop date in the file. See the 'Data Issues' tab.")        #
    } else                                                                                   #
      if(n_nostop > 1){                                                                        #
        paste0(                                                                              #
          "There were ", n_nostop, " records with no stop dates. ",                          #
          "Stop dates were added for these records setting them ",                           #
          "equal the maximum stop date in the file. See the 'Data Issues' tab.")             #
      } else {                                                                                 #
        NULL                                                                                 #
      }                                                                                        #
  })############################################################################################
  
  
  output$message1c <- renderText({##############################################################
    req(dataFile())                                                                          #
    #
    if(n_invalid == 1){                                                                      #
      paste0(                                                                              #
        "There was 1 record with an invalid start date > stop date. ",                   #
        "This record was removed and is not displayed.")                                 #
    } else                                                                                   #
      if(n_invalid > 1){                                                                   #
        paste0(                                                                              #
          "There were ", n_invalid, " records with no start date > stop date. ",           #
          "These record were removed and is not displayed. See the 'Data Issues' tab.")    #
      } else {                                                                                 #
        NULL                                                                                 #
      }                                                                                        #
  })############################################################################################
  
  
  output$message2 <- renderText({###############################################################
    req(dataFile())                                                                          #
    req(d_outlook())                                                                         #
    #
    today <- Sys.Date() - d_rlook()                                                          #
    outlook <- d_outlook() + d_rlook()                                                       #
    end_date <- as.Date(today + outlook)                                                     #
    #
    paste(                                                                                   #
      "The x-axis is set to a range from",                                                 #
      format(today, "%d %B %Y"), "to", format(end_date, "%d %B %Y"),                       #
      "(a duration of", outlook,                                                           #
      "days). Drag the x-axis to change the date range, or change",                        #
      "'Outlook Days' or 'Retrospective Days' input."                                      #
    )                                                                                        #
  })############################################################################################
  
  
  output$message3 <- renderText({###############################################################
    req(dataFile())                                                                          #
    req(d_outlook())                                                                         #
    #
    today <- Sys.Date() - d_rlook()                                                          #
    outlook <- d_outlook() + d_rlook()                                                       #
    end_date <- as.Date(today + outlook)                                                     #
    #
    paste(                                                                                   #
      "The y-axis is set to include only programs executing selected steps, events, etc.", # 
      "between the dates of",                                                              #
      format(today, "%d %B %Y"), "to", format(end_date, "%d %B %Y"),                       #
      "(a duration of", outlook,                                                           #
      "days).\n Drag the y-axis to see additional programs,",                              #
      "or change 'Outlook Days' and 'Retrospective Days' input."                           #
    )                                                                                        #
  })############################################################################################
  
  output$message <- renderText({###############################################################
    #class(input$dataFile)
    if(class(input$dataFile) != "list"){                                                       #is.numeric(input$dataFile)
      "Please select an Excel .xlsx file using the 'File select' button."
    } else{
      NULL
      #return(input$dataFile)
    }
  })############################################################################################
  
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  timevis_filter <- reactive({
    req(dataFile())
    
    dt <- dt()
    setDT(dt)
    dt[, Row := 1:.N]
    
    today <- Sys.Date() - d_rlook_tv()
    outlook <- d_outlook_tv()
    
    # Display primary only?
    primary <- d_primary_tv()
    
    if(primary == "Primary"){
      dt_long <- melt(
        dt,
        id.vars = c("Row", "Program", "DaysToStart", "Division", "Step", "Event", "Product",
                    "StartDate", "StopDate", "EventType", "OTPO", "OTPO2"),
        measure.vars = c("ORSA", "MS", "TM", "DM", "LFA", "LCA"),
        variable.name = "AG_Role",
        value.name = "Person"
      )
    } else if(primary == "Secondary"){
      dt_long <- melt(
        dt,
        id.vars = c("Row", "Program", "DaysToStart", "Division", "Step", "Event", "Product",
                    "StartDate", "StopDate", "EventType", "OTPO", "OTPO2"),
        measure.vars = c("ORSA2", "MS2", "TM2", "DM2", "LFA", "CA"),
        variable.name = "AG_Role",
        value.name = "Person"
      )
    } else {
      dt_long <- melt(
        dt,
        id.vars = c("Row", "Program", "DaysToStart", "Division", "Step", "Event", "Product",
                    "StartDate", "StopDate", "EventType", "OTPO", "OTPO2"),
        measure.vars = c("ORSA", "ORSA2", "MS", "MS2", "TM", "TM2", "DM", "DM2", "LFA", "LCA", "CA"),
        variable.name = "AG_Role",
        value.name = "Person"
      )
    }
    
    dt_long <- dt_long[, AG_Role := as.character(AG_Role)]
    
    dt_long[AG_Role == "ORSA2", AG_Role := "ORSA"]
    dt_long[AG_Role == "MS2", AG_Role := "MS"]
    dt_long[AG_Role == "TM2", AG_Role := "TM"]
    dt_long[AG_Role == "DM2", AG_Role := "DM"]
    dt_long[AG_Role == "LFA", AG_Role := "LF"]
    dt_long[AG_Role == "LCA", AG_Role := "Cyber"]
    dt_long[AG_Role == "CA", AG_Role := "Cyber"]
    
    dt_long[, Duration := paste(StopDate - StartDate, "days")]
    
    ##############################################
    if("All" %in% d_step_tv()){                  #
      steps_tv <- steps()                      #
    } else {                                     #
      steps_tv <- d_step_tv()                  #
    }                                            #
    #
    if("All" %in% d_event_tv()){                 #
      events_tv <- events()                    #
    } else {                                     #
      events_tv <- d_event_tv()                #
    }                                            #
    #
    if("All" %in% d_eventtype_tv()){             #
      eventtypes_tv <- eventtypes()            #
    } else {                                     #
      eventtypes_tv <- d_eventtype_tv()        #
    }                                            #
    #
    if("All" %in% d_product_tv()){               #
      products_tv <- products()                #
    } else {                                     #
      products_tv <- d_product_tv()            #
    }                                            #
    
    
    if("All" %in% d_personnel_tv()){             #
      personnel_tv <- personnel()              #
    } else {                                     #
      personnel_tv <- d_personnel_tv()         #
    }                                            #
    
    
    dt_long <- dt_long[, AG_Role := as.character(AG_Role)]
    
    sked <- dt_long[
      #StartDate <= today+outlook &   # moved so it is done in timevis_dt, that way it restricts the timevis x-axis but not the plotly
      Step %in% steps_tv &
        Event %in% events_tv &
        EventType %in% eventtypes_tv &
        Product %in% products_tv &
        AG_Role %in% d_ag_div_tv() &
        Person %in% personnel_tv
    ]
    
    # This will remove instances where for example we have MSPrimary and MSSecondary both as NA,
    # causing the program to show up twice displayed for NA
    sked <- unique(sked)
    
    sked[, Person := factor(Person, levels = sort(unique(Person), decreasing = TRUE))]
    sked[, id := 1:.N]
    
    #sked[, .N, .(Row, AG_Role, Person)]
    
    #sked <- sked[,
    #  .(DaysToStart, Division, Step, Event, Product,
    #    StartDate, StopDate, EventType, OTPO, OTPO2),
    #.(Row, AG_Role, Person, Program)
    #]
    
    sked
  })
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  output$sked_output <- renderPrint( str(timevis_filter()) )
  
  output$dt_output <- renderPrint( str(dt()) )
  
  
  output$my_table_tv <- renderDT(server = FALSE, {
    timevis_filter()
  })
  
  
  
  
  
  #--------------------------------------------------------------
  # This is what gets sent to timevis for plotting the bars
  timevis_dt <- reactive({
    sked <- timevis_filter()
    setDT(sked)
    
    today <- Sys.Date() - d_rlook_tv()
    outlook <- d_outlook_tv()
    
    #sked <- sked[
    #  StartDate <= today+outlook
    #]
    
    sked <- sked[,
                 .(
                   id = id, #Row,
                   start = StartDate,
                   content = Program,
                   title = paste("Program:", Program, "</br>Divison:", Division,  "</br>Step:", Step, "</br>Event:", Event, "</br>Event Type:", EventType,
                                 "</br>Day to Start:", DaysToStart, "</br>Start Date:", StartDate,  "</br>Stop Date:", StopDate, "</br>Duration:", Duration),
                   end = StopDate,
                   group = Person,
                   className = gsub("&", "", EventType)
                 )
    ]
    
    setkey(sked, group)
    sked
  })
  #--------------------------------------------------------------
  
  
  
  #--------------------------------------------------------------
  # This is what gets sent to timevis for formatting purposes
  # (what is displayed in hover-over on the bars)
  timevis_groups <- reactive({
    sked <- timevis_filter()
    setDT(sked)
    
    sked <- sked[,
                 .(
                   id = unique(Person),
                   content = unique(Person)
                 )
    ]
    
    sked[, id := factor(id, levels = sort(unique(id), decreasing = TRUE))]
    
    setkey(sked, id)
    sked
  })
  #--------------------------------------------------------------
  
  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # Creates the timevis plot
  output$tv <- renderTimevis({
    
    today <- Sys.Date() - d_rlook_tv()
    outlook <- d_outlook_tv()
    end <- today+outlook
    
    timevis(
      timevis_dt(),
      groups = timevis_groups()
    ) %>%
      setWindow(start = today, end = end)
  })
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  
  #========================================================================================
  output$plotly2 <- renderPlotly({                                                        #
    #
    sked <- timevis_filter()                                                              #
    setDT(sked)                                                                           #
    #
    setorder(sked, StartDate)                                                             #
    #
    outlook <- d_outlook_tv()                                                             #
    today <- Sys.Date()                                                                   #
    end_date <- as.Date(today + outlook)                                                  #
    #
    # Order y-axis by start date by turning program into a factor                         #
    # and specifying the levels based on StartDate                                        #
    #      person_by_start_date2 <- sked[, .(StartDate = min(StartDate)), Person]                #
    #      setorder(person_by_start_date2, StartDate)                                            #
    #      person_start_order2 <- person_by_start_date2$Person                                   #
    #      sked[, Person := factor(Person, levels = sort(unique(Person), decreasing = TRUE))]    #, levels = person_start_order2)]                        # order by person start date?
    #
    # Calculate duration                                                                  #
    #sked[,Duration := StopDate - StartDate]                                               #
    #
    # outlook <- d_outlook()                                                                #
    # today <- Sys.Date()                                                                   #
    # end_date <- as.Date(today + outlook)                                                  #
    #
    # DaysToStart and Duration not in here                                                #
    dt_long <- sked[ , .(                                                                 #
      Row, Program, Division, Step, Event,                                                #
      Product, StartDate, StopDate, EventType,                                            #
      AG_Role, Person, Duration,                                                                    #
      Date =                                                                              #
        #seq(today, end_date, by = "day")                                                 #
        seq(min(StartDate), max(StopDate), by = "day")                                    #
    ), by = 1:nrow(sked)]                                                                 #
    #
    #      
    # Remove rows that are outside the StartDate and StopDate for the given program       #
    final_dt <- dt_long[StartDate <= Date & StopDate >= Date,]                            #
    #
    ymax <- length(final_dt[, unique(Person)])                                            # StartDate <= end_date
    #
    # Verticals Numeric and Date formatted                                                #
    vertN <- d_vert_tv() %>% strsplit(",") %>% unlist%>% as.numeric                       #
    #vertD <- vertN + today                                                               #
    #
    vert_df <- expand.grid(                                                               #
      vertN = vertN,                                                                      #
      #y = final_dt[, seq(min(Row)-1, max(Row)+1, length.out = 3*(max(Row)-min(Row)+2) )]      # changed from id to Row
      y = final_dt[, seq(min(as.numeric(Person))-1, max(as.numeric(Person))+1, length.out = 3*(max(as.numeric(Person))-min(as.numeric(Person))+2) )]      # changed from id to Row
    )                                                                                     #
    vert_df$vertD <- vert_df$vertN + today                                                #
    execution_ribbon <- final_dt %>%                                                      #
      ggplot() +                                                                          #
      geom_ribbon(                                                                        #
        aes(x = as.Date(Date),                     #
            ymin = as.numeric(Person)-.25,                                                #
            ymax = as.numeric(Person)+.25,                                                #
            fill = eval(parse(text=input$colorby_tv)),                                    #
            group = interaction(Row, Program, Person),                                    #
            text = paste(                                                                 #
              'Program:', Program,                                                        #
              '</br></br>Division:', Division,                                            #
              '</br>Step:', Step,                                                         #
              '</br>Event:', Event,                                                       #
              '</br>Product:', Product,                                                   #
              '</br>Start Date:', StartDate,                                              #
              '</br>End Date:', StopDate,                                                 #
              '</br>Duration:', Duration,                                                 #
              '</br>AG Role:', AG_Role,                                                   #
              '</br>Event Type:', EventType,                                              #
              '</br>Person:', Person                                                      #
            ))) +                                                                         #
      scale_y_continuous(                                                                 #
        breaks = 1:nlevels(final_dt$Person),                                              #
        labels = levels(final_dt$Person)                                                  #
      ) +                                                                                 #
      theme(                                                                              #
        panel.grid.minor.x = element_blank(),                                             #
        panel.grid.minor.y = element_blank(),                                             #
        legend.title = element_blank()                                                    #
      ) +                                                                                 #
      labs(x = "Date", y = "") +                                                          #
      geom_line(                                                                          #
        data = vert_df,                                                                   #
        aes(                                                                              #
          x = as.Date(vertD),                    #
          y = y,                                                                          #
          group = vertD,                                                                  #
          text = paste('Date:', vertD, '</br></br>Outlook Days:', vertN)                  #
        ),                                                                                #
        linetype = 2                                                                      #
      ) +
      scale_x_date(breaks = "1 month")                                                    #
    #            geom_vline(                                                                    #
    #              data = data.frame(vertN, vertD),                                             #
    #              aes(                                                                         #
    #                xintercept = as.numeric(as.POSIXct(vertD, format="%Y-%m-%d"))*1000,        #
    #                text = paste('Date:', vertD, '</br></br>Outlook Days:', vertN)             #
    #              ),                                                                           #
    #              linetype = 2                                                                 #
    #            )                                                                              #
    #        execution_ribbon                                                                   #
    #
    execution_ribbon_plotly <- execution_ribbon %>% ggplotly(                             #
      height = d_height_tv(), width = d_width_tv(),                                       #
      tooltip = c("text")#, "fill")                                                       #
    ) %>%                                                                                 #
      layout(                                                                             #
        yaxis = list(range = c(1-.35,ymax+.35)),                                          #
        xaxis = list(                                                                     #
          range = c(                                                                      #
            as.numeric(as.Date(today - d_rlook_tv())),                                    #
            as.numeric(as.Date(today+outlook))                                            #
          )),
        legend = list(title = list(text = NULL))                                          #
      )                                                                                   #
    #
    execution_ribbon_plotly                                                               #
  })                                                                                      #
  #========================================================================================
  
  
}

  filedir <<- filedir
  shinyApp(ui = ui, server = server, option = list(launch.browser = launch.browser))
}


