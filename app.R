# Claudio Zanettini

source("other_f.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "COVID-19",
    tags$li(class = "dropdown", actionButton("license",
                                             label = "about",
                                             style = "text-transform: lowercase; font-style: italic;"
    ))
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "boe_website"
    ),
    fluidPage(
    fluidRow(
      box(
        div(
          style = "display: inline-block;vertical-align: center;",
          textOutput("updated")
        ),
        div(
          style = "display: inline-block; vertical-align: center; float:right",
          materialSwitch("log_scale", "Y-axis in log2", value = FALSE)
        ))),
    fluidRow(
      box(
        div(style = "display: inline-block; vertical-align: center; float:left",
        h2("Total Number of COVID-19 Cases")),
        selectizeInput(
          inputId = "imp_state",
          label = "Select one or more States (max 4):",
          choices = as.character(c("US", states)),
          selected = c("Maryland"),
          multiple = TRUE,
          options = list(maxItems = 4),
          width = "100%"
        ),
        tabBox(width = 12,
               tabPanel(id = "graph", title = "graph",
                       uiOutput("body_UI")),
        
                tabPanel(id = "map", title = "map", width = "50%",
                  
                  div(style = "display: inline-block; vertical-align: center; float:right",
                         selectizeInput(
                                inputId = "map",
                                label = NULL,
                                choices = c("Confirmed",
                                            "Deaths",
                                            "Recovered"),
                                selected = "Confirmed",
                                
                                width = "100%"
                                
                              )),
                  div(style = "display: inline-block; vertical-align: center; margin: auto", h4("Select Variable: ")),

            leafletOutput("leaf")))),
      
     column(6, 
      box(width = 12,
        h2("Total Number of Specimens Tested for COVID-19 in U.S."),
        h4(
          "Data of Confirmed, Deaths and Recovered from",
          a(href = "https://github.com/CSSEGISandData/COVID-19", "JHU."),
          br(),
          "Data of Specimens Tested from",
          a(href = "https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html", "CDC."),
          hr(style = " margin-top: 1.1em; margin-bottom: 1.2em;")
        ),
        
        
        plotlyOutput("tested", height = "400px")
      )
    )
  ))))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  # # Get data -----
  
  
  dat_US <- get_cases()
  tests <- get_tests()
  
  
  
  dat_f <- reactiveVal(NULL)
  dat_m <- reactiveVal(NULL)
  
  # filter data  ------------
  dat_f <- reactive({
    x <- input$imp_state
    dat_US %>%
      filter(state %in% x)
  })
  
  dat_m <- reactive({
    x <- input$map
    dat_f() %>%
      filter(condition ==  x)
  })
  
  # license --------
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$license
    },
    handlerExpr = {
      sendSweetAlert(
        session = session,
        title = NULL,
        html = TRUE,
        text =
          HTML("
             2020 Claudio Zanettini, <br>
             <a href=\"https://github.com/c1au6i0/COVID-19\"> Code of the app </a>"),
        width = "400px"
      )
    }
  )
  
  # Last update -----
  output$updated <- renderText({
    m_date <- max(dat_f()$date)
    
    x <- paste0("Last Update: ", m_date)
    x
  })
  
  
  # render test plot -----
  output$tested <- renderPlotly({
    p <- plot_tested(tests, log_scale = input$log_scale)
  })
  
  
  # show_p <- reactiveVal(value = FALSE)
  # output$show_p <- reactive({show_p()})
  # outputOptions(output, "show_p", suspendWhenHidden = FALSE)
  
  
  # Render plot_ cases -----
  output$cases <- renderPlotly({
    plot_cases(dat = dat_f(), log_scale = input$log_scale)
  })
  
  output$body_UI <- renderUI({
    n_cond <- length(unique(dat_f()$condition))
    
    if (n_cond <= 1) {
      p <- plotlyOutput("cases", height = "400px")
    }
    
    if (n_cond == 2) {
      p <- plotlyOutput("cases", height = "800px")
    }
    
    if (n_cond == 3) {
      p <- plotlyOutput("cases", height = "1200px")
    }
    p
  })
  
  # Render Map -------
  output$leaf <- renderLeaflet({
    map_leaf(dat = dat_m())
  })
  

  
}

# Run the application
shinyApp(ui = ui, server = server)

# https://stackoverflow.com/questions/38826893/shinydashboard-does-not-work-with-uioutput
