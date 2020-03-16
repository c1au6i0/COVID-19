# Claudio Zanettini

source("other_f.R")

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19",
                    tags$li(class = "dropdown", actionButton("license", 
                                                             label = "about",
                                                             style = "text-transform: lowercase; font-style: italic;")
                            )
                    ),
    dashboardSidebar(
      disable = TRUE
    ),  
    dashboardBody(
      shinyDashboardThemes(
        theme = "boe_website"
      ),
      fluidRow(
        box(
        textOutput("updated")
        )
      ),
      fluidRow(
        box(
          h2("Total Number of cases"),
          selectizeInput(
            inputId = "imp_state",
            label = "Select one or more States (max 4):",
            choices = as.character(c("US", states)),
            selected = c("Maryland", "Texas"),
            multiple = TRUE,
            options = list(maxItems = 4)),
          plotlyOutput("confirmed", height = "auto"),
          plotlyOutput("deaths", height = "auto"),
          plotlyOutput("recovered", height = "auto")
        ),
        
        box(
          h2("Total Number of Specimens Tested in U.S."),
          h4( "Data of Confirmed, Deaths and Recovered from", 
          a(href="https://github.com/CSSEGISandData/COVID-19", "JHU."),
          br(),
          "Data of spectimen tested from",
          a(href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html", "CDC."),
          hr(style =  " margin-top: 1.1em; margin-bottom: 1.2em;"),
          ),
              
          
          plotlyOutput("tested", height = "auto")
          
        )
      )
        
    )
        
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # # Get data -----
  dat_US <- reactiveVal()
  tests <- reactiveVal()
  withProgress(
    message = "retrieving the data...",
    {
      dat_US(get_cases())
      tests(get_tests())
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
    
    # render test plot -----
    output$tested <- renderPlotly({
        p <- plot_tested(tests())
    })
    
    
    # show_p <- reactiveVal(value = FALSE)
    # output$show_p <- reactive({show_p()})
    # outputOptions(output, "show_p", suspendWhenHidden = FALSE)

    
    # Render plot_ cases -----
    output$confirmed <- renderPlotly({
        plot_cases(dat = dat_US(), state = input$imp_state, cases = "Confirmed")
      })
  
    output$deaths <- renderPlotly({
        plot_cases(dat = isolate(dat_US()), state = input$imp_state, cases = "Deaths")
    })
    
    output$recovered <- renderPlotly({
        plot_cases(dat =  isolate(dat_US()), state = input$imp_state, cases = "Recovered")
    })
    
    
    output$updated <- renderText({
              m_date <- max(dat_US()$date)
              
              x <- paste0("Last Update: ", m_date)
              x
      
      })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)


