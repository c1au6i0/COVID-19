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
          
            uiOutput("body_UI")
            # plotlyOutput("cases", height = "400px")
        ),
        
        box(
          h2("Total Number of Specimens Tested in U.S."),
          h4( "Data of Confirmed, Deaths and Recovered from", 
          a(href="https://github.com/CSSEGISandData/COVID-19", "JHU."),
          br(),
          "Data of spectimen tested from",
          a(href="https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html", "CDC."),
          hr(style =  " margin-top: 1.1em; margin-bottom: 1.2em;")
          ),
              
          
          plotlyOutput("tested", height = "400px")
          
        )
      )
        
    )
        
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # # Get data -----
  
  withProgress(
    message = "retrieving the data...",
    {
      dat_US <-  get_cases()
      tests <- get_tests()
    })

  
  dat_f <- reactiveVal(NULL)
 
  dat_f <- reactive({
     x <- input$imp_state
     dat_US %>%
       filter(state %in% x)
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
        p <- plot_tested(tests)
    })
    
    
    # show_p <- reactiveVal(value = FALSE)
    # output$show_p <- reactive({show_p()})
    # outputOptions(output, "show_p", suspendWhenHidden = FALSE)

    
    # Render plot_ cases -----
    output$cases <- renderPlotly({
        plot_cases(dat = dat_f())
      })
  
  output$body_UI <- renderUI ({
    n_cond <- length(unique(dat_f()$condition))
    
    if(n_cond == 1) {
      p <- plotlyOutput("cases", height = "400px")
    }

  if(n_cond == 2){
    p <- plotlyOutput("cases", height = "800px")
  }

  if(n_cond == 3){
    p <- plotlyOutput("cases", height = "1200px")
  }
    p
  })
  
    

  
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# https://stackoverflow.com/questions/38826893/shinydashboard-does-not-work-with-uioutput
