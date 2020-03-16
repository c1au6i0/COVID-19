library(DT)
library(ggiraph)
library(ggridges)
library(rvest)
library(htmlwidgets)
library(janitor)
library(leaflet)
library(plotly)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(scales)
library(vroom)




# Get data ------
states <- c("Washington", "New York", "California", "Massachusetts", "Georgia", "Colorado", "Florida", "New Jersey",
            "Oregon", "Texas", "Illinois", "Pennsylvania", "Iowa", "Maryland",
            "North Carolina", "South Carolina", "Tennessee", "Virginia",
            "Arizona", "Indiana", "Kentucky", "District of Columbia", "Nevada",
            "New Hampshire", "Minnesota", "Nebraska", "Ohio", "Rhode Island",
            "Wisconsin", "Connecticut", "Hawaii", "Oklahoma", "Utah")
get_cases <- function(){
  
  COVID <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-"
  dat <- map_dfr(list("Confirmed", "Deaths", "Recovered"), function(x){
    
    url_dat <- paste(COVID, x, ".csv", sep ="")
    dat <- vroom(file = url_dat)
    dat <- add_column(dat, condition = x, .before = 1)
    dat
  })
  
  # Clean data -----
  states <- c("Washington", "New York", "California", "Massachusetts", "Diamond Princess",
              "Grand Princess", "Georgia", "Colorado", "Florida", "New Jersey",
              "Oregon", "Texas", "Illinois", "Pennsylvania", "Iowa", "Maryland",
              "North Carolina", "South Carolina", "Tennessee", "Virginia",
              "Arizona", "Indiana", "Kentucky", "District of Columbia", "Nevada",
              "New Hampshire", "Minnesota", "Nebraska", "Ohio", "Rhode Island",
              "Wisconsin", "Connecticut", "Hawaii", "Oklahoma", "Utah")
  
  dat_US <-  dat %>%
    rename(state = `Province/State`, country = `Country/Region`) %>%
    filter(state %in% states ) %>%
    pivot_longer(6: ncol(dat), names_to = "date", values_to = "cases") %>%
    mutate(date  = lubridate::mdy(date))
  
  dat_US[dat_US == 0] <- NA
  
  dat_US <-   na.omit(dat_US)
  
  # tot by day in US
  all_US <- dat_US %>%
    group_by(condition, date) %>%
    summarize(cases = sum(cases, na.rm = TRUE)) %>%
    mutate(state = "US", country = "US", Lat = 37.0902, Long = 95.7129)
  
  all_US <- all_US[, names(dat_US)]
  
  dat_US <- bind_rows(dat_US, all_US)
  
  dat_US
}

get_tests <- function(){
  # Get number of tests -----
  cdc_page <- xml2::read_html("https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html")
  tab_cdc <- cdc_page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table() 
  
  tests <-  tab_cdc [[1]] %>% 
    data.frame() %>% 
    clean_names() %>% 
    mutate(date_collected = lubridate::mdy(paste0(date_collected, "/2020"))) %>% 
    pivot_longer(2:3, names_to = "lab_type", values_to = "n") %>% 
    mutate(n = as.numeric(n)) %>% 
    na.omit() 
  tests
}


# Plot tests -----

plot_tested <- function(dat = tests) {
    tests <- dat %>% 
      group_by(date_collected) %>% 
      summarize(d_test = sum(n))
    
    tests[, "tot"] <- cumsum(tests$d_test)
    
    p <- tests %>% 
      ggplot(aes(x = date_collected, y = tot), color = "black") +
      geom_line(show.legend = FALSE, size = 0.2) +
      geom_point(size = 2, stroke = 0.2, fill = "#999999", shape = 21) +
      labs(x ="", y = "", 
           title = "Total number of specimens tested in U.S.") +
      scale_x_date(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous(limits = c(0, ceiling(max(tests$tot)/5000) *5000)) +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "top") 
    p <- ggplotly(p) 
    p
}

# Plot infected-----

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot_cases <- function(dat , state = c("Maryland", "California")){
  # state <- sym(state)
    dat_f <- dat %>%
      filter(state %in%  !!state) 
    
    # shapes of symbols
    shapes_l <- 21:24
    shapes_plot <- shapes_l[1:length(unique(dat_f$state))]
    
    p <- dat_f %>% 
      ggplot(aes(x = date, y = cases, fill = state, shape = state),color = "black") +
      geom_line(show.legend = FALSE, size = 0.2) +
      geom_point(size = 2, stroke = 0.2) +
      scale_x_date(guide = guide_axis(n.dodge = 2)) +
      scale_shape_manual(values = shapes_plot) +
      scale_fill_manual(values = cbPalette) +
      scale_alpha(guide = 'none') +
      labs(x ="", y = "",
           title = "Total number of cases") +
      facet_wrap(vars(condition), ncol =  1, scales = "free_y") +
      scale_y_continuous(limits = c(0, NA),breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "top",
            strip.background = element_rect(fill="grey90"))
    
    p <- ggplotly(p) %>%
      layout(legend = list(
        orientation = "h"
      ))
    p
}


