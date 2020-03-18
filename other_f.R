library(DT)
library(ggiraph)
library(ggridges)
library(rvest)
library(htmltools)
library(htmlwidgets)
library(janitor)
# library(leaflet)
library(lemon)
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

states_abr <- vroom::vroom("other/states.csv")
states <- states_abr$state

get_cases <- function() {
  COVID <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-"
  dat <- map_dfr(list("Confirmed", "Deaths", "Recovered"), function(x) {
    url_dat <- paste(COVID, x, ".csv", sep = "")
    dat <- vroom(file = url_dat)
    dat <- add_column(dat, condition = x, .before = 1)
    dat
  })

  # Clean data -
  dat_US <- dat %>%
    rename(state = `Province/State`, country = `Country/Region`) %>%
    filter(country %in% "US") %>%
    pivot_longer(6:ncol(dat), names_to = "date", values_to = "cases") %>%
    mutate(date = lubridate::mdy(date))

  dat_US[dat_US == 0] <- NA

  dat_US <- na.omit(dat_US)

  # tot by day in US
  all_US <- dat_US %>%
    group_by(condition, date) %>%
    summarize(cases = sum(cases, na.rm = TRUE)) %>%
    mutate(state = "US", country = "US", Lat = 38.9, Long = -77.00)
  

  all_US <- all_US[, names(dat_US)]

  dat_US <- bind_rows(dat_US, all_US)

  dat_US
}

get_tests <- function() {
  # Get number of tests -----
  cdc_page <- xml2::read_html("https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/testing-in-us.html")
  tab_cdc <- cdc_page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table()

  tests <- tab_cdc [[1]] %>%
    data.frame() %>%
    clean_names() %>%
    mutate(date_collected = lubridate::mdy(paste0(date_collected, "/2020"))) %>%
    pivot_longer(2:3, names_to = "lab_type", values_to = "n") %>%
    mutate(n = as.numeric(n)) %>%
    na.omit()
  tests
}


# Plot tests -----

plot_tested <- function(dat = tests, log_scale = FALSE) {
  tests <- dat %>%
    group_by(date_collected) %>%
    summarize(d_test = sum(n))

  tests[, "tot"] <- cumsum(tests$d_test)

  tests <- tests %>%
    separate(date_collected, sep = -5, into = c("y", "m_d")) %>%
    mutate(group = 1)

  n_date <- length(unique(tests$m_d))

  breaks <- unique(tests$m_d)[seq(1, n_date, ceiling(n_date / 5))]

  p <- tests %>%
    rename(date = m_d) %>%
    ggplot(aes(x = date, y = tot, group = group), color = "black") +
    geom_line(show.legend = FALSE, size = 0.2) +
    geom_point(size = 2, stroke = 0.2, fill = "#999999", shape = 21) +
    labs(x = "", y = "") +
    scale_x_discrete(breaks = breaks) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(hjust = 1)
    )
  if (log_scale == FALSE) {
    p <- p + scale_y_continuous(limits = c(0, ceiling(max(tests$tot) / 5000) * 5000))
  } else {
    p <- p + scale_y_continuous(trans = "log2", limits = function(x) c(2, ceiling(sqrt(max(x)))^2))
  }

  p <- ggplotly(p, tooltip = c("y", "x", "shape")) %>%
    layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    )

  p <- p %>%
    config(p = ., displayModeBar = FALSE)
  p
}




# Plot infected-----

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot_cases <- function(dat, log_scale = FALSE) {
  if (nrow(dat) > 0) {
    # shapes of symbols
    shapes_l <- 21:24
    shapes_plot <- shapes_l[1:length(unique(dat$state))]

    dat_f <- dat %>%
      arrange(date) %>%
      separate(date, sep = -5, into = c("y", "m_d"))

    n_date <- length(unique(dat_f$m_d))

    val <- unique(dat_f$m_d)

    breaks <- unique(dat_f$m_d)[seq(1, n_date, ceiling(n_date / 5))]

    p <- dat_f %>%
      rename(date = m_d) %>%
      ggplot(aes(x = date, y = cases, fill = state, shape = state, group = state), color = "black") +
      geom_point(size = 2, stroke = 0.2) +
      geom_line(show.legend = FALSE, size = 0.2) +
      scale_shape_manual(values = shapes_plot) +
      scale_fill_manual(values = cbPalette) +
      scale_alpha(guide = "none") +
      labs(x = "", y = "") +
      facet_wrap(vars(condition), ncol = 1, scales = "free") +
      scale_x_discrete(breaks = breaks, limits = val) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(hjust = 1)
      )

    if (log_scale == FALSE) {
      p_scale <- p + scale_y_continuous(limits = c(0, NA), breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    } else {
      p_scale <- p + scale_y_continuous(trans = "log2", limits = function(x) c(2, ceiling(sqrt(max(x)))^2))
    }

    p_plotly <- ggplotly(p_scale, tooltip = c("y", "x", "shape")) %>%
      layout(
        legend = list(orientation = "h"),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE),
        xaxis2 = list(fixedrange = TRUE),
        yaxis2 = list(fixedrange = TRUE),
        xaxis3 = list(fixedrange = TRUE),
        yaxis3 = list(fixedrange = TRUE)
      ) %>%
      config(displayModeBar = FALSE)
  }

  if (nrow(dat) == 0) {
    df <- data.frame()

    p <- ggplot(df) +
      geom_point() +
      theme_void() +
      annotate("text", x = 1, y = 1, label = "No Case Reported")

    p_plotly <- ggplotly(p) %>%
      config(displayModeBar = FALSE)
  }

  p_plotly
}


# MAP ------

# state_abb -----
# 
# map_leaf <- function(dat, sel = NULL) {
#     if (nrow(dat) != 0) {
#         # THIS IS NOT NECESSARY YET but might when we get more info on the locations of cases
#         # # get the state abbreviation from the dataframe 
#         # st_abbr <- states_abr[states_abr$state == sel, "abbr"]
#         # 
#         # # subset dat based on the abbr. This is to avaoid to take twice the same data.
#         # # If there are not provinces than takes the name
#         # 
#         # # create pattern depending on the number of selection
#         # if (length(st_abbr) == 1) {
#         #   pattern <- paste0(st_abbr, "$")
#         # } else{
#         #   pattern <- paste0("(",
#         #          paste(sel, collapse = "|"), 
#         #          ")$", 
#         #          sep = "")
#         # }
#     
#     dat <- dat %>%
#       group_by(condition, state, country, Lat, Long) %>% 
#       summarise(max = max(cases))
#     
#     # Probably not the most elegant solution
#     dat <- dat[rep(seq_len(nrow(dat)), dat$max), ]
#     
#        p <-  dat %>% 
#           # filter(str_detect(state, pattern)) %>% 
#           leaflet() %>%
#           addTiles(options = providerTileOptions(maxZoom = 7, zoomControl = FALSE)) %>%
#           addCircleMarkers(~Long, ~Lat, label = ~ htmlEscape(state), radius = 2, clusterOptions = "yes")
# 
#   p
#     }
# }
# 
