library(shiny)
library(shinydashboard)

library(dplyr)
library(purrr)


library(DT)
library(highcharter)
library(tidyverse)


library(RODBC)
library(odbc)
library(DBI)
library(dbplyr)
library(RMySQL)

## Connection with DBI and Mysql
con <- dbConnect(odbc(), "MySql Unicode")

## Create a lazy evaluation of the data
flights <- tbl(con, "flights")
airline <- tbl(con, "airline")
airport_db <- tbl(con, "airports")

## All flight that departed from California in 2015
Californiaflight <- flights %>% 
  filter(ORIGIN %in% c("LAX","SFO","SAN","OAK", "SJC", "SNA", "TIJ","SMF","ONT", "BUR", "FAT","SBA"))

## California cities
calflights15 <- Californiaflight %>% 
  select(2,3,4,13,14,15,17,18,19,7,23,8,10,12,17,24)%>% 
  rename(year = YEAR, month = MONTH, day = DAY_OF_MONTH, dep_time = DEP_TIME, 
         dep_delay = DEP_DELAY, dep_del15 = DEP_DEL15, arr_time = ARR_TIME, arr_delay = 8, arr_del15 = ARR_DEL15,
         carrier = 10, flight = 11, tailnum = 12, origin = 13, dest = 14, distance = 15)
####
airlines <- airline %>% 
  select(IATA_CODE, AIRLINE) %>% 
  rename(carrier = IATA_CODE, carrier_name = AIRLINE)
###
airports <- airport_db %>% 
  select(2:8) %>% 
  rename(iata_code = 1, airport_name = 2, city = 3, state = 4, country = 5, latitude = 6, longitude = 7)

## Use purr's split() and map() function to create the list
## Needed to display the name of the airline but pass it's
## Carrier code as the value
airline_list <- airlines %>% 
  collect() %>% 
  split(.$carrier_name) %>% 
  map(~.$carrier)

month_list <- as.list(1:12) %>% 
  set_names(month.name)
month_list$`All Year` <- 99

flights <- calflights15

##Creating 
ui <- dashboardPage(
  dashboardHeader(title = "California Flights Dashboard 2015", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Map", tabName = "map", icon = icon("map")),
                conditionalPanel("input.sidebarmenu == 'dashboard'",
                                 selectInput(
                                   inputId = "airline", 
                                   label = "Airline:",
                                   choices = airline_list,
                                   selected = "DL",
                                   selectize = FALSE),
                                 sidebarMenu(
                                   selectInput(
                                     inputId = "month",
                                     label = "Month:",
                                     choices = month_list,
                                     selected = 99,
                                     # size = 13,
                                     selectize = FALSE)
                                 )),
                conditionalPanel("input.sidebarmenu == 'map'",
                                 radioButtons("n", "Figures need to be shown",
                                              choices = list("Number of Flights" = 2,
                                                             "Cancel Rate" = 3,
                                                             "Delay Rate" = 4)))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              tabsetPanel(id = "tabs",
                          tabPanel(
                            title = "Main Dashboard",
                            value = "page1",
                            fluidRow(
                              valueBoxOutput("total_flights"),
                              valueBoxOutput("per_day"),
                              valueBoxOutput("percent_delayed")
                            ),
                            fluidRow(
                              
                            ),
                            fluidRow(
                              column(width = 7,
                                     p(textOutput("monthly")),
                                     highchartOutput("group_totals")),
                              column(width = 5,
                                     p("Click on an airport in the plot to see the details"),
                                     DTOutput("top_airports"))
                            )
                          )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "KPI in State Level",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("map_state", height = 600, width = 1000)
                )
              )
      )
    )
  )
)

server <- function(input, output, session){
  tab_list <- NULL
  
    db_flights <- flights %>% 
      left_join(airlines, by = "carrier") %>% 
      left_join(airports, by = c("origin" = "iata_code")) %>% 
      rename(origin_name = airport_name) %>% 
      select(-latitude, -longitude, -state, -country) %>% 
      left_join(airports, by = c("dest" = "iata_code")) %>% 
      rename(dest_name = airport_name)
  
    output$monthly <- renderText({
      if(input$month == "99")"Click on a month in the plot to see the daily counts"
    })  
    
    output$total_flights <- renderValueBox({
      # The following code runs inside the database
      result <- db_flights %>%
        filter(carrier == !!input$airline)
      
      if(!!input$month != 99) result <- filter(result, month == !!input$month)
      
      result <- result %>%
        tally() %>%
        pull() %>% 
        as.integer()%>% 
        prettyNum(big.mark = ",") %>% 
        valueBox(subtitle = "Number of Flights",
                 icon = icon("plane", lib='font-awesome'))
    })
  
  
  
  # Avg per Day (server) -----------------------------
    output$per_day <- renderValueBox({
      
      # The following code runs inside the database
      result <- db_flights %>%
        filter(carrier == !!input$airline)
      
      if(!!input$month != 99) result <- filter(result, month == !!input$month)
      result <- result %>%
        group_by(day, month) %>% 
        tally() %>% 
        ungroup() %>% 
        summarise(avg = mean(n, na.rm = TRUE)) %>% 
        pull() %>% 
        round() %>% 
        prettyNum(big.mark = ",") %>% 
        valueBox(
          subtitle = "Average Flights per day",
          color = "blue",
          icon = icon("bell", lib = 'font-awesome')
        )
      
    })
    
  ## Percent delayed (server) ---------------
    
    output$percent_delayed <- renderValueBox({
      
      # The following code runs inside the database
      result <- db_flights %>%
        filter(carrier == !!input$airline)
      
      if(!!input$month != 99) result <- filter(result, month == !!input$month)
      result <- result %>%
        filter(!is.na(dep_del15)) %>% 
        summarise(
          delays = sum(dep_del15, na.rm = TRUE),
          total = n()
        ) %>% 
        mutate(percent = (delays / total) * 100) %>% 
        pull() %>% 
        round() %>% 
        paste0("%") %>% 
        valueBox(
          subtitle = "Flights Delayed",
          color = "teal",
          icon = icon("bell-slash", lib = 'font-awesome')
        )
    })

    # Events in Highcharts can be tracked using a JavaScript. For data points in a plot, the 
    # event.point.category returns the value that is used for an additional filter, in this case
    # the month that was clicked on.  A paired observeEvent() command is activated when
    # this java script is executed
    js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
    
    output$group_totals <- renderHighchart({
      
      if(!!input$month != 99) {
        result <- db_flights %>%
          filter(month == !!input$month,
                 carrier == !!input$airline) %>%
          group_by(day) %>%
          tally() %>%
          collect()
        group_name <- "Daily"
      } else {
        result <- db_flights %>%
          filter(carrier == !!input$airline) %>%
          group_by(month) %>%
          tally() %>%
          collect()    
        group_name <- "Monthly"
      } 
      
      highchart() %>%
        hc_add_series(
          data = result$n, 
          type = "line",
          name = paste(group_name, " total flights"),
          events = list(click = js_click_line)) 
    })
    
    # Tracks the JavaScript event created by `js_click_line`
    observeEvent(input$line_clicked != "",
                 if(!! input$month == 99)
                   updateSelectInput(session, "month", selected = input$line_clicked),
                 ignoreInit = TRUE)
    js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
    
    output$top_airports <- renderDT({
      # The following code runs inside the database
      result <- db_flights %>%
        filter(carrier == !!input$airline) 
      
      if(!!input$month != 99) result <- filter(result, month == !!input$month) 
      
      result <- result %>%
        group_by(dest, dest_name) %>%
        tally() %>%
        arrange(desc(n)) %>%
        rename(`No of flights` = n) %>% 
        collect()
      
    })
}

shinyApp(ui, server)





























