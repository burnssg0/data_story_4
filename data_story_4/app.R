
################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

################################################################################
# Data Prep

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(DT)
library(lubridate)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

sewanee_rain$year <- as.numeric(sewanee_rain$year)

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))

sewanee_temp$year <- as.numeric(sewanee_temp$year)
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail


# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail


# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail
utilities %>% as.data.frame %>% pull(building) %>% unique


# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail

# Data Work ====================================================================

temps <- 
  sewanee_temp %>% 
  filter(stat == 'avg') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(date = paste(month, year)) %>% 
  mutate(date = my(date))%>% 
  mutate(mm = month(date,label = TRUE))

year_temp <-
  temps %>% 
  group_by(year) %>% 
  summarize(year_avg = mean(temp))

year_temp$year <- as.numeric(year_temp$year)

rain <- 
  sewanee_rain %>% 
  mutate(inches = as.numeric(inches)) %>% 
  mutate(date = paste(month, year)) %>%
  mutate(date = my(date)) %>% 
  mutate(mm = month(date,label = TRUE))

year_rain <- 
  rain %>% 
  group_by(year) %>% 
  summarize(year_rainavg = mean(inches))

year_rain$year <- as.numeric(year_rain$year)


################################################################################
################################################################################
################################################################################
################################################################################
ui <- fluidPage(
  titlePanel('Tempature and Rainfall Trends in Sewanee Tennesse'),
  p('Explore how tempatures and rainfall trends have changed over the years in Sewanee, TN.'),
  helpText('Us the "Over the years" tabs to look at over all trends thought the years. Use the "Compare" tabs to compare trends of specific years'),
  tabsetPanel(
    tabPanel(h5('Tempature Over the Years'),
             fluidRow(column(4, sliderInput(inputId = 'temp_years',
                                   label = 'Select Years',
                                   min = min(sewanee_temp$year, na.rm=TRUE),
                                   max = max(sewanee_temp$year, na.rm=TRUE), 
                                   value = range(sewanee_temp$year, na.rm=TRUE), 
                                   sep = ""))),
    br(),
    br(),
    fluidRow(column(1),
             column(10, plotOutput("temp_timeplot")),
             column(1)), 
    br(),
    br(),
    fluidRow(column(1),
             column(10, plotOutput("temp_timeplot_year")),
             column(1))
  ),
  tabPanel(h5('Compare Tempature Between Years'),
           fluidRow(column(5, selectInput(inputId = 'temp_years_2',
                                                 label = 'Select Years',
                                                 multiple = TRUE,
                                                 choices = unique(sewanee_temp$year),
                                                 selected = c('1958','1959','1960','2023','2022','2021'))),
                    column(4, selectInput(inputId = 'temp_months',
                                          label = 'Select Months',
                                          multiple = TRUE,
                                          choices = unique(temps$mm),
                                          selected = unique(temps$mm)))),
                    br(),
                    br(),
                    br(),
                    br(),
                    fluidRow(column(12, plotOutput("temp_yearplot")))), 
  tabPanel(h5('Rainfall Over the Years'),
           fluidRow(column(4, sliderInput(inputId = 'rain_years',
                                          label = 'Select Years',
                                          min = min(sewanee_rain$year, na.rm=TRUE),
                                          max = max(sewanee_rain$year, na.rm=TRUE), 
                                          value = range(sewanee_rain$year, na.rm=TRUE), 
                                          sep = ""))),
           br(),
           br(),
           fluidRow(column(1),
                    column(10, plotOutput("rain_timeplot")),
                    column(1)), 
           br(),
           br(),
           fluidRow(column(1),
                    column(10, plotOutput("rain_timeplot_year")),
                    column(1))
  ),
  tabPanel(h5('Compare Rainfall Between Years'),
           fluidRow(column(4, selectInput(inputId = 'rain_years_2',
                                          label = 'Select Years',
                                          multiple = TRUE,
                                          choices = unique(sewanee_rain$year),
                                          selected = c('1896','1897','1898','2021','2022','2023'))),
                    column(4, selectInput(inputId = 'rain_months',
                                          label = 'Select Months',
                                          multiple = TRUE,
                                          choices = unique(rain$mm),
                                          selected = unique(rain$mm)))),
                    br(),
                    br(),
                    br(),
                    br(),
                    fluidRow(column(12, plotOutput("rain_yearplot")))
                    
  )
  )
)

###################################################################
###################################################################
# Server

server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$temps <- temps
  rv$rain <- rain

  ##################################################
  output$temp_timeplot <- renderPlot({
    
    ggplot(rv$temps%>% filter(year >= input$temp_years[1],
                              year <= input$temp_years[2]), 
           aes(x = date, 
               y = temp))+
      geom_path() +
      geom_smooth(method = 'lm')+
      labs(title = 'Average Monthly Temperature Over Time in Sewanee',
           y='Temperature (F)',
           x = 'Date')
      
    })
  
  
  output$temp_timeplot_year <- renderPlot({
    
    ggplot(year_temp,
           aes( x = year, 
                y = year_avg))+
      geom_point()+
      geom_line()+
      xlim(input$temp_years)+
      labs(title = 'Average Yearly Temperature Over Time in Sewanee',
           y='Temperature (F)',
           x = 'Date')
    
  })
  ##################################################
  filtered_temps <- reactive({
    temps %>%
      filter(
        year %in% input$temp_years_2,
        mm %in% input$temp_months
      )
  })
  
  output$temp_yearplot <- renderPlot({
    
    ggplot(filtered_temps(),
           aes(x = mm,
               y = temp,
               group = factor(year),
               color = year)) +
      geom_line() +
      geom_point()+
      labs(title = 'Yearly Trends in Average Temperature',
           y='Temperature (F)',
           x = 'Months', 
           color = "Years")
    
  })
  
  
  ##################################################
  output$rain_timeplot <- renderPlot({
    
    ggplot(rv$rain %>% filter(year >= input$rain_years[1],
                               year <= input$rain_years[2]), 
           aes(x = date, 
               y = inches))+
      geom_path() +
      geom_smooth(method = 'lm')+
      labs(title = 'Average Monthly Rainfall Over Time in Sewanee',
           y='Rainfall (inches)',
           x = 'Date')
    
  })
  
  
  output$rain_timeplot_year <- renderPlot({
    
    ggplot(year_rain,
           aes( x = year, 
                y = year_rainavg))+
      geom_point()+
      geom_line()+
      xlim(input$rain_years)+
      labs(title = 'Average Yearly Rainfall Over Time in Sewanee',
           y='Rainfall (inches)',
           x = 'Date')
    
  })
  ##################################################
  filtered_rain <- reactive({
    rain %>%
      filter(
        year %in% input$rain_years_2,
        mm %in% input$rain_months
      )
  })
  
  output$rain_yearplot <- renderPlot({
    
    ggplot(filtered_rain(),
           aes(x = mm,
               y = inches,
               group = factor(year),
               color = year)) +
      geom_line() +
      geom_point()+
      labs(title = 'Yearly Trends in Average Rainfall',
           y='Rainfall (inches)',
           x = 'Months', 
           color = "Years")
    
  })
  
  
  ##################################################
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


