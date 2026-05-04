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
################################################################################
################################################################################
################################################################################
# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

#sewanee_rain %>% View

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

#sewanee_temp %>% View

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

#split_creek %>% View

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail
utilities %>% as.data.frame %>% pull(building) %>% unique

#utilities %>% View

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail

#fall2025 %>% View

################################################################################
################################################################################
################################################################################

# Avg Temps Over the Years By Month Average 

temps <- 
  sewanee_temp %>% 
  filter(stat == 'avg') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(date = paste(month, year)) %>% 
  mutate(date = my(date))

ggplot(temps, 
       aes(x = date, 
           y = temp))+
  geom_path() +
  geom_smooth(method = 'lm')

# Ang Temps over the years by Year Avg 


year_temp <-
  temps %>% 
  group_by(year) %>% 
  summarize(year_avg = mean(temp))

year_temp$year <- as.numeric(year_temp$year)

ggplot(year_temp,
       aes( x = year, 
            y = year_avg))+
  geom_point()+
  geom_line()

  
# Rain Over the Years

rain <- 
  sewanee_rain %>% 
  mutate(inches = as.numeric(inches)) %>% 
  mutate(date = paste(month, year)) %>%
  mutate(date = my(date))


ggplot(rain, 
       aes(x = date, 
           y = inches)) +
geom_path() +
  geom_smooth(method = 'lm')

# Avg rain over the years by Year Avg 

year_rain <- 
  rain %>% 
  group_by(year) %>% 
  summarize(year_rainavg = mean(inches))
  
year_rain$year <- as.numeric(year_rain$year)

ggplot(year_rain,
       aes( x = year, 
            y = year_rainavg))+
  geom_point()+
  geom_line()




################################################################################
################################################################################
################################################################################
# compare rain between years 
com_rain <- 
  sewanee_rain %>% 
  mutate(inches = as.numeric(inches)) %>% 
  mutate(date = paste(month, year)) %>%
  mutate(date = my(date)) %>% 
  mutate(mm = month(date,label = TRUE))




ggplot(com_rain, 
       aes(x= mm, 
           y = inches, 
           group = year, 
           color = factor(year))) + 
  geom_path()

# compare temps between years 
com_temps <- 
  sewanee_temp %>% 
  filter(stat == 'avg') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(date = paste(month, year)) %>% 
  mutate(date = my(date))%>% 
  mutate(mm = month(date,label = TRUE))


ggplot(com_temps, 
       aes(x= mm, 
           y = temp, 
           group = year, 
           color = factor(year))) + 
  geom_path()




























