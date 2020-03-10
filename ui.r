######################################################################################
######################################################################################
###                                                                                ###
###                                     TITLE:                                     ###
###                                     R CODE                                     ###
###                        AUTHOR: 政 ZHUZHENG(IVERSON) ZHOU                        ###
###                                DATE: 2020-03-10                                ###
###                                    VERSION 1                                   ###
###  TOPIC: REAL TIME DASHBOARD FOR TRACKING THE CURRENT GLOBAL COVID-19 EPIDEMIC  ###
###                 SOURCE: 19 COVID DATABASE FROM VARIOUS SOURCES                 ###
###                                                                                ###
######################################################################################
######################################################################################

library(readr)
library(reshape2)
library(lubridate)
library(rgdal)
library(shiny)
library(shinydashboard)
library(maptools)
library(ggplot2)
library(ggthemes)
library(rgeos)
library(dplyr)
library(repmis)
library(scatterpie)
library(RColorBrewer)



test_19_covid_Confirmed<-reshape2::melt(time_series_19_covid_Confirmed,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                        variable.name = "Calendar_Date", 
                                        ,value.name = "Headcount_Confirmed"
)
test_19_covid_Recovered<-reshape2::melt(time_series_19_covid_Recovered,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                        variable.name = "Calendar_Date", 
                                        ,value.name = "Headcount_Recovered"
)
test_19_covid_Deaths<-reshape2::melt(time_series_19_covid_Deaths,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                     variable.name = "Calendar_Date", 
                                     ,value.name = "Headcount_Deaths"
)


test_19_covid_Confirmed$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Confirmed$Calendar_Date), orders = "mdy")

test_19_covid_Recovered$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Recovered$Calendar_Date), orders = "mdy")

test_19_covid_Deaths$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Deaths$Calendar_Date), orders = "mdy")



all_19_covid0<- merge(test_19_covid_Confirmed,test_19_covid_Recovered,by=c("Province.State","Country.Region","Lat","Long","Calendar_Date"))
all_19_covid<- merge(all_19_covid0,test_19_covid_Deaths,by=c("Province.State","Country.Region","Lat","Long","Calendar_Date"))


all_19_covid$Headcount_active<-all_19_covid$Headcount_Confirmed-all_19_covid$Headcount_Recovered-all_19_covid$Headcount_Deaths




header <- dashboardHeader(title = "Real time dashboard for tracking the current Global Covid-19 Epidemic")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Contact-me", icon = icon("barcode ",lib='glyphicon'), 
             href = "http://z-ing.net")
  )
)




frow1 <- fluidRow(
  #  column(3,
  #         dateRangeInput("dateRange", h3("Filter Earthquake Events by date")
  #                       , start = min(quakes_master$date), end =  max(quakes_master$date)
  #                       , min = NULL,max = NULL
  #                       , format = "yyyy-mm-dd",
  #                        autoclose = TRUE
  #          )
  #         )
  
  column(3, 
    dateInput('date',
              label = 'Date input: yyyy-mm-dd',
              value = max(all_19_covid$date)
    )
  )
    # column(3, 
    #        numericInput("mag", 
    #                     h3("Input earthquake magnitude scale"), 
    #                     value = 5))   
    
  # , 
  #  sidebarPanel(sliderInput( "magslider", h3("Select earthquake magnitude scale")
  #                            ,min = 1,max= 10,value = c(5, 9) ))
    
    #valueBoxOutput("value1")
    # ,valueBoxOutput("value2")
    
  )
  
  frow11 <- fluidRow(
    box(
      title = "Cumulative COVID-19 number",
      color = "blue",
      ribbon = TRUE,
      collapsible = TRUE,
      #width = 8,
      plotOutput("Cumulative_COVID_19", height = "300px")
    ),
    box(
      title = "COVID-19 time series",
      color = "blue",
      ribbon = TRUE,
      collapsible = TRUE,
     # width = 8,
      plotOutput("COVID_19_timeseries", height = "300px")
    ),
    box(
      title = "COVID-19 time series Worldwide & Mainland China",
      color = "blue",
      ribbon = TRUE,
      collapsible = TRUE,
      #width = 8,
      plotOutput("COVID_19_timeseries_World_China", height = "300px")
    ),
    box(
      title = "COVID-19 time series Worldwide & Mainland China(logarithmic)",
      color = "blue",
      ribbon = TRUE,
      collapsible = TRUE,
      #width = 8,
      plotOutput("COVID_19_timeseries_World_China_log", height = "300px")
    )
  )
  #frow1_1<-  fluidRow(
  #  column(4, verbatimTextOutput("range"))
  #)
  
  frow2 <- fluidRow(
    
    #  box(
    #    title = "Test"
    #    ,status = "primary"
    #    ,solidHeader = TRUE 
    #    ,collapsible = TRUE 
    #    ,plotOutput("test", height = "1500px",width = 4)
    #  )
    plotOutput("test",  height = "1000px")
  )
  
  
  
  
  body <- dashboardBody(frow1,frow2,frow11
  )
  
  ui <- dashboardPage(title = 'Real time dashboard for tracking the current Global Covid-19 Epidemic', header, sidebar, body, skin='red')
  
  
