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
library(shiny.i18n)
library(showtext)
library(waiter)

showtext_auto()

i18n < -Translator$new(translation_json_path = "./translation.json")
i18n$set_translation_language("en")

translator < -
    Translator$new(translation_json_path = "./translation.json")




test_19_covid_Confirmed < -
    reshape2::melt(
        time_series_19_covid_Confirmed,
        id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", ,
        value.name = "Headcount_Confirmed"
    )
test_19_covid_Recovered < -
    reshape2::melt(
        time_series_19_covid_Recovered,
        id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", ,
        value.name = "Headcount_Recovered"
    )
test_19_covid_Deaths < -
    reshape2::melt(
        time_series_19_covid_Deaths,
        id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", ,
        value.name = "Headcount_Deaths"
    )


test_19_covid_Confirmed$date < -
    lubridate::parse_date_time(sub("X", "", test_19_covid_Confirmed$Calendar_Date), orders = "mdy")

test_19_covid_Recovered$date < -
    lubridate::parse_date_time(sub("X", "", test_19_covid_Recovered$Calendar_Date), orders = "mdy")

test_19_covid_Deaths$date < -
    lubridate::parse_date_time(sub("X", "", test_19_covid_Deaths$Calendar_Date), orders = "mdy")



all_19_covid0 < -
    merge(
        test_19_covid_Confirmed,
        test_19_covid_Recovered,
        by = c(
            "Province.State",
            "Country.Region",
            "Lat",
            "Long",
            "Calendar_Date"
        )
    )
all_19_covid < -
    merge(
        all_19_covid0,
        test_19_covid_Deaths,
        by = c(
            "Province.State",
            "Country.Region",
            "Lat",
            "Long",
            "Calendar_Date"
        )
    )


all_19_covid$Headcount_active < -
    all_19_covid$Headcount_Confirmed - all_19_covid$Headcount_Recovered - all_19_covid$Headcount_Deaths


all_19_covid[is.na(all_19_covid)] < -0



header < -dashboardHeader(title = i18n$t("Covid-19 Tracker") #, tags$li(class = "dropdown", radioButtons(inputId = "language", label = "Select Language",
    # choices = c("en" = "en", "cn" = "cn"),
    # selected = "en")))




sidebar < -dashboardSidebar(sidebarMenu(
    menuItem(
        i18n$t("Dashboard"),
        tabName = "dashboard",
        icon = icon("dashboard")
    ),
    menuItem(
        i18n$t("Contact-me"),
        icon = icon("barcode ", lib = 'glyphicon'),
        href = "http://z-ing.net"
    )
))




frow1 < -fluidRow(uiOutput('page_content'),
    #column(3, radioButtons(inputId = "language", label = "Select Language",
        # choices = c("English" = "en", "Chinese" = "cn"),
        # selected = "en")),



    column(
        3,
        dateInput(
            i18n$t('date'),
            label = i18n$t('Date input: yyyy-mm-dd'),
            value = max(all_19_covid$date),
            min = min(all_19_covid$date),
            max = max(all_19_covid$date)
        )
    ))


frow0 < -fluidRow(
    # A static valueBox valueBox(
        floor(all_19_covid % > %
            filter(date.x >= max(date.x)) % > %
            summarise(sum(
                Headcount_Confirmed
            ))),
        i18n$t("Total Confirmed"),
        icon = icon("ambulance"),
        color = "yellow",
        width = 3
    ),

    valueBox(
        floor(all_19_covid % > %
            filter(date.x >= max(date.x)) % > %
            summarise(sum(Headcount_active)))

        ,
        i18n$t("Currently Active"),
        icon = icon("hospital"),
        color = "red",
        width = 3
    ),

    valueBox(
        all_19_covid % > %
        filter(date >= max(date)) % > %
        summarise(sum(Headcount_Recovered)),
        i18n$t("Total Recovered"),
        icon = icon("first-aid"),
        color = "green",
        width = 3
    ),

    valueBox(
        floor(all_19_covid % > %
            filter(date.x >= max(date.x)) % > %
            summarise(sum(Headcount_Deaths))),
        i18n$t("Total Deaths"),
        icon = icon("diagnoses"),
        color = "black",
        width = 3
    )
)


#frow1111 < -fluidRow(
    # valueBoxOutput("value1") #, valueBoxOutput("value2") #, valueBoxOutput("value3") #)


frow11 < -fluidRow(
    box(
        title = i18n$t("Cumulative COVID-19 number"),
        color = "blue",
        ribbon = TRUE,
        collapsible = TRUE,
        #width = 8,
        plotOutput("Cumulative_COVID_19", height = "300px")
    ),
    box(
        title = i18n$t("COVID-19 time series"),
        color = "blue",
        ribbon = TRUE,
        collapsible = TRUE,
        # width = 8,
        plotOutput("COVID_19_timeseries", height = "300px")
    ),
    box(
        title = ("COVID-19 time series Worldwide & Mainland China"),
        color = "blue",
        ribbon = TRUE,
        collapsible = TRUE,
        #width = 8,
        plotOutput("COVID_19_timeseries_World_China", height = "300px")
    ),
    box(
        title = i18n$t("COVID-19 time series Worldwide & Mainland China(logarithmic)"),
        color = "blue",
        ribbon = TRUE,
        collapsible = TRUE,
        #width = 8,
        plotOutput("COVID_19_timeseries_World_China_log", height = "300px")
    )
)
#frow1_1 < -fluidRow(
    # column(4, verbatimTextOutput("range")) #)

frow2 < -fluidRow(# box(
    # title = "Test"
    #, status = "primary"
    #, solidHeader = TRUE #, collapsible = TRUE #, plotOutput("test", height = "1500px", width = 4) #) plotOutput("test", height = "500px"))

frow3 < -fluidRow(# box(
    # title = i18n$t("WHO Timeline - COVID-19"),
    # color = "blue",
    # ribbon = TRUE,
    # collapsible = TRUE,
    # width = 10,
    plotOutput("WHOTimeline", height = "500px") #))



body < -dashboardBody(tags$head(
        tags$meta(name = "viewport", content = "width=1600"),
        uiOu tput("body")
    ),
    frow1,
    frow0,
    frow2,
    frow3,
    frow11)

ui < -
    dashboardPage(
        title = i18n$t(
            'Real time dashboard for tracking the current Global Covid-19 Epidemic'
        ),
        header,
        sidebar,
        body,
        skin = 'red'
    )
