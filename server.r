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


shinyServer(function(input, output) {




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
    library(dplyr)
    showtext_auto()



    translator < -Translator$new(translation_json_path = "./translation.json")




    test_19_covid_Confirmed < -reshape2::melt(time_series_19_covid_Confirmed, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", value.name = "Headcount_Confirmed"
    )
    test_19_covid_Recovered < -reshape2::melt(time_series_19_covid_Recovered, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", value.name = "Headcount_Recovered"
    )
    test_19_covid_Deaths < -reshape2::melt(time_series_19_covid_Deaths, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date", value.name = "Headcount_Deaths"
    )


    test_19_covid_Confirmed$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Confirmed$Calendar_Date), orders = "mdy")

    test_19_covid_Recovered$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Recovered$Calendar_Date), orders = "mdy")

    test_19_covid_Deaths$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Deaths$Calendar_Date), orders = "mdy")



    all_19_covid0 < -merge(test_19_covid_Confirmed, test_19_covid_Recovered, by = c("Province.State", "Country.Region", "Lat", "Long", "Calendar_Date"))
    all_19_covid < -merge(all_19_covid0, test_19_covid_Deaths, by = c("Province.State", "Country.Region", "Lat", "Long", "Calendar_Date"))


    all_19_covid$Headcount_active < -all_19_covid$Headcount_Confirmed - all_19_covid$Headcount_Recovered - all_19_covid$Headcount_Deaths




    test_19_covid_Confirmed_bar < -reshape2::melt(time_series_19_covid_Confirmed, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date",
        value.name = "Headcount"
    )

    test_19_covid_Recovered_bar < -reshape2::melt(time_series_19_covid_Recovered, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date",
        value.name = "Headcount"
    )

    test_19_covid_Deaths_bar < -reshape2::melt(time_series_19_covid_Deaths, id.vars = c("Province.State", "Country.Region", "Lat", "Long"),
        variable.name = "Calendar_Date",
        value.name = "Headcount"
    )

    test_19_covid_Confirmed_bar$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Confirmed_bar$Calendar_Date), orders = "mdy")

    test_19_covid_Recovered_bar$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Recovered_bar$Calendar_Date), orders = "mdy")

    test_19_covid_Deaths_bar$date < -lubridate::parse_date_time(sub("X", "", test_19_covid_Deaths_bar$Calendar_Date), orders = "mdy")

    test_19_covid_Confirmed_bar$Type < -'Confirmed'
    test_19_covid_Recovered_bar$Type < -'Recovered'
    test_19_covid_Deaths_bar$Type < -'Deaths'

    test_19_covid_Active_bar < -all_19_covid[, c(1: 5, 12, 7)]
    test_19_covid_Active_bar$Type < -'Active'

    names(test_19_covid_Active_bar)[6] < -"Headcount"
    names(test_19_covid_Active_bar)[7] < -"date"

    #all_19_covid_bar < -rbind(test_19_covid_Confirmed_bar, test_19_covid_Recovered_bar, test_19_covid_Deaths_bar, test_19_covid_Active_bar)


    world < -readOGR("countries.geo.json", encoding = "OGRGeoJSON", stringsAsFactors = FALSE)
    #world_map < -fortify(world)
    world_map < -as.data.frame(merge(fortify(world), as.data.frame(world), by.x = "id", by.y = 0))
    #world < -gBuffer(world, byid = TRUE, width = 0)
    #world < -nowrapRecenter(world)

    #test2 < -as.data.frame(world_map)
    #test2[which(test2$long == 33.80340), ]



    # Return the formula text
    for printing as a caption-- --
    output$caption < -renderText({
        formulaText()
    })


    output$page_content < -renderUI({
        column(3,
            selectInput('selected_language',
                i18n() $t("Change language"),
                choices = translator$languages,
                selected = input$selected_language)

        )

    })




    #output$value1 < -renderValueBox({

        #all_19_covid_today < -all_19_covid % > % filter(
            # date.x >= input$date[1] & date.x <= input$date[1])

        # valueBox(
            # formatC(all_19_covid_today$Headcount_Confirmed, format = "d", big.mark = ',') #, paste('Total Confirmed') # #, icon = icon("stats", lib = 'glyphicon') #, color = "purple")


        #
    })

    output$test < -renderPlot({


        all_19_covid_today < -all_19_covid % > % filter(
            date.x >= input$date[1] & date.x <= input$date[1])


        gg < -ggplot()
        gg < -gg + geom_map(data = world_map, map = world_map,
            aes(x = long, y = lat, map_id = id),
            color = "white", size = 0.15, fill = "#d8d8d6")
        # #gg < -gg +
        # # geom_point(data = test_19_covid_Confirmed, aes(x = Long, y = Lat), color = "#e60000")
        #gg < -gg + geom_text(data = test_19_covid_Confirmed, aes(x = Long, y = Lat,
            # label = Country.Region, hjust = 0, vjust = 0.5), check_overlap = TRUE)
        gg < -gg + geom_point(data = subset(all_19_covid_today, Headcount_Confirmed > 0),
            aes(x = Long, y = Lat, size = Headcount_Confirmed),
            shape = 8 # #alpha = 1 / 3, color = "red"
            # "#d47e5d", fill = "#00000000"
        )
        # gg < -gg + scale_fill_gradient2()
        gg < -gg + geom_point(data = subset(all_19_covid_today, Headcount_Confirmed > 5000),
            aes(x = Long, y = Lat, size = Headcount_Confirmed),
            shape = 1, alpha = 1, color = "black", fill = "#00000000")


        gg < -gg + geom_scatterpie(data = subset(all_19_covid_today, Headcount_Confirmed > 500),
            aes(x = Long, y = Lat, r = sqrt(Headcount_Confirmed) / 50),
            cols = c("Headcount_active", "Headcount_Recovered", "Headcount_Deaths"),
            alpha = 0.5) +
        scale_fill_manual(
            breaks = c("Headcount_active", "Headcount_Recovered", "Headcount_Deaths"),
            labels = c(i18n() $t("Active"), i18n() $t("Recovered"), i18n() $t("Deaths")),
            values = c("Headcount_Recovered" = "orange",
                # "white" = "white",
                "Headcount_Deaths" = "black", "Headcount_active" = "cyan"
            )
        )

        # gg < -gg + ggtitle("Real time dashboard for tracking the current Global Covid-19 Epidemic")
        gg < -gg + labs(title = i18n() $t("Real time dashboard for tracking the current Global Covid-19 Pandemic"),
            subtitle = i18n() $t("19 Covid Worldwide Cases Breakdown"),
            caption = i18n() $t("Source: 19 Covid database from various sources"))
        gg < -gg + scale_size(name = "Confirmed Cases"
            #, trans = 'identity', breaks = c(500, 1000, 10000, 20000, 30000, 50000, 70000, 80000, 100000, 200000),
            labels = c("500", "1000", "10000", "20000", "30000", "50000", "70000", "80000", "100000", "200000") # #, range = c(10000, 70000), guide = "legend")

        gg < -gg + coord_quickmap()
        gg < -gg + theme_map()
        gg < -gg + theme(legend.position = c(0.96, 0.02), legend.justification = c(1, 0) #, panel.grid = element_blank(),
            # panel.border = element_blank(),
            # axis.title = element_blank(),
            # axis.text = element_blank(),
            # axis.ticks = element_blank()
        )
        #gg < -gg + theme(legend.position = c(0.05, 0.99))
        gg < -gg + theme(legend.direction = "horizontal"
            #, legend.position = "bottom", plot.title = element_text(size = 18, face = "bold"))
        gg < -gg + theme(legend.key = element_rect(color = "#00000000"))
        gg


    })




    output$WHOTimeline < -renderPlot({

        WHO_timeline < -read.csv("https://raw.githubusercontent.com/Z-ingdotnet/COVID19_Dashboard/master/WHO%20timeline.csv")

        status_levels < -c("Minor", "Significant", "Severe", "Emergency")
        status_colors < -c("#0070C0", "#00B050", "#FFC000", "#C00000")

        df < -WHO_timeline

        df < -df % > %
        mutate(date = dmy(date))


        df$status < -factor(df$status, levels = status_levels, ordered = TRUE)

        positions < -c(0.3, -0.3, 0.8, -0.8, 0.15, -0.15)
        directions < -c(1, -1)

        line_pos < -data.frame(
            "date" = unique(df$date),
            "position" = rep(positions, length.out = length(unique(df$date))),
            "direction" = rep(directions, length.out = length(unique(df$date)))
        )

        df < -merge(x = df, y = line_pos, by = "date", all = TRUE)
        df < -df[with(df, order(date, status)), ]

        text_offset < -0.1

        df$month_count < -ave(df$date == df$date, df$date, FUN = cumsum)
        df$text_position < -(df$month_count * text_offset * df$direction) + df$position


        month_buffer < -2

        month_date_range < -seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by = 'month')
        month_format < -format(month_date_range, '%b')
        month_df < -data.frame(month_date_range, month_format)


        year_date_range < -seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by = 'year')
        year_date_range < -as.Date(
            intersect(
                ceiling_date(year_date_range, unit = "year"),
                floor_date(year_date_range, unit = "year")
            ), origin = "1970-01-01"
        )
        year_format < -format(year_date_range, '%Y')
        year_df < -data.frame(year_date_range, year_format)


        timeline_plot < -ggplot(df, aes(x = as.Date(date), y = 0, col = status, label = event))
        timeline_plot < -timeline_plot + labs(col = "WHO Event")
        timeline_plot < -timeline_plot + scale_color_manual(values = status_colors, labels = status_levels, drop = FALSE)
        timeline_plot < -timeline_plot + theme_classic()
        timeline_plot < -timeline_plot + scale_x_date(breaks = '2 week')


        # Plot horizontal black line
        for timeline
        timeline_plot < -timeline_plot + geom_hline(yintercept = 0,
            color = "black", size = 0.3)

        # Plot vertical segment lines
        for event
        timeline_plot < -timeline_plot + geom_segment(data = df[df$month_count == 1, ], aes(y = position, yend = 0, xend = as.Date(date)), color = 'black', size = 0.2)



        # Plot scatter points at zero and date
        timeline_plot < -timeline_plot + geom_point(aes(y = 0), size = 3)


        # Don 't show axes, appropriately position legend
        timeline_plot < -timeline_plot + theme(axis.line.y = element_blank(),
            # axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            # axis.ticks.y = element_blank(),
            #axis.text.y = element_text(angle = 90, hjust = 1),
            # axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            plot.title = element_text(size = 18, face = "bold"),
            legend.position = "bottom"

        )

        timeline_plot < -timeline_plot + scale_y_continuous(limits = c(-2, 2))
        timeline_plot < -timeline_plot + geom_text(aes(y = text_position, label = event, angle = 90, vjust = -0.5), size = 3)
        timeline_plot + labs(title = i18n() $t("WHO Timeline - COVID-19"),
            caption = i18n() $t("Source: https://www.who.int/news-room/detail/08-04-2020-who-timeline---covid-19"))
    })



    output$COVID_19_timeseries < -renderPlot({

        all_19_covid_line < -all_19_covid % > %
        group_by(date) % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))

        gg < -ggplot(all_19_covid_line, aes(x = as.Date(date)))
        gg < -gg + geom_line(aes(y = Headcount_Recovered, col = i18n() $t("Recovered")), size = 2)
        gg < -gg + geom_line(aes(y = Headcount_active, col = i18n() $t("Active")), size = 2)
        gg < -gg + geom_line(aes(y = Headcount_Confirmed, col = i18n() $t("Confirmed")), size = 1)
        gg < -gg + geom_line(aes(y = Headcount_Deaths, col = i18n() $t("Deaths")), size = 1)
        gg < -gg + scale_x_date(breaks = '1 week', date_labels = "%d-%b-%y")
        gg < -gg + xlab(i18n() $t("Date"))
        gg < -gg + ylab(i18n() $t("Number of People")) + scale_y_continuous(labels = scales::comma)
        gg < -gg + scale_color_manual(name = "",
            values = c("#e82507", "#E7B800", "#0a0a0a", "#22d606"))
        gg < -gg + theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
        gg

    })


    output$Cumulative_COVID_19 < -renderPlot({



        all_19_covid_bar < -rbind(test_19_covid_Active_bar, test_19_covid_Recovered_bar, test_19_covid_Deaths_bar)


        qualitative < -c(
            "#e41a1c", "#000000", "#4daf4a"
        )

        gg2 < -ggplot(data = all_19_covid_bar, aes(x = as.Date(date), y = Headcount, fill = Type))
        gg2 < -gg2 + geom_bar(stat = "identity", position = 'stack'
            #， position = 'dodge'
        ) + scale_fill_manual(values = qualitative) + theme_tufte(ticks = FALSE) + scale_x_date(breaks = '1 week', date_labels = "%d-%b-%y")
        gg2 < -gg2 # + ggtitle("Cumulative COVID-19 number")
        gg2 < -gg2 + xlab(i18n() $t("Date")) + ylab(i18n() $t("Number of People")) + scale_y_continuous(labels = scales::comma)
        gg2 < -gg2 + theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
        gg2
        # + scale_fill_brewer(palette = sequential)


    })


    output$COVID_19_timeseries_World_China < -renderPlot({


        all_19_covid_line < -all_19_covid % > %
        group_by(date) % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))


        all_19_covid_line_china < -all_19_covid % > %
        group_by(date) % > %
        filter(Country.Region == "China") % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))

        all_19_covid_line_nonchina < -all_19_covid % > %
        group_by(date) % > %
        filter(Country.Region != "China") % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))

        all_19_covid_line2 < -merge(all_19_covid_line_china, all_19_covid_line_nonchina, by = c("date"))
        names(all_19_covid_line2)[4] < -"Confirmed_china"
        names(all_19_covid_line2)[8] < -"Confirmed_outsidechina"
        all_19_covid_line2 < -merge(all_19_covid_line[, c(1: 3)], all_19_covid_line2, by = c("date"))




        gg < -ggplot(all_19_covid_line2, aes(x = as.Date(date)))
        gg < -gg + geom_line(aes(y = Confirmed_china, col = i18n() $t("Total Confirmed Mainland China")), size = 1.5)
        gg < -gg + geom_line(aes(y = Confirmed_outsidechina, col = i18n() $t("Total Confirmed Outside Mainland China")), size = 1.5)
        gg < -gg + geom_line(aes(y = Headcount_active, col = i18n() $t("Active Worldwide")), size = 1.5)
        gg < -gg + geom_line(aes(y = Headcount_Recovered, col = i18n() $t("Total Recovered Worldwide")), size = 1.5)
        #ggtitle("COVID-19 timeseries Worldwide & Mainland China")
        gg < -gg + xlab(i18n() $t("Date")) + ylab(i18n() $t("Number of People"))
        gg < -gg + scale_x_date(breaks = '1 week', date_labels = "%d-%b-%y")
        gg < -gg + scale_y_continuous(labels = scales::comma)
        gg < -gg + scale_color_manual(name = "",
            values = brewer.pal(4, "PRGn"))
        gg < -gg + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.text = element_text(size = 7), axis.text.x = element_text(angle = 90, hjust = 1))
        gg

    })



    output$COVID_19_timeseries_World_China_log < -renderPlot({


        all_19_covid_line < -all_19_covid % > %
        group_by(date) % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))


        all_19_covid_line_china < -all_19_covid % > %
        group_by(date) % > %
        filter(Country.Region == "China") % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))

        all_19_covid_line_nonchina < -all_19_covid % > %
        group_by(date) % > %
        filter(Country.Region != "China") % > %
        summarise_at(vars(Headcount_Recovered, Headcount_active, Headcount_Confirmed, Headcount_Deaths), funs(sum(., na.rm = TRUE)))

        all_19_covid_line2 < -merge(all_19_covid_line_china, all_19_covid_line_nonchina, by = c("date"))
        names(all_19_covid_line2)[4] < -"Confirmed_china"
        names(all_19_covid_line2)[8] < -"Confirmed_outsidechina"
        all_19_covid_line2 < -merge(all_19_covid_line[, c(1: 3)], all_19_covid_line2, by = c("date"))



        gg < -ggplot(all_19_covid_line2, aes(x = as.Date(date)))
        gg < -gg + geom_line(aes(y = log(Confirmed_china), col = i18n() $t("Total Confirmed Mainland China")), size = 1.5)
        gg < -gg + geom_line(aes(y = log(Confirmed_outsidechina), col = i18n() $t("Total Confirmed Outside Mainland China")), size = 1.5)
        gg < -gg + geom_line(aes(y = log(Headcount_active), col = i18n() $t("Active Worldwide")), size = 1.5)
        gg < -gg + geom_line(aes(y = log(Headcount_Recovered), col = i18n() $t("Total Recovered Worldwide")), size = 1.5)
        #ggtitle("COVID-19 timeseries Worldwide & Mainland China log")
        gg < -gg + xlab(i18n() $t("Date")) + ylab(i18n() $t("Number of People"))
        gg < -gg + scale_x_date(breaks = '1 week', date_labels = "%d-%b-%y")
        gg < -gg + scale_color_manual(name = "",
            values = brewer.pal(4, "PRGn"))
        gg < -gg + theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.text = element_text(size = 7), axis.text.x = element_text(angle = 90, hjust = 1))
        gg
    })



})

