
setwd("C:/Users/Zing/OneDrive/GitHub/R/19_covid")


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


time_series_19_covid_Confirmed <- read.csv("./time_series_19-covid-Confirmed.csv")
time_series_19_covid_Recovered <- read.csv("./time_series_19-covid-Recovered.csv")
time_series_19_covid_Deaths <- read.csv("./time_series_19-covid-Deaths.csv")

View(time_series_19_covid_Confirmed)
names(time_series_19_covid_Confirmed)



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





test_19_covid_Confirmed_bar<-reshape2::melt(time_series_19_covid_Confirmed,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                        variable.name = "Calendar_Date", 
                                        ,value.name = "Headcount"
)

test_19_covid_Recovered_bar<-reshape2::melt(time_series_19_covid_Recovered,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                                  variable.name = "Calendar_Date", 
                                                  ,value.name = "Headcount"
                                                 )

test_19_covid_Deaths_bar<-reshape2::melt(time_series_19_covid_Deaths,id.vars=c("Province.State","Country.Region","Lat","Long"),
                                                  variable.name = "Calendar_Date", 
                                                  ,value.name = "Headcount"
                                                 )

test_19_covid_Confirmed_bar$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Confirmed_bar$Calendar_Date), orders = "mdy")

test_19_covid_Recovered_bar$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Recovered_bar$Calendar_Date), orders = "mdy")

test_19_covid_Deaths_bar$date<-lubridate::parse_date_time(sub("X", "", test_19_covid_Deaths_bar$Calendar_Date), orders = "mdy")

test_19_covid_Confirmed_bar$Type<-'Confirmed'
test_19_covid_Recovered_bar$Type<-'Recovered'
test_19_covid_Deaths_bar$Type<-'Deaths'

test_19_covid_Active_bar<-all_19_covid[,c(1:5,12,7)]
test_19_covid_Active_bar$Type<-'Active'

names(test_19_covid_Active_bar)[6]<-"Headcount"
names(test_19_covid_Active_bar)[7]<-"date"

#all_19_covid_bar<-rbind(test_19_covid_Confirmed_bar,test_19_covid_Recovered_bar,test_19_covid_Deaths_bar,test_19_covid_Active_bar)

all_19_covid_bar<-rbind(test_19_covid_Active_bar,test_19_covid_Recovered_bar,test_19_covid_Deaths_bar)



world <- readOGR("countries.geo.json", encoding="OGRGeoJSON", stringsAsFactors=FALSE)
world_map <- fortify(world)

world <- gBuffer(world, byid=TRUE, width=0)
world <- nowrapRecenter(world)

test2<-as.data.frame(world_map)
test2[which(test2$long == 33.80340), ]




all_19_covid_today<- all_19_covid %>% filter( 
                                            date.x >=as.Date("2020-03-07") & date.x <=as.Date("2020-03-07")
                                            )


gg <- ggplot()
gg <- gg + geom_map(data=world_map, map=world_map,
                    aes(x=long, y=lat, map_id=id),
                    color="white", size=0.15, fill="#d8d8d6")
##gg<- gg +
##  geom_point(data = test_19_covid_Confirmed, aes(x = Long, y = Lat), color = "#e60000")

gg <- gg + geom_point(data=all_19_covid_today,
                      aes(x=Long, y=Lat, size=Headcount_Confirmed),
                      shape=8, ##alpha=1/3
                      , color="red" #"#d47e5d"
                        , fill="#00000000")
gg <- gg + geom_point(data=subset(all_19_covid_today, Headcount_Confirmed>2000),
                      aes(x=Long, y=Lat, size=Headcount_Confirmed),
                      shape=1, alpha=1, color="black", fill="#00000000")


gg <- gg +  geom_scatterpie(data = all_19_covid_today, 
                            aes(x=Long, y=Lat, r = sqrt(Headcount_Confirmed)/50),
                            cols = c("Headcount_active","Headcount_Recovered", "Headcount_Deaths"), 
                            alpha = 0.5) +
  scale_fill_manual(
    breaks = c("Headcount_active","Headcount_Recovered", "Headcount_Deaths"),
    labels = c("Active","Recovered", "Deaths"),
    values = c("Headcount_Recovered" = "orange",
               #"white" = "white",
               "Headcount_Deaths" = "black"
               ,"Headcount_active" = "cyan"
    )
  )


gg <- gg + scale_size(name="Confirmed Cases"#,trans='identity'
                     ,breaks = c(10,100,500,1000,10000,20000,30000,50000,70000,80000),
                       , labels =  c("10","100","500","1000","10000","20000","30000","50000","70000","80000")
                      ##                       ,range=c(10000, 70000)
                      ,guide = "legend")

gg <- gg + coord_quickmap()
gg <- gg + theme_map()
gg <- gg +theme(legend.position = c(0.96, 0.02)
          ,legend.justification = c(1, 0)
#         , panel.grid = element_blank(),
#          panel.border = element_blank(),
#          axis.title = element_blank(),
#          axis.text = element_blank(),
#          axis.ticks = element_blank()
)
#gg <- gg + theme(legend.position=c(0.05, 0.99))
gg <- gg + theme(legend.direction="horizontal")
gg <- gg + theme(legend.key=element_rect(color="#00000000"))
gg






#sequential <- brewer.pal(6, "BuGn")

qualitative<-c(
"#e41a1c"
,"#000000"
,"#4daf4a"
)

gg2 <-ggplot(data = all_19_covid_bar, aes(x = as.Date(date), y = Headcount,fill=Type)) 
gg2 <-gg2+ geom_bar(stat = "identity", position = 'stack'#， position = 'dodge'
	)+scale_fill_manual(values=qualitative)+theme_tufte(ticks = FALSE)+scale_x_date(breaks = '1 week')
gg2 <-gg2+ggtitle("Cumulative COVID-19 number") + xlab("Date") + ylab("Number of People")
gg2+theme_set(theme_bw())
gg2
#+ scale_fill_brewer(palette = sequential)







all_19_covid_line<-all_19_covid %>%
  group_by(date) %>%
  summarise_at(vars(Headcount_Recovered,Headcount_active,Headcount_Confirmed,Headcount_Deaths),funs(sum(.,na.rm=TRUE)))


ggplot(all_19_covid_line, aes(x=as.Date(date))) + 
  geom_line(aes(y=Headcount_Recovered, col="Recovered"), size = 2) + 
  geom_line(aes(y=Headcount_active, col="Active"), size = 2) +
  geom_line(aes(y=Headcount_Confirmed, col="_Confirmed"), size = 1) + 
  geom_line(aes(y=Headcount_Deaths, col="Deaths"), size = 1) +
  scale_x_date(breaks = '1 week') +  
  scale_color_manual(name="", 
                     values = c("#e82507", "#E7B800","#0a0a0a","#22d606")) 
  theme(panel.grid.minor = element_blank()) 





all_19_covid_line_china<-all_19_covid %>%
  group_by(date) %>%
  filter(Country.Region =="Mainland China") %>%
  summarise_at(vars(Headcount_Recovered,Headcount_active,Headcount_Confirmed,Headcount_Deaths),funs(sum(.,na.rm=TRUE)))


all_19_covid_line_nonchina<-all_19_covid %>%
  group_by(date) %>%
  filter(Country.Region !="Mainland China") %>%
  summarise_at(vars(Headcount_Recovered,Headcount_active,Headcount_Confirmed,Headcount_Deaths),funs(sum(.,na.rm=TRUE)))

all_19_covid_line2<-merge(all_19_covid_line_china,all_19_covid_line_nonchina,by=c("date"))
names(all_19_covid_line2)[2:3]<-c("Confirmed_china","Confirmed_outsidechina")
all_19_covid_line2<-merge(all_19_covid_line[,c(1:3)],all_19_covid_line2,by=c("date"))



ggplot(all_19_covid_line2, aes(x=as.Date(date))) + 
  geom_line(aes(y=Confirmed_china, col="Total Confirmed Mainland China"), size = 1.5) + 
  geom_line(aes(y=Confirmed_outsidechina, col="Total Confirmed Outside Mainland China"), size = 1.5) +
  geom_line(aes(y=Headcount_active, col="Active Worldwide"), size = 1.5) +
  geom_line(aes(y=Headcount_Recovered, col="Total Recovered Worldwide"), size = 1.5) + 
  scale_x_date(breaks = '1 week') +  
  scale_color_manual(name="", 
                     values = brewer.pal(4, "PRGn")) 
theme(panel.grid.minor = element_blank()) 

