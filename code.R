library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(countrycode)
library(highcharter)
library(directlabels)
library(plotly)
library(DT)
library(ggthemes)
library(ggExtra)
library(tidyverse)
str(data)
summary(data)

data_events <- read.csv("olympics.csv")
        
data_team <- read.csv("olympics.csv")
        
data <- data_events%>%left_join(data_team, by =NULL)
head(data, n=10)%>% datatable(style="bootstrap", class="table-condensed", extensions = 'Responsive',
                              options = list(dom = 'tp',scrollX = TRUE, pageLength = 5))
season = data_events%>%left_join(data_team,by=NULL)
    season$notes = NULL
    season =season%>%group_by(Season,Year,Sex)%>%summarize(total=n())
        
        ggplot(season, aes(x=Year, y=total,colour=total,group=Sex))+
          geom_line(size=1.5)+
          geom_dl(aes(label = Sex), method = list(dl.trans(x = x + .05), "last.points")) +
          facet_wrap(~Season)+
          scale_color_viridis(option = "B")+
          scale_x_continuous(breaks = seq(1896, 2016, by = 12))+
          xlab("Year")+ylab("Athletes")+
          theme(axis.line = element_line(color = "orange",size=1))+theme(panel.background=element_blank())+
          theme(legend.position = "none",
                axis.text = element_text(size = 8,face="bold"),
                plot.title = element_text(size=12,face = "bold")) + 
          
          ggtitle("Male and Female Athletes",subtitle = "Olympics 1896 to 2016 ") 
        gender = data_events%>%filter(!is.na(Medal),Season=="Summer")
        
        
        gender =gender%>%group_by(Year,Sex)%>%summarize(total=n())
        
        ggplot(gender, aes(x=Year, y=total,colour=total,group=Sex))+
          geom_line(size=1.5)+
          geom_dl(aes(label = Sex), method = list(dl.trans(x = x + .2), "last.points")) +
          scale_color_viridis(option = "B")+
          scale_x_continuous(breaks = seq(1896, 2016, by = 8))+
          xlab("Year")+ylab("Medals")+
          theme(axis.line = element_line(color = "orange",size=1))+theme(panel.background=element_blank())+
          theme(legend.position = "none",
                axis.text = element_text(size = 8,face="bold"),
                plot.title = element_text(size=12,face = "bold")) + 
          
          ggtitle("Men and Women Medal winners ",subtitle = "Summer Olympics 1896 to 2016 ")
        age_mi <- data_events%>%filter(!is.na(Medal),Season=='Summer')
        age_mi <-age_mi%>%group_by(Sex,Sport)%>%summarize(Age=min(Age,na.rm = TRUE))
        age_min <- data_events%>%filter(!is.na(Medal),Season=="Summer")%>%right_join(age_mi,by=c("Sex","Sport","Age"))
        
        
        c <-ggplot(age_min,aes(Sport,Age, color=Sport,fill=Name)) +
          geom_bar(position = "dodge",  width =.5,stat="identity") +
          coord_flip()+
          facet_wrap(~Sex)+
          theme_grey() + 
          scale_x_discrete() +
          xlab("Sport")+ylab("Age")+
          theme(legend.position = "none",
                axis.text = element_text(size = 8,face="bold"),
                plot.title = element_text(size=16,face = "bold")) + 
          ggtitle("Youngest Medal Winners in all Games") 
        
        ggplotly(c)
        df <- data_events%>%filter(!is.na(Age),Season=='Summer')%>%group_by(Sex,Age,Year)%>%summarize(pop=n())
        df$Sex <- ifelse(df$Sex=="F","Female","Male")
        
        df <- df%>%
          mutate(athletes = pop*ifelse(Sex == "Female", -1, 1))
        
        series <- df %>% 
          group_by(Sex, Age)%>%
          do(data = list(sequence = .$athletes)) %>% 
          ungroup() %>% 
          group_by(Sex) %>% 
          do(data = .$data) %>%
          mutate(name = Sex)%>%
          list_parse()
        
        maxpop <- max(abs(df$athletes))
        
        xaxis <- list(categories = sort(unique(df$Age)),
                      reversed = FALSE, tickInterval = 3,
                      labels = list(step= 3))
        
        yrs <-  sort(unique(df$Year))
        
        highchart() %>%
          hc_chart(type = "bar") %>%
          hc_motion(enabled = TRUE, labels =yrs, series = c(0,1), autoplay = TRUE, updateInterval = 4) %>% 
          hc_add_series_list(series) %>% 
          hc_plotOptions(
            series = list(stacking = "normal"),
            bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
          ) %>% 
          hc_tooltip(shared = TRUE) %>% 
          hc_yAxis(
            min=-800,max=800)%>% 
          hc_xAxis(
            xaxis,
            rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
          ) %>% 
          hc_tooltip(shared = FALSE,
                     formatter = JS("function () { return '<b>' + this.series.name + ', Age ' + this.point.category + '</b><br/>' + 'athletes: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
          ) %>%
          hc_title(text = " Athletes by Gender and Age") %>%
          hc_subtitle(text = "Summer Olympics 1896 to 2016")
        
        athlete = athlete%>%left_join(data_team,by=NULL)
        
        athlete =athlete%>%group_by(Sex,Name)%>%summarize(total=n())%>%arrange(desc(total))
        women =athlete%>%filter(Sex=="F")%>%head(n=30)
        men =athlete%>%filter(Sex=='M')%>%head(n=30)
        
        men$Name <- factor(men$Name,levels = men$Name[order(men$total)])
        
        ggplot(men,aes(Name,total,color=Name,fill=Name)) +
          geom_bar(position = "stack",  width =.6,stat="identity") +
          coord_flip()+
          geom_text(aes(label=total,hjust=-.03,  colour="black"),size=3)+
          
          theme(axis.line = element_line(color = "orange",size=1))+
          theme(panel.background=element_blank())+ 
          scale_x_discrete() +
          xlab("Athlete")+ylab("Medals")+
          theme(legend.position = "none",
                axis.text = element_text(size = 8,face="bold"),
                plot.title = element_text(size=16,face = "bold")) + 
          ggtitle("Top men medal winners " ,subtitle = "Summer Olympics 1896 to 2016")
        
        sport <- data_events%>%filter(!is.na(Medal),Season=='Summer',Year==2016)%>%left_join(data_team,by=NULL)
        sport$notes<-NULL
        sport <- sport%>%group_by(Sport,Team)%>%summarize(total=n())
        
        slist =c('Archery','Athletics','Badminton','Baseball','Basketball','Boxing','Canoeing','Football','Gymnastics','Hockey','Judo','Rowing',' Shooting','Swimming ','Table Tennis','Tennis','Triathlon','Weightlifting','Wrestling','Volleyball')
        
        sport <- filter(sport,Sport%in%slist,total>=1)
        sport$Team <- as.factor(sport$Team)
        
        p<-ggplot(sport,aes(Sport,total,color=Team,fill=Team)) +
          geom_bar(position = "stack",  width =.75,stat="identity") +
          coord_flip()+
          theme_dark() + 
          scale_x_discrete() +
          xlab("Sport")+ylab("Medals")+
          theme(legend.position = "none",
                axis.text = element_text(size = 8,face="bold"),
                plot.title = element_text(size=16,face = "bold")) + 
          ggtitle("Major sports  in 2016 games") 
        
        ggplotly(p)
        age_m <- data_events%>%filter(!is.na(Age), Season=='Summer')
        age_m$Medal <- ifelse(is.na(age_m$Medal),"others",ifelse(age_m$Medal=="Gold","Gold",ifelse(age_m$Medal=="Silver","Silver","Bronze")))
        
        hcboxplot(x = age_m$Age, var = age_m$Sex, var2 = age_m$Medal, 
                  outliers = FALSE) %>% 
          hc_chart(type = "column")%>%
          hc_title(text = "Age of medal winners and all athletes") %>%
          hc_subtitle(text = "Summer Olympics 1896 to 2016") %>%
          hc_credits(enabled = TRUE, text = "120 years of Olympic history: athletes and results", 
                     style = list(fontSize = "10px")) %>%
          hc_add_theme(hc_theme_sandsignika())
k<-count_medal
k<-filter(k,(Team=='India'|Team=='United States'|Team=='China'|Team=='Russia'|Team=='Germany'|Team=='Japan'|Team=='Pakistan'|Team=='Sri Lanka'|Team=='South Korea'))
ggplot(data=k,aes(x=Team,y=Medals))+
  geom_point() +
  scale_color_viridis(option = "B")+
  xlab("countries")+ylab("Medals")+
  theme(axis.line = element_line(color = "orange",size=1))+theme(panel.background=element_blank())+
  theme(legend.position = "none",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=12,face = "bold")) + 
  
  ggtitle("Medals won")
