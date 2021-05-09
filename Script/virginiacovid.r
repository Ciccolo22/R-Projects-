## Load libraries
library(ggplot2)
library(formattable)
library(lubridate)

#read in NYT COVID data
vacsv <- read.csv('us-counties.csv')                  

#convert date using lubridate
vacsv$date <- ymd(vacsv$date)

#look @ just the state of virginia
vac2 <- vacsv%>% filter(state=='Virginia')
View(vac2)

##preprocessing
countydata1 <- vac2%>% filter(county %in% c('Loudoun', 'Fairfax', 'Chesterfield',
                                             'Richmond city', 'Richmond', 'Goochland', 'Newport News city',
                                             'Arlington', 'Henrico', 'Virginia Beach city', 'Williamsburg city',
                                            'Norfolk city'))

##plot deaths and cases
ggplot(countydata1, aes(date, deaths, group=county,col=county, linetype=county))+
  geom_line()+geom_point()+theme_bw()+ggtitle('Total Deaths by Day')

ggplot(countydata1, aes(date, cases, group=county,col=county, linetype=county))+
  geom_line()+geom_point()+theme_bw()+ggtitle('Total Cases by Day')


#summarize cases and deaths to be put into a clean table using formattable package
fortab <- countydata1%>%group_by(county)%>%summarise(totaldeaths=max(deaths), totalcases=max(cases))%>% arrange(desc(totaldeaths), desc(totalcases))

sum(fortab$totaldeaths)
newlist <- list(fortab)
fortab$totalcases <- as.numeric(fortab$totalcases)

#create nice table set colors

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

blue = '#1e90ff'
powdah = '#b0e0e6'

#create table, still needs some work..
formattable(fortab, align=c('l','c', 'c'), list(totaldeaths=color_tile(powdah,blue), totalcases=color_tile(customGreen,customGreen0)))

            

            
