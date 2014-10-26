#setwd("/Users/dfernandezcanon/Documents/Development/R/Project/ReproducibleResearch/Project2")

#download packages if needed
list.of.packages <- c("plyr","grid","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages and functions
library("plyr")
library("grid")
library("ggplot2")
source("giveConsistencyEVTYPE.R")
source("convertEXP.R")

#download and read data
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', destfile='stormData.csv.bz2', method='curl')
storm.data <- read.csv('stormData.csv.bz2')

#I reduce our data frame to only the values that have some kind of interest for this study
sub.storm <- storm.data[storm.data$FATALITIES > 0 | storm.data$INJURIES >0 | storm.data$PROPDMG >0 | storm.data$CROPDMG > 0,]

#I convert the date
sub.storm$BGN_DATE <- as.Date(sub.storm$BGN_DATE, "%m/%d/%Y %H:%M:%S")

#EVTYPE is quite inconsistent, as it is very time consuming to reduce almost 1000 variables to the 48 that are defined by the National weather service in the documentation, I have decided to reduce my study to the last 15 years.   
sub.storm2 <- sub.storm[sub.storm$BGN_DATE > "1999-01-01", ]

sub.storm2 <- giveConsistencyEVTYPE(sub.storm2)

#health data processing
fatalities.type <- ddply(sub.storm2, .(EVTYPE), summarize, steps = sum(FATALITIES, na.rm=TRUE))
colnames(fatalities.type)[2] <- c("Fatalities")
injuries.type <- ddply(sub.storm2, .(EVTYPE), summarize, steps = sum(INJURIES, na.rm=TRUE))
colnames(injuries.type)[2] <- c("Injuries")

health.data <- merge(fatalities.type, injuries.type, by="EVTYPE")


#economics data processing
levels(sub.storm2$PROPDMGEXP)
levels(sub.storm2$CROPDMGEXP)
sub.storm2 <- convertEXP(sub.storm2)
sub.storm2$PROPDMGTOT <- sub.storm2$PROPDMG * 10^as.integer(sub.storm2$PROPDMGEXP)
sub.storm2$CROPDMGTOT <- sub.storm2$CROPDMG * 10^as.integer(sub.storm2$CROPDMGEXP)

propdmg.type <- ddply(sub.storm2, .(EVTYPE), summarize, steps = sum(PROPDMGTOT, na.rm=TRUE))
colnames(propdmg.type)[2] <- c("Propdmg")
cropdmg.type <- ddply(sub.storm2, .(EVTYPE), summarize, steps = sum(CROPDMGTOT, na.rm=TRUE))
colnames(cropdmg.type)[2] <- c("Cropdmg")

economics.data <- merge(propdmg.type, cropdmg.type, by="EVTYPE")

#Results health
#get the 15 that are most harmful with respect to population health
top15.health.data <- arrange(health.data, desc(Injuries))[1:15, ]
print(top15.health.data[,c(1,3)])
top15.health.data <- arrange(health.data, desc(Fatalities))[1:15, ]
print(top15.health.data[,c(1:2)])

#plot the 15 that are most harmful with respect to population health
plot1 <- ggplot(top15.health.data, aes(x = EVTYPE, y = Injuries))
plot1 <- plot1 + geom_bar(stat = "identity")
plot1 <- plot1 + xlab("Event type") + ylab("Number of injuries")
plot1 <- plot1 + theme(axis.text.x = element_text(angle = 35, hjust = 1))

plot2 <- ggplot(top15.health.data, aes(x = EVTYPE, y = Fatalities))
plot2 <- plot2 + geom_bar(stat = "identity")
plot2 <- plot2 + xlab("Event type") + ylab("Number of fatalities")
plot2 <- plot2 + theme(axis.text.x = element_text(angle = 35, hjust = 1))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))

#Results economics
#get the 15 events that have the greatest economic consequences
top15.economics.data <- arrange(economics.data, desc(Propdmg))[1:15, ]
print(top15.economics.data[,c(1:2)])
top15.economics.data <- arrange(economics.data, desc(Cropdmg))[1:15, ]
print(top15.economics.data[,c(1,3)])

#plot the 15 events that have the greatest economic consequences
plot1 <- ggplot(top15.economics.data, aes(x = EVTYPE, y = Propdmg))
plot1 <- plot1 + geom_bar(stat = "identity")
plot1 <- plot1 + xlab("Event type") + ylab("Property damage")
plot1 <- plot1 + theme(axis.text.x = element_text(angle = 35, hjust = 1))

plot2 <- ggplot(top15.economics.data, aes(x = EVTYPE, y = Cropdmg))
plot2 <- plot2 + geom_bar(stat = "identity")
plot2 <- plot2 + xlab("Event type") + ylab("Crop damange")
plot2 <- plot2 + theme(axis.text.x = element_text(angle = 35, hjust = 1))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))

