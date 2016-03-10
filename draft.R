library(reshape2)
library(ggplot2)

storms <- read.csv("repdata_data_StormData.csv")
storms <- storms[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP", "CROPDMG","CROPDMGEXP")]
## kilo <- storms$PROPDMGEXP == 'K'
## mill <- storms$PROPDMGEXP == 'M'
## bill <- storms$PROPDMGEXP == 'B'
## storms$PROPDMG[kilo] <- storms$PROPDMG[kilo] * 1000
## storms$PROPDMG[mill] <- storms$PROPDMG[mill] * 1*10^6
## storms$PROPDMG[bill] <- storms$PROPDMG[bill] * 1*10^9

storms$Total.Victims <- storms$FATALITIES + storms$INJURIES
storms$PROPDMGEXP <- as.character(storms$PROPDMGEXP)

## Replace PROPDMGEXP with numbers
storms$PROPDMGEXP[storms$PROPDMGEXP %in% c("0":"8")] <- 2
storms$PROPDMGEXP[storms$PROPDMGEXP == "-" | storms$PROPDMGEXP == "?" | storms$PROPDMGEXP == ""] <- 0
storms$PROPDMGEXP[storms$PROPDMGEXP == "+"] <- 1
storms$PROPDMGEXP[storms$PROPDMGEXP == "h" | storms$PROPDMGEXP == "H"] <- 3
storms$PROPDMGEXP[storms$PROPDMGEXP == "k" | storms$PROPDMGEXP == "K"] <- 4
storms$PROPDMGEXP[storms$PROPDMGEXP == "m" | storms$PROPDMGEXP == "M"] <- 7
storms$PROPDMGEXP[storms$PROPDMGEXP == "B"] <- 10

## Convert PROPDMGEXP into numeric
storms$PROPDMGEXP <- as.numeric(storms$PROPDMGEXP)

## Calculate property damade in dollars
storms$PROPDMG <- storms$PROPDMG * 10 ^ (storms$PROPDMGEXP-1) * ifelse(storms$PROPDMGEXP == 0,0,1)

## Convert CROPDMGEXP into characters
storms$CROPDMGEXP <- as.character(storms$CROPDMGEXP)

## Replace CROPDMGEXP with numbers
storms$CROPDMGEXP[storms$CROPDMGEXP %in% c("0":"8")] <- 2
storms$CROPDMGEXP[storms$CROPDMGEXP == "-" | storms$CROPDMGEXP == "?" | storms$CROPDMGEXP == ""] <- 0
storms$CROPDMGEXP[storms$CROPDMGEXP == "+"] <- 1
storms$CROPDMGEXP[storms$CROPDMGEXP == "h" | storms$CROPDMGEXP == "H"] <- 3
storms$CROPDMGEXP[storms$CROPDMGEXP == "k" | storms$CROPDMGEXP == "K"] <- 4
storms$CROPDMGEXP[storms$CROPDMGEXP == "m" | storms$CROPDMGEXP == "M"] <- 7
storms$CROPDMGEXP[storms$CROPDMGEXP == "B"] <- 10

## Convert CROPDMGEXP into numeric
storms$CROPDMGEXP <- as.numeric(storms$CROPDMGEXP)

## Calculate crop damade in dollars
storms$CROPDMG <- storms$CROPDMG * 10 ^ (storms$CROPDMGEXP-1) * ifelse(storms$CROPDMGEXP == 0,0,1)

## Calculate total economic damage

storms$Total.Damage <- storms$PROPDMG + storms$CROPDMG



t <- tapply(storms$FATALITIES, storms$EVTYPE, sum, na.rm = TRUE)
t <- t[t != 0]
barplot(t)
x <- which(t == max(t))
max_fatalities <- t[t == max(t)]
t <- sort(t, decreasing = TRUE)
t10 <- t[1:10]
barplot(t10, cex.names = .5)

t <- tapply(storms$INJURIES, storms$EVTYPE, sum, na.rm = TRUE)
t <- t[t != 0]
barplot(t)
x <- names(which(t == max(t)))
max_injuries <- t[t == max(t)]
t <- sort(t, decreasing = TRUE)
t10 <- t[1:10]
barplot(t10, cex.names = .5)

t <- tapply(storms$PROPDMG, storms$EVTYPE, sum, na.rm = TRUE)
t <- t[t != 0]
barplot(t)
x <- names(which(t == max(t)))
max_injuries <- t[t == max(t)]
t <- sort(t, decreasing = TRUE)
t10 <- t[1:10]
barplot(t10, cex.names = .5)

## health damage

healthDmg <- aggregate(storms[c("Total.Victims","FATALITIES","INJURIES")], by=storms["EVTYPE"],FUN = sum)
healthDmg <- healthDmg[order(-healthDmg$Total.Victims, -healthDmg$FATALITIES, -healthDmg$INJURIES),]
healthDmg <- healthDmg[1:10,]
healthDmg

healthDmg2 <- melt(healthDmg[,c('EVTYPE','Total.Victims','INJURIES','FATALITIES')],id.vars = 1)

g <- ggplot(healthDmg2,aes(x = EVTYPE,y = value)) + 
      geom_bar(aes(fill = variable),position = "dodge",stat = "identity") +
      theme(axis.text.x = element_text(angle=60, hjust = 1)) +
      xlab("Weather events") +
      ylab("Number of victims") +
      labs(title = "Population health damage due the Weather Events in U.S. (1951-2011)")+
      guides(fill = guide_legend(title = "Damage type"))
      
      

## g <- ggplot(hd)
## g <- g + geom_bar(aes(x=EVTYPE, y=Victims), stat = "identity", col = "blue",fill="blue", position=position_dodge(width=.3))
## g <- g + geom_bar(aes(x=EVTYPE, y=INJURIES), stat = "identity", col = "green",fill="green", position=position_dodge(width=.3))