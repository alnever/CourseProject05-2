# Analysis of The Most Dangerous Weather Events Respect To Population Health and Economics in the USA Between 1951 and 2011
Aleksei Neverov  



## Synopsis

In this report we aim to describe the most dangerous weather events in the United States according the data collected by the U.S. National Oceanic and Atmospheric Administration (NOAA) between 1951 and 2011. We're focused on two aspects describing the damage from the storms and other weather events: harm caused population health's, including fatalities and injuries, and economic consequences. From these data, we found that some weather events are more dangerous for public health and economics than other because of damage they cause. 

## Data Processing

From the [NOAA Storm Database](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) we obtained data on storm and weather evernts across the United States collected between 1951 and 2011.


```r
fileName <- "repdata_data_StormData.csv.bz2"
dsFileName <- "repdata_data_StormData.csv"
if (!file.exists(fileName)) {
      fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"      
      download.file(fileUrl, fileName)
      if (!file.exists(dsFileName)) {
            unzip(fileName)
      }
}

storms <- read.csv(dsFileName)
dim(storms)
```

```
## [1] 902297     37
```

```r
names(storms)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

According [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) and [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf) we chose the variable of the dataset most intresting for our analysis:


```r
storms <- storms[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP", "CROPDMG","CROPDMGEXP")]
storm_names <- names(storms)
```

For analysis we chose 7 variables:

* EVTYPE contains names of weather evernts;
* FATALITIES contains number of fatalities for each event;
* INJURIES contains number of injuries for each event;
* PROPDMG and PROPDMGEXP describe the property damage of the event;
* CROPDMG and CROPDMGEXP describe the crop damage of the event.

Variables FATALITIES and INJURIES together describe the damage for the population health so we decided to summarize them to estimate the total number of victims with fatalities and injuries for each event.


```r
storms$Total.Victims <- storms$FATALITIES + storms$INJURIES
```

Variable PROPDMG, PROPDMGEXP, CROPDMG and CROPDMGEXP describe the economic damage of the event herewith PROPDMGEXP and CROPDMGEXP contain the magnitudes for PROPDMG:


```r
names(table(storms$PROPDMGEXP))
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
names(table(storms$CROPDMGEXP))
```

```
## [1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
```

Variables PROPDMGEXP and CROPDMGEXP contain are signs, letters and numbers as their values. To calculate the total economic damage we converted these signs in numbers according [results of this analysis](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html). We decided to use these results because of lacks in the [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)


```r
## Convert PROPDMGEXP into characters
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
```

As result we got the dataset with the following structure prepared for the analysis

```r
str(storms)
```

```
## 'data.frame':	902297 obs. of  9 variables:
##  $ EVTYPE       : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES   : num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES     : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG      : num  25000 2500 25000 2500 2500 2500 2500 2500 25000 25000 ...
##  $ PROPDMGEXP   : num  4 4 4 4 4 4 4 4 4 4 ...
##  $ CROPDMG      : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Total.Victims: num  15 0 2 2 2 6 1 0 15 0 ...
##  $ Total.Damage : num  25000 2500 25000 2500 2500 2500 2500 2500 25000 25000 ...
```

```r
head(storms)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 1 TORNADO          0       15   25000          4       0          0
## 2 TORNADO          0        0    2500          4       0          0
## 3 TORNADO          0        2   25000          4       0          0
## 4 TORNADO          0        2    2500          4       0          0
## 5 TORNADO          0        2    2500          4       0          0
## 6 TORNADO          0        6    2500          4       0          0
##   Total.Victims Total.Damage
## 1            15        25000
## 2             0         2500
## 3             2        25000
## 4             2         2500
## 5             2         2500
## 6             6         2500
```

## Results

### The Most Harmful Weather Events Respect To Population Health

To define the most harmful weather event in the U.S. between 1951 and 2011 we aggregated data about victims, including fatalities and injuries, by event types. Thereafter we sorted the data and selected the very first 10 event types:


```r
healthDmg <- aggregate(storms[c("Total.Victims","FATALITIES","INJURIES")], by=storms["EVTYPE"],FUN = sum)
healthDmg <- healthDmg[order(-healthDmg$Total.Victims, -healthDmg$FATALITIES, -healthDmg$INJURIES),]
healthDmg <- healthDmg[1:10,]
healthDmg
```

```
##                EVTYPE Total.Victims FATALITIES INJURIES
## 834           TORNADO         96979       5633    91346
## 130    EXCESSIVE HEAT          8428       1903     6525
## 856         TSTM WIND          7461        504     6957
## 170             FLOOD          7259        470     6789
## 464         LIGHTNING          6046        816     5230
## 275              HEAT          3037        937     2100
## 153       FLASH FLOOD          2755        978     1777
## 427         ICE STORM          2064         89     1975
## 760 THUNDERSTORM WIND          1621        133     1488
## 972      WINTER STORM          1527        206     1321
```

To demonstrate the the difference between the top ten harmful weather events we prepared the temporary dataset and drew a plot.


```r
healthDmg2 <- melt(healthDmg[,c('EVTYPE','Total.Victims','INJURIES','FATALITIES')],id.vars = 1)
```


```r
g <- ggplot(healthDmg2,aes(x = EVTYPE,y = value)) + 
      geom_bar(aes(fill = variable),position = "dodge",stat = "identity") +
      theme(axis.text.x = element_text(angle=60, hjust = 1)) +
      xlab("Weather Events") +
      ylab("Number of Victims") +
      labs(title = "Population Health Damage due the Weather Events in U.S. (1951-2011)")+
      guides(fill = guide_legend(title = "Damage type"))
print(g)
```

![](figure/unnamed-chunk-8-1.png)

It is obviously from the plot, that the most harmful event respet to population live and health in the U.S. is TORNADO. For this weather event we calculated exactly numbers of victims.


```r
mostHarmful <- healthDmg[1,]
mostHarmful
```

```
##      EVTYPE Total.Victims FATALITIES INJURIES
## 834 TORNADO         96979       5633    91346
```

According results of the an analysis we can concluse that the most harmful weather event respect to population health in U.S. is TORNADO. Between 1951 and 2011 there was 9.6979\times 10^{4} victims, including 5633 fatalities and 9.1346\times 10^{4} injuries.

### The Weather Events Having The Greatest Economic Consequences
