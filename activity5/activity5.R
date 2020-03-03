#load in lubridate
library(lubridate)
library(dplyr)

#read in streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)   


#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay-1/366),
                       datP$year + (datP$decDay-1/365))        

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))


#####################################
#####Q3#####
#####################################

length(datD$decYear)

length(datP$decYear)


#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#####################################
#####Q5#####
#####################################

#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
lines(datD$doy[datD$year == 2017], datD$discharge[datD$year == 2017], col = "red")
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365), #tick intervals
     lab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "End")) #tick labels
axis(2, seq(0,150, by=30),
     seq(0,150, by=30),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


#####################################
#####Q7#####
#####################################

pmeasure <- aggregate(datP$HPCP, by=list(datP$doy, datP$year), FUN="length")

pmeasure24 <- pmeasure[which(pmeasure$x == 24), ]

colnames(pmeasure24) <- c("doy", "year")

pmeasure24$decYear <- ifelse(leap_year(pmeasure24$year),pmeasure24$year + ((pmeasure24$doy-1)/366),
                             pmeasure24$year + ((pmeasure24$doy-1)/365))

dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(datD$decYear,datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,450),
     xaxs="i", yaxs ="i",
     axes=FALSE)
axis(2, seq(0,400, by=50),
     seq(0,400, by=50),
     las = 2)
axis(1, seq(2007, 2019, by=1), seq(2007, 2019, by=1))
points(x = pmeasure24$decYear, y = rep(440, nrow(pmeasure24)), pch = 1, col = "red")
legend("topright", c("Discharge","Days that have all precipitation measurements"), #legend items
       lwd=c(2,NA),#lines
       col=c("black", "red"),#colors
       pch=c(NA,1),#symbols
       bty="n")#no legend border


#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#####################################
#####Q8#####
#####################################


#subsest discharge and precipitation within range of interest
hydroDwinter <- datD[datD$doy >= 356 & datD$doy < 358 & datD$year == 2012,]
hydroPwinter <- datP[datP$doy >= 356 & datP$doy < 358 & datP$year == 2012,]

min(hydroDwinter$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
ylwinter <- floor(min(hydroDwinter$discharge))-1
#celing rounds up to the integer
yhwinter <- ceiling(max(hydroDwinter$discharge))+1
#minimum and maximum range of precipitation to plot
plwinter <- 0
pmwinter <-  ceiling(max(hydroPwinter$HPCP))+.5
#scale precipitation to fit on the 
hydroPwinter$pscale <- (((yhwinter-ylwinter)/(pmwinter-plwinter)) * hydroPwinter$HPCP) + ylwinter

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroDwinter$decDay,
     hydroDwinter$discharge, 
     type="l", 
     ylim=c(ylwinter,yhwinter), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroPwinter)){
        polygon(c(hydroPwinter$decDay[i]-0.017,hydroPwinter$decDay[i]-0.017,
                  hydroPwinter$decDay[i]+0.017,hydroPwinter$decDay[i]+0.017),
                c(ylwinter,hydroPwinter$pscale[i],hydroPwinter$pscale[i],ylwinter),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}



library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

#####################################
#####Q9#####
#####################################

datD2016 <- datD[datD$year == 2016,]
datD2017 <- datD[datD$year == 2017,]

season2016 <- c()

for(i in 1:nrow(datD2016)){
        if(datD2016$doy[i] >= 61 && datD2016$doy[i]<= 152){
                season2016[i] <- "Spring"
        }
        else if(datD2016$doy[i] >= 153 && datD2016$doy[i] <= 244){
                season2016[i] <- "Summer"
        }
        else if(datD2016$doy[i] >= 245 && datD2016$doy[i] <= 335){
                season2016[i] <- "Fall"
        }
        else{
                season2016[i] <- "Winter"
        }
}

datD2016$season <- as.factor(season2016)

season2017 <- c()

for(i in 1:nrow(datD2017)){
        if(datD2017$doy[i] >= 60 && datD2017$doy[i]<= 151){
                season2017[i] <- "Spring"
        }
        else if(datD2017$doy[i] >= 152 && datD2017$doy[i] <= 243){
                season2017[i] <- "Summer"
        }
        else if(datD2017$doy[i] >= 244 && datD2017$doy[i] <= 334){
                season2017[i] <- "Fall"
        }
        else{
                season2017[i] <- "Winter"
        }
}


datD2017$season <- as.factor(season2017)



ggplot(datD2016, aes(x=season, y=discharge)) + 
        geom_violin(fill="lightblue",    
                    trim = FALSE,
                    alpha = 0.5, 
                    show.legend = FALSE)+
        xlab("Season")  + 
        ylab(bquote("Discharge ft"^3~"sec"^-1))    + 
        ggtitle("Discharge on 2016") + 
        theme_bw()  


ggplot(datD2017, aes(x=season, y=discharge)) + 
        geom_violin(fill="lightblue",    
                    trim = FALSE,
                    alpha = 0.5, 
                    show.legend = FALSE)+
        xlab("Season")  + 
        ylab(bquote("Discharge ft"^3~"sec"^-1))    + 
        ggtitle("Discharge on 2017") + 
        theme_bw()  
