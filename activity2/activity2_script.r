#make a vector of tree heights in meters
heights <- c(30,41,20,22)

#convert to cm
heights_cm <- heights*100
heights_cm

#first item in the vectors
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

help(matrix)

#set up a matrix with 2 columns and fills in by row
Mat <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = T)
Mat

#set up a matrix with 2 columns and fills in by column
Mat.bycol <- matrix(c(1,2,3,4,5,6), ncol=2, byrow = F)
Mat.bycol

#index row 1, column 2
Mat.bycol[1,2]

#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from the data folder
datW <- read.csv("y:\\Students\\klian\\a02\\2011124.csv")

#get more information about the dataframe
str(datW)

#specify a column with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))


#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=T)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemps

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)