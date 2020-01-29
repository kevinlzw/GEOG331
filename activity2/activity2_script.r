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




