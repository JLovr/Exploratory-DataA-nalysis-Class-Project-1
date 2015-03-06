plot2<-function(workingdirectory=getwd()) {
## plot2.R - standard plot of Global Active Power
## for dates 2007/02/01 through 2007/02/02
## data is assumed to be in the working directory
## use the basic plot package  
  
  
## save par values
  pardefault <- par(no.readonly = T) 
  
## Read data cc
  setwd(workingdirectory)  
  a<-read.table("household_power_consumption.txt",header=TRUE,sep=";")
  ## reformat data
  a[,1]<-as.Date(a[,1],"%d/%m/%Y")
 
## a[,2]<-as.POSIXct(strptime(a[,2], "%H:%M:%S"))
  for (i in 3:9)  {a[,i]<-as.numeric(as.character(a[,i]))}

## filter data
  fromd<-as.Date("2007-02-01")
  tod<-as.Date("2007-02-02")
  ok<-a$Date>=fromd & a$Date<=tod
  w<-a[ok,]

## Combine date and time fields for plot, standardize class
  WD<-paste(w$Date,w$Time,sep=" ")
  WE<-as.POSIXct(strptime(WD,"%Y-%m-%d %H:%M:%S"))
  w<-cbind(w,WE)

## reduce font size, set margins
  par(ps = 12, cex = 1, cex.main = 1,mar=c(4,4,3,1))

## display the plot
  plot(w$WE,w$Global_active_power,type="l",
       main="",xlab="",ylab="Global Active Power (kilowatts)")
  dev.copy(png,"plot2.png")
  dev.off()

## restore par defaults
  par(pardefault)
}