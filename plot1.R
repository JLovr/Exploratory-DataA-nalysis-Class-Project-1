plot1<-function(workingdirectory=getwd()) {
## plot1.R - hist plot of Global Active Power  
## for dates 2007/02/01 through 2007/02/02
## data is assumed to be in the working directory
## use the basic plot package  
  
## save par values
  pardefault <- par(no.readonly = T) 
  
## Read data cc
  setwd(workingdirectory)
  a<-read.table("household_power_consumption.txt",
                header=TRUE,sep=";")
  
## reformat data
  a[,1]<-as.Date(a[,1],"%d/%m/%Y")
  a[,2]<-as.POSIXct(strptime(a[,2], "%H:%M:%S"))
  for (i in 3:9)  {a[,i]<-as.numeric(as.character(a[,i]))}

## filter data
  fromd<-as.Date("2007-02-01")
  tod<-as.Date("2007-02-02")
  ok<-a$Date>=fromd & a$Date<=tod
  w<-a$Global_active_power[ok]

## reduce font size, set margins
  par(ps = 12, cex = 1, cex.main = 1,mar=c(4,4,3,1))

## display the histogram
  hist(w,breaks=12,
       xlab="Global Active Power (kilowatts)",ylab="Frequency",
       col="red",right=FALSE,main="")

## add title separately so to be able to use a larger font
  par(ps = 14, cex = 1, cex.main = 1)
  title("Global Active Power")
  dev.copy(png,"plot1.png")
  dev.off()

## restore par defaults
  par(pardefault)

}