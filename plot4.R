plot4<-function(workingdirectory=getwd()) {
## plot3.R - standard plot of Global Active Power
## for dates 2007/02/01 through 2007/02/02
## data is assumed to be in the working directory
## use the basic plot package  
 
  
## save par values
  pardefault <- par(no.readonly = T) 
  
## import required packages
  if(!require(reshape)) {
    install.packages("reshape")
    if(!require(reshape)) {
      stop("can't install reshape package")
    }
  }
  
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
  
## Combine date and time fields for plot, standardize class, reduce working dataset
  WD<-paste(w$Date,w$Time,sep=" ")
  WE<-as.POSIXct(strptime(WD,"%Y-%m-%d %H:%M:%S"))
  w<-cbind(w,WE)
 
## employ internal function for actual plot as work around for
## legend issues with dev.copy in R
  fplot<-function(w) {  
    
## set up matrix for plot display
  par(mfcol=c(2,2))  
  
## reduce font size, set margins
  par(ps = 11)
  ## display the plots ##
  
## do first plot(upper left)
  plot(w$WE,w$Global_active_power,type="l",
     main="",xlab="",ylab="Global Active Power (kilowatts)")

  
## do second plot (lower left)
## set colors for each type

## remove unnecessary data

  w1<-w[,c(-1:-6)]
  colors<-c("black","red","blue")
  fields<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
  
## set up basic plot with proper scaling
## requires combining sub metering data - doing it this way to
## ensure that all relevant data is taken into account
## could also do a melt
  ww<-melt(w1,id.vars="WE",measure.vars=fields)
  colnames(ww)<-c("WE","type","smv")
  wx<-ww[,-2]
  
## now construct framework
  plot(wx,type="l",xlab="",col="white",ylab="Energy sub metering")
  
## fill in data
  for(i in 1:3) {
    lines(w$WE,w[,fields[i]],type="l",
          main="",col=colors[i],xlab="",ylab="")
  }
  
## add legends, etc.
  par(ps = 12)
  leg<-c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
  legend("topright",legend=leg,lty=1,col=colors,
         xjust=0,y.intersp=1,merge=TRUE,pch=NA,cex=.8)


## do 3rd plot (upper right)
  plot(w$WE,w$Voltage,type="l",
     main="",xlab="datetime",ylab="Voltage")

  
## do 4th plot (lower right)
  plot(w$WE,w$Global_reactive_power,type="l",
     main="",xlab="datetime",ylab="Global_reactive_power")
}

  fplot(w)
## save plot to file - do this the long way because of
## dev.copy issues with legends
  png("plot4.png")
  fplot(w)  
  dev.off()

## restore par defaults
  par(pardefault)
}