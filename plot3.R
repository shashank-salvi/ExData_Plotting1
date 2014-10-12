# Required for reading data from file
library(data.table)

## Read HouseHold Power Consumption Data for period 1-Feb-2007 to 2-Feb-2007
readFEBHPCData<- function(){
  completeHPCData <- fread("household_power_consumption.txt",header=T,sep=";",verbose=F);
  completeHPCData <- as.data.frame(completeHPCData);
  
  data1 <- completeHPCData[,"Date"]=="1/2/2007";
  data2 <- completeHPCData[,"Date"]=="2/2/2007";
  tempData <- rbind(completeHPCData[data1,],completeHPCData[data2,]);
  
  numCol <- ncol(tempData);
  
  ## Removing records containing any "?"
  for(index in seq_len(numCol)){
    qVec <- which(tempData[,index]=="?")
    if(length(qVec)>0)
      tempData<-tempData[-qVec,];
  }
  
  tempData$Time <- strptime(paste(tempData$Date,tempData$Time),format="%d/%m/%Y %H:%M:%S");
  tempData$Date <- as.Date(tempData$Date,format="%d/%m/%Y");
  
  for(index in 3:9)
    class(tempData[,index]) <- "numeric";
  
  return(tempData);
}

febHPCData <- readFEBHPCData();

## Plot 3
png("plot3.png");
plot(x=febHPCData[,"Time"],y=febHPCData[,"Sub_metering_1"],xlab="",ylab="Energy sub metering",type="l");
lines(x=febHPCData[,"Time"],y=febHPCData[,"Sub_metering_2"],col="red");
lines(x=febHPCData[,"Time"],y=febHPCData[,"Sub_metering_3"],col="blue");
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=1)
dev.off();