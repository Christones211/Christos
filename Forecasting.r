
### Thesis Starts Here

### Get Libraries
install.packages('astsa')
install.packages('forecast')

library(astsa)
library(forecast)
library(xlsx)
library(openxlsx)
library(data.table)
library(readxl)
library(dplyr)
library(readxl)

### Import Session
Sys.setlocale(category = "LC_ALL", locale = "Greek")
setwd("C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Input")

ThesisData <-fread("DataBeers.csv", sep=",",header = T,encoding="UTF-8")
ThesisData<- ThesisData[ ,  .(Allqty=sum(Allqty),
                              Allrevenues=sum(Allrevenues)),
                     by = .(Cust_Point_UniqueId,Cust_Description,Cust_RPS_Name
                            ,Loc_Ael_1,week,year,BrandID	,BrandName,Prod_CatID) ]

Brands<-read.xlsx("Top20Brands.xlsx",sheet ="PickBrand", fill=TRUE )
Outlets <-read.xlsx("CUSTOMER.xlsx",sheet ="PickPoints", fill=TRUE )
Brands<-data.table(Brands)
Outlets<-data.table(Outlets)

### Pick Brands outlets based on input daya
setkey(Brands,BrandName)
setkey(Outlets,Outlet)
setkey(ThesisData,BrandName)

ThesisData<-ThesisData[Brands[,'BrandName'], nomatch=0]

setkey(ThesisData,Cust_Point_UniqueId)
ThesisData<-ThesisData[Outlets, nomatch=0]
ThesisData<-ThesisData[order(Cust_Point_UniqueId,year,week)]

# Create Cartesian Week for Weeks with Zero selling

Week1<- data.frame("Year" = 2016,"Week" = ((1:52)))
Week2<- data.frame("Year" = 2017,"Week" = ((1:52)))
Week3<- data.frame("Year" = 2018,"Week" = ((1:30)))

Week<-rbind(Week1,Week2,Week3)
Week<-data.table(Week)

x<-unique(ThesisData$Cust_Point_UniqueId)
y<-unique(ThesisData$BrandName)

#Define Dimensions 
n1<-length(x)*length(y)
n2<- nrow(Week)

Fitted<-data.frame(matrix(nrow=n2,ncol=1))
Actual<-data.frame(matrix(nrow=n2,ncol=1))
ForecastStart<-numeric()

dev.off() 
for ( j in x)
  for ( i in y)
    {
      {
        Temp<-ThesisData[Cust_Point_UniqueId==j & BrandName==i,] 
       # Temp<-ThesisData[Cust_Point_UniqueId=='ip10030' & BrandName=='FIX',] 
        t<- merge(x=Week, y=Temp, by.x=c("Year","Week"),by.y=c("year","week")   , all.x=TRUE)
        
        t[is.na(t$Cust_Point_UniqueId),"Cust_Point_UniqueId"]=unique(Temp$Cust_Point_UniqueId)
        t[is.na(t$Cust_Description),"Cust_Description"]=unique(Temp$Cust_Description)
        t[is.na(t$Cust_RPS_Name),"Cust_RPS_Name"]=unique(Temp$Cust_RPS_Name)
        t[is.na(t$Loc_Ael_1),"Loc_Ael_1"]=unique(Temp$Loc_Ael_1)
        t[is.na(t$BrandID),"BrandID"]=unique(Temp$BrandID)
        t[is.na(t$BrandName),"BrandName"]=unique(Temp$BrandName)
      #  t[is.na(t$Man_Name),"Man_Name"]=unique(Temp$Man_Name)
        t[is.na(t$Prod_CatID),"Prod_CatID"]=unique(Temp$Prod_CatID)
        t[is.na(t$Allqty),"Allqty"]=0
        t[is.na(t$Allrevenues),"Allrevenues"]=0
        
        
        print(paste("Customer =" ,j, "Brand =" , i))
        ThesisDataTime<-ts(t[,Allqty],start=c(2016,31),frequency=52)
        Model<-auto.arima(ThesisDataTime)
        summary(Model)
        Fit<-ThesisDataTime-residuals(Model)
        
        pdf(paste("Fitt Customer =" ,j, "Brand =" , i,".pdf"))
          ts.plot(ThesisDataTime)
          points(Fit,type="l",col="red")
        dev.off()
        
          fcast <- forecast(Model, h=20)
       
         pdf(paste("Forecast Customer =" ,j, "Brand =" , i,".pdf"))
          plot(fcast, sub=paste("Customer =" ,j, "Brand =" , i))
         dev.off()
         #Strore Fitted and Actual Values
         Fitted1<-data.frame(Model$fitted)
         Fitted<-cbind(Fitted,Fitted1)
         
         Actual1<-data.frame(Model$x)
         Actual<-cbind(Actual,Actual1)
         
         #### Repeat Exercise in Test and Validation Dataset
         
         Model1<-auto.arima(ts(ThesisDataTime[1:(length(ThesisDataTime)-5)],start=c(2016,31),frequency=52))
         fcast_no_holdout <- forecast(Model1,h=5)
         
         pdf(paste("Forecast In Test Train Dataset Customer =" ,j, "Brand =" , i,".pdf"))
           
          plot(fcast_no_holdout, main=" ")
          lines(ThesisDataTime)
          
          dev.off()
          
          Forecast1<-as.numeric(fcast_no_holdout$mean)
          ForecastStart<-rbind(ForecastStart,Forecast1)
         
      }
  } 


plot(fcast)
dev.off()
write.csv(Fitted,"Fitted.csv",row.names = FALSE)
write.csv(Actual,"Actual.csv",row.names = FALSE)
write.csv(ForecastStart,"ForecastStart.csv",row.names = FALSE)


PmiDataTime<-ts(Temp[,Allqty],start=c(2016,31),frequency=52)
plot(PmiDataTime)
acf((PmiDataTime))
pacf((PmiDataTime))

Model1<-auto.arima(PmiDataTime)

Fit<-PmiDataTime-residuals(Model1)
ts.plot(PmiDataTime)
points(Fit,type="l",col="red")
Model1 %>% forecast(h=30) %>% autoplot()


############################################################
# Working Session
############################################################
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data
dataTime = ts(data[,2],start = c(2003,1),frequency = 12)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

#####################SOM forecast
#####################

library(RODBC) 
library(xlsx)
library(openxlsx)
library(data.table)
library(readxl)

SOM         <-read.xlsx("C:\\Users\\ctripodis\\Documents\\PMI\\ToPMI\\HeetsResults091918.xlsx",sheet ="2018", fill=TRUE )
SOM<- data.table(SOM)
SOM<-SOM[location=="Athens",c("Market.share","Flag1")]
SOM[is.na(SOM$Flag1),"Flag1"]=0
SOM[is.na(SOM$Flag2),"Flag2"]=0

fc<-ses(SOMTime,h=5)
summary(fc)

SOMTime<-ts(SOM,start=1,end = 30)
plot(SOMTime)
plot(diff(SOMTime))

fc1<-holt(SOMTime,h=22)
fc2<-holt(SOMTime,damped=TRUE,h=7)
fc1

SomModel<-auto.arima(SOMTime[,"Market.share"],xreg = SOMTime[,"Flag1"])
SomModel
ts.plot(SOM)
points(SomModel$fitted,type="l",col="red")
fcast <- forecast(SomModel, xreg=rep(1,1) )

plot(fcast)
fcast
acf2(diff(SOMTime))


##########################
x<-unique(ThesisData$Cust_Point_UniqueId)
for ( j in x){
  Temp<-ThesisData[Cust_Point_UniqueId=='ip10038' & BrandID==448,] 
  
 t<- merge(x=Week, y=Temp, by.x=c("Year","Week"),by.y=c("year","week")   , all.x=TRUE)
 t[is.na(t$Cust_Point_UniqueId),"Cust_Point_UniqueId"]=unique(Temp$Cust_Point_UniqueId)
 t[is.na(t$Cust_Description),"Cust_Description"]=unique(Temp$Cust_Description)
 t[is.na(t$Cust_RPS_Name),"Cust_RPS_Name"]=unique(Temp$Cust_RPS_Name)
 t[is.na(t$Loc_Ael_1),"Loc_Ael_1"]=unique(Temp$Loc_Ael_1)
 t[is.na(t$BrandID),"BrandID"]=unique(Temp$BrandID)
 t[is.na(t$BrandName),"BrandName"]=unique(Temp$BrandName)
 t[is.na(t$Man_Name),"Man_Name"]=unique(Temp$Man_Name)
 t[is.na(t$Prod_CatID),"Prod_CatID"]=unique(Temp$Prod_CatID)
 t[is.na(t$Allqty),"Allqty"]=0
 t[is.na(t$Allrevenues),"Allrevenues"]=0

 
 write.csv(Temp,"t.csv",row.names = FALSE)
 
  print(j)
  ThesisDataTime<-ts(Temp[,Allqty],start=c(2016,31),frequency=52)
  Model<-auto.arima(ThesisDataTime)
  Model1<-auto.arima(ts(ThesisDataTime[1:(length(ThesisDataTime)-5)],start=c(2016,31),frequency=52))
  fcast_no_holdout <- forecast(Model1,h=5)
  #ts.plot(ThesisDataTime[1:(length(ThesisDataTime)-5)])
  plot(fcast_no_holdout, main=" ")
  lines(ThesisDataTime)
  
  
  summary(Model)
  Fit<-ThesisDataTime-residuals(Model)
  ts.plot(ThesisDataTime)
  points(Fit,type="l",col="red")
  fcast <- forecast(Model, h=20)
Model$fitted
    plot(fcast,sub="sub-title" )
  Model %>% forecast(h=30) %>% autoplot()
}




PmiDataTime<-ts(Temp[,Allqty],start=c(2016,31),frequency=52)
plot(PmiDataTime)
plot(diff(PmiDataTime))
acf((PmiDataTime))
pacf((PmiDataTime))
acf2(PmiDataTime,max.lag=52)

acf(diff(PmiDataTime),max.lag=52)
pacf(diff(PmiDataTime),max.lag=52)

Model1<-auto.arima(PmiDataTime)

Fit<-PmiDataTime-residuals(Model1)
ts.plot(PmiDataTime)
points(Fit,type="l",col="red")
Model1 %>% forecast(h=30) %>% autoplot()

i1<-which(x==x[i])

Actual[,i]<-Model$fitted

i1<-which(x==x[i])
Actual[,i1]<-Model$fitted

############
SOMData <-fread("C:\\Users\\ctripodis\\Documents\\PMI\\DataPulls\\SOMForecasting\\SOM.csv", sep=",",header = T,encoding="UTF-8")
SOM<- data.table(SOMData)
SOM<-SOM[location=="Other","Marketshareincecomm"]
SOM$Marketshareincecomm<-as.numeric(SOM$Marketshareincecomm)
SOMTime<-ts(SOM,start=1,end = 9)

Model1<-auto.arima(SOMTime)
Fit<-SOMTime-residuals(Model1)
ts.plot(SOMTime)
points(Fit,type="l",col="red")
fcast <- forecast(Model1, h=3)
plot(fcast)
fcast$fitted
fcast


