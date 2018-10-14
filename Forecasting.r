
### Thesis Starts Here

### Get Libraries
#install.packages('astsa')
#install.packages('forecast')

library(astsa)
library(forecast)
library(xlsx)
library(openxlsx)
library(data.table)
library(readxl)
library(dplyr)
library(readxl)
library(ggplot2)
library(metrics)

##### Parameters Session
FirstWeek <-10         #Data starts from week 25 2016
LastWeek    <-30       #Last week of the whole dataset
LastWeekTest <-10       #Last Week of the Test Dataset
FOrecastingLength <-LastWeekTest  #Foreacsting length on the Test
Metric <-"Allqty"   #switch between  Allrevenues and Allqty

#### Create Folder Structure
now<-Sys.time()
OutDir <- "C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Output"
ModelDir<-paste0(format(now, "%Y%m%d_%H%M%S"))
dir.create(file.path(OutDir,(ModelDir)))

ChartDir<-file.path(ModelDir,"Charts")
dir.create(file.path(OutDir,(ChartDir)))

FilesDir<-file.path(ModelDir,"Files")
dir.create(file.path(OutDir,(FilesDir)))

### Import Session
Sys.setlocale(category = "LC_ALL", locale = "Greek")
setwd("C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Input")

ThesisData <-fread("databeverages09291.csv", sep=",",header = T,encoding="UTF-8")
ThesisData<- ThesisData[ ,  .(Allqty=sum(Allqty),
                              Metric=sum(get(Metric))),
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

Week1<- data.frame("Year" = 2016,"Week" = ((FirstWeek:52)))
Week2<- data.frame("Year" = 2017,"Week" = ((1:52)))
Week3<- data.frame("Year" = 2018,"Week" = ((1:LastWeek)))

Week<-rbind(Week1,Week2,Week3)
Week<-data.table(Week)

x<-unique(ThesisData$Cust_Point_UniqueId)
y<-unique(ThesisData$BrandName)

#Define Dimensions 
n1<-length(x)*length(y)
n2<- nrow(Week)

Fitted<-data.frame()
ForecastStartd<-data.frame()
ForecastStart<-numeric()
ParametersModel <-numeric()
#Set up Goodnes of fit Functions
R2 <- function(actual,predict){
  round(1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2)),2)
}
my_RMSE<-function(actual,predict){round(sqrt(mean((predict -actual)^2)),0)}


mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Charts"
                    ,paste("Individual_Fit_Charts.pdf"))
pdf(file=mypath1)

for ( j in x)
  for ( i in y)
    {
      {
        Temp<-ThesisData[Cust_Point_UniqueId==j & BrandName==i & (week > FirstWeek-1 | year>2016),] 
        if(nrow(Temp) ==0){next}
        t<- merge(x=Week, y=Temp, by.x=c("Year","Week"),by.y=c("year","week")   , all.x=TRUE)
        t[is.na(t$Cust_Point_UniqueId),"Cust_Point_UniqueId"]=unique(Temp$Cust_Point_UniqueId)
        t[is.na(t$Cust_Description),"Cust_Description"]=unique(Temp$Cust_Description)
        t[is.na(t$Cust_RPS_Name),"Cust_RPS_Name"]=unique(Temp$Cust_RPS_Name)
        t[is.na(t$Loc_Ael_1),"Loc_Ael_1"]=unique(Temp$Loc_Ael_1)
        t[is.na(t$BrandID),"BrandID"]=unique(Temp$BrandID)
        t[is.na(t$BrandName),"BrandName"]=unique(Temp$BrandName)
        t[is.na(t$Prod_CatID),"Prod_CatID"]=unique(Temp$Prod_CatID)
        #t[is.na(t$Allqty),"Allqty"]=0
        t[is.na(t$Metric),Metric]=0
        
        print(paste("Customer =" ,j, "Brand =" , i))
        ThesisDataTime<-ts(t[,Metric],start=c(2016,FirstWeek),frequency=52)
        Model<-auto.arima(ThesisDataTime)
        summary(Model)
        Fit<-ThesisDataTime-residuals(Model)
        
        Rsq<-R2(Model$x,Model$fitted)
        Rmse<-my_RMSE(Model$x,Model$fitted)
        
        #Output Fit charts
      
          ts.plot(ThesisDataTime, main=paste("Fit Chart Customer =" ,j, "Brand =" , i),
                  sub=paste("Total Rsq=" ,Rsq ,"Rmse = ", Rmse))
          points(Fit,type="l",col="red")

         #Strore Fitted and Actual Values
         Fitted1<-data.frame(Actual=as.numeric(Model$x),Fit=as.numeric(Model$fitted),Outlet=j,Brand=i)
         Fitted1$ID <- seq.int(nrow(Fitted1))
         Fitted<-rbind(Fitted,Fitted1)
         
         #### Repeat Exercise in Test and Validation Dataset
         
         Model1<-auto.arima(ts(ThesisDataTime[1:(length(ThesisDataTime)-LastWeekTest)],start=c(2016,FirstWeek),frequency=52))
         fcast_no_holdout <- forecast(Model1,h=FOrecastingLength)
         
         ParametersModel1<-arimaorder(Model1)
         ParametersModel<-rbind(ParametersModel,ParametersModel1)
         
         ForecastStartd<-rbind(ForecastStartd,ForecastStartd1)
         
         plot(fcast_no_holdout, main=paste("Forecasting Customer =" ,j, "Brand =" , i))
         lines(ThesisDataTime)
        
          Forecast1<-as.numeric(fcast_no_holdout$mean)
          ForecastStart<-rbind(ForecastStart,Forecast1)
          
          ForecastStartd1<-data.frame(Fcast=as.numeric(fcast_no_holdout$mean),
                                      Outlet=j,Brand=i)
          
          ForecastStartd1$ID <- seq.int(nrow(ForecastStartd1))+n2-FOrecastingLength
          ForecastStartd<-rbind(ForecastStartd,ForecastStartd1)
         
      }
  } 
dev.off()

#Merge Fitted With Forecast
Fitted<-merge(x=Fitted,y=ForecastStartd,by=c("ID","Outlet","Brand") ,all=TRUE)
Fitted[is.na(Fitted)] <- 0

Fitted<- data.table(Fitted)
FittedOutlet<- Fitted[ ,  .(Actual=sum(Actual),
                            Fit=sum(Fit),
                            Fcast=sum( Fcast)),
                               by = .(Outlet, ID) ]

FittedBrand<- Fitted[ ,  .(Actual=sum(Actual),
                            Fit=sum(Fit),
                           Fcast=sum( Fcast)),
                       by = .(Brand, ID) ]


FittedTotal<- Fitted[ ,  .(Actual=sum(Actual),
                           Fit=sum(Fit),
                           Fcast=sum( Fcast)),
                         by = .( ID) ]



mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Charts"
                     ,paste("Outlet_Fit_Charts.pdf"))
pdf(file=mypath1)

#Output Total Outlet Fit
for ( j in x){
  print(j)
  FittedOutletInd<- FittedOutlet[Outlet==j]
  
  Rsq<-R2(FittedOutletInd$Actual,FittedOutletInd$Fit)
  Rmse<-my_RMSE(FittedOutletInd$Actual,FittedOutletInd$Fit)
  
  print(ggplot(FittedOutletInd,aes(x=ID))+geom_line(aes(y=Actual)) +
  geom_line(aes(y=Fit,colour="Fit"))+  geom_line(aes(y=Fcast,colour="Fcast"))+
    ggtitle(paste("Outlet = " , j 
                                                    ,"Rsq = " ,Rsq ,"Rmse = ", Rmse)))
 
}

dev.off() 
#Output Total Brand Fit
mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Charts"
                     ,paste("Brand_Fit_Charts.pdf"))
pdf(file=mypath1)

for ( j in y){
  print(j)
  FittedBrandInd<- FittedBrand[Brand==j]
  
  Rsq<-R2(FittedBrandInd$Actual,FittedBrandInd$Fit)
  Rmse<-my_RMSE(FittedBrandInd$Actual,FittedBrandInd$Fit)
  
  print(ggplot(FittedBrandInd,aes(x=ID))+geom_line(aes(y=Actual)) +
          geom_line(aes(y=Fit,colour="Fit"))+geom_line(aes(y=Fcast,colour="Fcast"))+
          ggtitle(paste("Brand = " , j 
                                                            ,"Rsq = " ,Rsq ,"Rmse = ", Rmse)))
  
}

dev.off() 
#Output Total Fit

mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Charts"
                     ,paste("Grant Total_Fit_Charts.pdf"))
pdf(file=mypath1)

  Rsq<-R2(FittedTotal$Actual,FittedTotal$Fit)
  Rmse<-my_RMSE(FittedTotal$Actual,FittedTotal$Fit)
  
  ggplot(FittedTotal,aes(x=ID))+geom_line(aes(y=Actual)) +
  geom_line(aes(y=Fit,colour="Fit"))+ geom_line(aes(y=Fcast,colour="Fcast"))+
    ggtitle(paste("Grant Total Rsq=" ,Rsq ,"Rmse = ", Rmse))

dev.off()

#### Run Forecasting At Grant total
mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Charts"
                     ,paste("Grant Total_Fit_Charts_Direct.pdf"))
pdf(file=mypath1)

FittedTotalTime<-ts(FittedTotal[,Actual],start=c(2016,FirstWeek),frequency=52)
Model<-auto.arima(FittedTotalTime)
summary(FittedTotalTime)
Fit<-FittedTotalTime-residuals(Model)

Rsq<-R2(Model$x,Model$fitted)
Rmse<-my_RMSE(Model$x,Model$fitted)

ts.plot(FittedTotalTime, main=paste("GrantTotal"),
        sub=paste("Total Rsq=" ,Rsq ,"Rmse = ", Rmse))
points(Fit,type="l",col="red")

Model1<-auto.arima(ts(FittedTotalTime[1:(length(FittedTotalTime)-LastWeekTest)],start=c(2016,FirstWeek),frequency=52))
fcast_no_holdout <- forecast(Model1,h=FOrecastingLength)

plot(fcast_no_holdout, main=paste("Forecasting GrantTotal"))
lines(FittedTotalTime)

dev.off()

### Compare Grant Total vs Bottom up (Do we need to do the same for Brand/Outlet Totals/)

ExportPath<-file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Files","Fitted.csv")
write.csv(Fitted, ExportPath ,row.names = FALSE)

ExportPath<-file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Files","ForecastStartd.csv")
write.csv(ForecastStartd, ExportPath ,row.names = FALSE)

ExportPath<-file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output",ModelDir,"Files","ParametersModel.csv")
write.csv(ParametersModel, ExportPath ,row.names = FALSE)







#work



Temp<-ThesisData[Cust_Point_UniqueId=="ip10028" & BrandName=="COCA COLA"& (week > FirstWeek-1 | year>2016),] 

if(nrow(Temp) ==0){next}
t<- merge(x=Week, y=Temp, by.x=c("Year","Week"),by.y=c("year","week")   , all.x=TRUE)
t[is.na(t$Cust_Point_UniqueId),"Cust_Point_UniqueId"]=unique(Temp$Cust_Point_UniqueId)
t[is.na(t$Cust_Description),"Cust_Description"]=unique(Temp$Cust_Description)
t[is.na(t$Cust_RPS_Name),"Cust_RPS_Name"]=unique(Temp$Cust_RPS_Name)
t[is.na(t$Loc_Ael_1),"Loc_Ael_1"]=unique(Temp$Loc_Ael_1)
t[is.na(t$BrandID),"BrandID"]=unique(Temp$BrandID)
t[is.na(t$BrandName),"BrandName"]=unique(Temp$BrandName)
t[is.na(t$Prod_CatID),"Prod_CatID"]=unique(Temp$Prod_CatID)
t[is.na(t$Allqty),"Allqty"]=0
t[is.na(t$Allrevenues),"Allrevenues"]=0


ThesisDataTime<-ts(t[,Allqty],start=c(2016,31),frequency=52)
Model<-auto.arima(ThesisDataTime)
summary(Model)
Fit<-ThesisDataTime-residuals(Model)



print(data.frame(Temp))
