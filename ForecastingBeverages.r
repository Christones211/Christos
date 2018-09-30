
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
library(ggplot2)
library(metrics)


### Import Session
Sys.setlocale(category = "LC_ALL", locale = "Greek")
setwd("C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Input")

ThesisData <-fread("databeverages09291.csv", sep=",",header = T,encoding="UTF-8")
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

Fitted<-data.frame()
ForecastStart<-numeric()

#Set up Goodnes of fit Functions
R2 <- function(actual,predict){
  round(1 - (sum((actual-predict )^2)/sum((actual-mean(actual))^2)),2)
}
my_RMSE<-function(actual,predict){round(sqrt(mean((predict -actual)^2)),0)}


mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output","Charts"
                    ,paste("Individual_Fit_Charts.pdf"))
pdf(file=mypath1)

for ( j in x)
  for ( i in y)
    {
      {
        Temp<-ThesisData[Cust_Point_UniqueId==j & BrandName==i,] 
        if(nrow(Temp) ==0){next}
        t<- merge(x=Week, y=Temp, by.x=c("Year","Week"),by.y=c("year","week")   , all.x=TRUE)
        t[is.na(t$Cust_Point_UniqueId),"Cust_Point_UniqueId"]=unique(Temp$Cust_Point_UniqueId)
        t[is.na(t$Cust_Description),"Cust_Description"]=unique(Temp$Cust_Description)
        t[is.na(t$Cust_RPS_Name),"Cust_RPS_Name"]=unique(Temp$Cust_RPS_Name)
        t[is.na(t$Loc_Ael_1),"Loc_Ael_1"]=unique(Temp$Loc_Ael_1)
        t[is.na(t$BrandID),"BrandID"]=unique(Temp$BrandID)
        t[is.na(t$BrandName),"BrandName"]=unique(Temp$BrandName)
        #t[is.na(t$Man_Name),"Man_Name"]=unique(Temp$Man_Name)
        t[is.na(t$Prod_CatID),"Prod_CatID"]=unique(Temp$Prod_CatID)
        t[is.na(t$Allqty),"Allqty"]=0
        t[is.na(t$Allrevenues),"Allrevenues"]=0
        
        print(paste("Customer =" ,j, "Brand =" , i))
        ThesisDataTime<-ts(t[,Allqty],start=c(2016,31),frequency=52)
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

         Model1<-auto.arima(ts(ThesisDataTime[1:(length(ThesisDataTime)-5)],start=c(2016,31),frequency=52))
         fcast_no_holdout <- forecast(Model1,h=5)
         
         plot(fcast_no_holdout, main=paste("Forecasting Customer =" ,j, "Brand =" , i))
         lines(ThesisDataTime)
        
          Forecast1<-as.numeric(fcast_no_holdout$mean)
          ForecastStart<-rbind(ForecastStart,Forecast1)
         
      }
  } 
dev.off()

Fitted<- data.table(Fitted)
FittedOutlet<- Fitted[ ,  .(Actual=sum(Actual),
                            Fit=sum(Fit)),
                               by = .(Outlet, ID) ]

FittedBrand<- Fitted[ ,  .(Actual=sum(Actual),
                            Fit=sum(Fit)),
                       by = .(Brand, ID) ]


FittedTotal<- Fitted[ ,  .(Actual=sum(Actual),
                           Fit=sum(Fit)),
                         by = .( ID) ]

mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output","Charts"
                     ,paste("Outlet_Fit_Charts.pdf"))
pdf(file=mypath1)

#Output Total Outlet Fit
for ( j in x){
  print(j)
  FittedOutletInd<- FittedOutlet[Outlet==j]
  
  Rsq<-R2(FittedOutletInd$Actual,FittedOutletInd$Fit)
  Rmse<-my_RMSE(FittedOutletInd$Actual,FittedOutletInd$Fit)
  
  print(ggplot(FittedOutletInd,aes(x=ID))+geom_line(aes(y=Actual)) +
  geom_line(aes(y=Fit,colour="red"))+ ggtitle(paste("Outlet = " , j 
                                                    ,"Rsq = " ,Rsq ,"Rmse = ", Rmse)))
 
}

dev.off() 
#Output Total Brand Fit
mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output","Charts"
                     ,paste("Brand_Fit_Charts.pdf"))
pdf(file=mypath1)

for ( j in y){
  print(j)
  FittedBrandInd<- FittedBrand[Brand==j]
  
  Rsq<-R2(FittedBrandInd$Actual,FittedBrandInd$Fit)
  Rmse<-my_RMSE(FittedBrandInd$Actual,FittedBrandInd$Fit)
  
  print(ggplot(FittedBrandInd,aes(x=ID))+geom_line(aes(y=Actual)) +
          geom_line(aes(y=Fit,colour="red"))+ ggtitle(paste("Brand = " , j 
                                                            ,"Rsq = " ,Rsq ,"Rmse = ", Rmse)))
  
}

dev.off() 
#Output Total Fit

mypath1 <- file.path("C:","Users","ctripodis","Desktop","AllFiles","AUEB","Thesis","Output","Charts"
                     ,paste("Total_Fit_Charts.pdf"))
pdf(file=mypath1)

  Rsq<-R2(FittedTotal$Actual,FittedTotal$Fit)
  Rmse<-my_RMSE(FittedTotal$Actual,FittedTotal$Fit)
  
  ggplot(FittedTotal,aes(x=ID))+geom_line(aes(y=Actual)) +
  geom_line(aes(y=Fit,colour="red"))+ ggtitle(paste("Total Outlets Brandds Rsq=" ,Rsq ,"Rmse = ", Rmse))

dev.off()


write.csv(Fitted,"C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Output\\Files\\Fitted.csv",row.names = FALSE)
write.csv(ForecastStart,"C:\\Users\\ctripodis\\Desktop\\AllFiles\\AUEB\\Thesis\\Output\\Files\\Forecast.csv",row.names = FALSE)

#Calculate totals Level 1 Leve2












