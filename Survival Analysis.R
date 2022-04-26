library(survival)
library("readxl")
library(forecast)
library(mice)
getwd()
setwd("C:/Users/Aman Bhargava/OneDrive - purdue.edu/Desktop/Purdue/690 - Industry Practicum/Data")
mdata <- read.csv("FinalWait.csv")

imputedValues <- mice(data=mdata
                      , seed=2016     # keep to repicate results
                      , method="cart" # model you want to use
                      , m=1           # Number of multiple imputations
                      , maxit = 1     # number of iterations
)

# impute the missing values in our tr data.frame
mdata <- mice::complete(imputedValues,1) # completely fills in the missing
view(mdata)

#  KM survival curve for all data. 
fit.all<-survfit(Surv(Final_wait_time,Event==1)~1,data=mdata) # analyze w/ all data
plot(fit.all)
plot(fit.all,col=3, main='Willingness to wait') # make plot more readable

# analyze KM survivals by part type and add color and legend to make the chart more readable.
fit.part<-survfit(Surv(Final_wait_time,Event)~PART_TYPE,data=mdata)
plot(fit.part,col=2:7,lty=1:6,  ylab = 'Customer willing to wait', xlab = 'Hours', xlim=c(0,20), main="Willingness to wait by Part Type")
llabel<-gsub("x=","",names(fit.part$strata))
legend("topright",legend=llabel,col=2:7,lty=1:6,bty='n')

#Significant variables
mdata$PART_TYPE<-as.factor(mdata$PART_TYPE)
fit.cox<- coxph(Surv(Final_wait_time,Event)~SKU_NUMBER+STORE_NUMBER+PART_TYPE+POP_EST_CY+POP_DENSITY_CY+PCST_WHITE+AGE+PCT_COLLEGE+PCT_BLUE_COLLAR,data=mdata)
fit.cox

fit.cox1<- coxph(Surv(Final_wait_time,Event)~MEDIAN_HOUSEHOLD_INCOME+ESTABLISHMENTS+SKU_EXISTENCE_PY+SKU_EXISTENCE_CY+SKU_STORE_PDQ_PY+SKU_STORE_PDQ_CY+LOST_QTY_PY+LOST_QTY_CY+AVG_CLUSTER_UNIT_SALES_PY+AVG_CLUSTER_UNIT_SALES_CY+AVG_CLUSTER_LOST_SALES_PY+AVG_CLUSTER_LOST_SALES_CY,data=mdata)
fit.cox1

fit.cox2<- coxph(Surv(Final_wait_time,Event)~ROAD_QUALITY_INDEX+VIO_COMPARED_TO_CLUSTER_CY+ADJUSTED_AVG_CLUSTER_SALES_CY+QTY_SOLD_CY+AVG_CLUSTER_TOTAL_SALES_CY+ADJUSTED_LIFECYCLE_CY+PCT_OF_LIFECYCLE_REMAINING+ADJ_AVG_CLUSTER_LOST_SALES_CY+ADJ_AVG_CLUSTER_TOTAL_SALES_CY,data=mdata)
fit.cox2

fit.cox3<- coxph(Surv(Final_wait_time,Event)~SOLD_SINCE_MAXI+cy_gross_sales+cy_sales_cost+OTHER_UNIT_PLS_LOST_SALES_CY+WEIGHTED_LOOKUP_CNT_CY+CY_PERIODS_IN_STOCK+MPOG_ID+cy_qty_sold_transfer+cy_qty_sold_on_hand+fy_ts_forecast,data=mdata)
fit.cox3
