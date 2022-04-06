rm(list=ls())
#install.packages("pacman")

library(pacman)
pacman::p_load(data.table, dplyr, lubridate, readxl)

#1. There are 9 energy commodities: Coal, Crude Oil (CL), Ethanol (EH),
#Emissions (EA), Natural Gas (NG), and Refined Products

#2. (a)
library(readxl)
futures_daily <- read_excel("Data_CL_and_ES.xlsx")
#head(futures_daily)
futures_weekly <- futures_daily %>% as.data.frame %>% mutate(weekday = wday(Date), year = (Date)) %>% filter(weekday == 2, CL1 > 0)
#head(futures_weekly)
#view(futures_weekly)
ggplot(futures_weekly,aes(x=Date,y=lead(CL1)))+
  geom_line()

#2(b) 
#log the values
ln_sd1<-log(futures_weekly%>%select(2:25))
#find the percentage difference
ln_sd3<-data.frame(sapply(ln_sd1,diff,lead=1))
#standard deviation
ln_sd4<-sapply(ln_sd3,sd)
tenor<-c(1:24)
#annualized
annualized_ln_sd<-ln_sd4*sqrt(52)
twob<-data.frame(tenor,ln_sd4,annualized_ln_sd)
ggplot(twob,aes(tenor))+
  geom_line(aes(y=ln_sd4),colour="pink") +
  geom_line(aes(y=annualized_ln_sd),colour="purple")
#twob

#2(c)
#i
ln_sd_year<-futures_weekly%>%select(2,28)%>%group_by(year)%>%
  mutate(sd_f=sd(diff(log(CL1),lead=1)),mean_f=mean(CL1),
         annualized_sd_f=sd(diff(log(CL1),lead=1))*sqrt(52))
#ln_sd_year ？？？？？

#2(d)
#i.
futures_curve<-futures_weekly%>%select(1:25)%>%filter(row_number()==946)
futures_curve1<-as.numeric(t(futures_curve)[-1,])
feb_14_2022<-data.frame(tenor,futures_curve1)
plot(feb_14_2022$tenor,feb_14_2022$futures_curve1)

slope<-(log(feb_14_2022$futures_curve1[24])-log(feb_14_2022$futures_curve1[1]))/
  (feb_14_2022$tenor[24]-feb_14_2022$tenor[1])
intercept<-log(feb_14_2022$futures_curve1[1])+(slope*feb_14_2022$tenor[1])
plot(feb_14_2022$tenor,log(feb_14_2022$futures_curve1))
abline(a=intercept,b=slope)
#Implicit spot price is 4.54. The model fits the data well

#ii.
reg<-lm(log(feb_14_2022$futures_curve1)~feb_14_2022$tenor)
plot(feb_14_2022$tenor,log(feb_14_2022$futures_curve1))
abline(reg)
#Implicit spot price is 4.51, it fits better than in part i


#3.

hedging <- function(x){
  gain <- rep(0,1000)
  strike <- 0:999
  for (i in 0:999){
    gain[i+1] <- 500-i
  }
  pl <- data.frame(strike,gain)
  return (pl)
}
Q3_dataframe <- hedging(x)
Q3plot <- ggplot(data=Q3_dataframe,aes(x=strike,y=gain,group=1)) +geom_line(color='pink')

#firms can the  shield the desired sale price by using futures contracts. 
#The actual produce is sold at available market rates, but the fluctuation in prices is 
#eliminated by the futures contract.
#pro: reduce risk, locking profit, survive hard period
#con: cost can eat up benefit, reducing risk means reducing benefit, difficult for short-term traders


#4
Q4<-futures_weekly%>%select(1,2,26)%>%filter(year(DATES)>2019)
cl1<-diff(part4$CL1)/part4$CL1[-1]
es1<-diff(part4$ES1)/part4$ES1[-1]
reg<-lm(cl1~es1)
h<-reg$coefficients[2]
N_contract<-h*(92.1/4222)*(1000000/50)
N_contract

#b. The company should buy 211 futures contracts today to hedge




