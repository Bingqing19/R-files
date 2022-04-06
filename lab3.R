rm(list=ls())

#install.packages('dplyr')
require(ggplot2)
require(dplyr)

#1.

coupon_bond_price <- function(f, c, y, t){
  couponPay = c * f
  couponBond = couponPay * ((1 - 1/(1+y)^t) /y) + f/(1+y)^t  
  return(couponBond)
}

coupon_bond_price(1000, 0.04, 0.08, 10)


#(a)

coupon_rate_chart <- function(f, c, y, t){
  cLength <- length(c)
  bondPrice <- rep(0,cLength)
  for (i in 1: cLength) {
    bondPrice[i] <- coupon_bond_price(f, c[i], y, t)
  }
  datafCoup <- data.frame(c, bondPrice)
  c_chart <- ggplot(data=datafCoup, aes(x=c, y=bondPrice, group=1))+ geom_line()
  return(c_chart)
}

coupon_rate_chart(1000, c(0.01,0.02,0.03), 0.06, 9)



#(b) 

t_chart <- function(f, c, y, t){
  tVector <- 1:t
  bondPrice <- rep(0,t)
  for (i in 1: t) {
    bondPrice[i] <- coupon_bond_price(f, c, y, i)
  }
  dataft <- data.frame(tVector, bondPrice)
  t_chart <- ggplot(data=dataft, aes(x=tVector, y=bondPrice, group=1))+ geom_line()
  return(t_chart)
}

t_chart(1000, 0.05, 0.06, 5)


#(c)

data_frame1c <- function(f, c, y, t){
  cashflow <- rep(c*f, t)
  cashflow[t] <- c*f+f
  pv_factor <- rep(0,t)
  pv <- rep(0,t)
  for(i in 1: t){
    pv[i] <- (cashflow[i]/((1+y)^i))
    pv_factor[i] <- (pv[i] / cashflow[i])
  }
  dataFrame0 <- data.frame(cashflow, pv_factor, pv)
  return(dataFrame0)
}

data_frame1c(100, 0.05, 0.02, 10)

#2  A coupon-bond that pays interest of $100 annually has a par value of $1,000, matures in 6
# years, and is selling today at $65
#????? 65 or 650

#(a)

data_frame2a <- function(f, c, y, t){
  yLength <- length(y)
  bondPrice <- rep(0,yLength)
  for (i in 1: yLength) {
    bondPrice[i] <- coupon_bond_price(f, c, y[i], t)
  }
  datafy <- data.frame(y, bondPrice)
  return(datafy)
}


data_frame2a(1000, 0.1, c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 
                    0.11, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.19, 0.2), 6)


# YTM = 0.115



#(b)uniroot 

my_function <- function(x){
  ((100*((1-(1/((1+x)**6)))/x))+(1000*(1/((1+x)**6))))-935
}

curve(expr = my_function,
      from = 0.1,
      to = 0.2)

uniroot_out <- uniroot(f = my_function, interval = c(0.1,0.2))
uniroot_out

# y= 0.115



#3. Consider a bond at $100 par value, 5% annual coupon rate, 20 years to maturity, 5% yield
# to maturity 

#(a) Calculate the duration and convexity of the bond. Write an R function to do so.
# What is the modified duration of this bond?

Duration_Convexity <- function(f, c, y, t){
  tVector <- 1:t
  cashflow <- rep(c*f, t)
  cashflow[t] <- c*f+f
  for(i in 1: t){
    D = i * ((cashflow[i]/(1+y)^i)/coupon_bond_price(f, c, y, i))
    Convexity = (1/coupon_bond_price(f, c, y, i))*(1/(1+y)^2)*(cashflow[i]*(i^2+i)/(1+y)^i)
  }
  #dataFrame0 <- data.frame(cashflow, pv_factor, pv)
  return(paste(D, Convexity))
}

Duration_Convexity(100, 0.05, 0.05, 20)

#Duration: 7.9, Convexity: 150.8
#Modified Duration = Duration/(1+y) = 7.9/(1+0.05) = 7.52

#(b) If the YTM is expected to increase to 7%, what is the price change due to duration
#only, due to convexity only, and due to both?

# Price_change_Duration = (-D)*(change_in_y) = (-7.9)*(0.07-0.05) = 0.158
# Price_change_Convexity = (1/2)*(Convexity)*(change_in_y)^2 = (1/2)*(150.8)*(0.02)^2 = 0.03 
# Price_change_Both = 0.158 + 0.03 = 0.188


#4. You have a bond which consists of a 12-year annual coupon bond with par value of
# $4,000, 5% coupon rate, 5% yield to maturity


#(a) Make an R function to calculate the portfolio value from t = 0 to t = 14? (Hint:
#portfolio value = price of bond + value of re-invested coupons)

t_potfolio <- function(f, c, y, t){
  tVector <- 1:12
  cashflow <- rep(c*f, t)
  reinvestCou <- rep(0,12)
  bondPrice <- rep(0,t)
  tempbondPrice <- rep(0,12)
  total <- rep(0,13)
  
  for (i in 1: t) {
    bondPrice[i] <- coupon_bond_price(f, c, y, i)
    #tempbondPrice[i] <- bondPrice[t-i+1]
    if(i == 1){
      reinvestCou[i] = 0
    }
    else{
      reinvestCou[i] <- ((1+y)^(i-1)-1)/y*cashflow[i-1]
    }
  }
  #tempbondPrice = rev(bondPrice)
  for (i in 1: t){
   tempbondPrice[i] <- bondPrice[t-i+1]
   total[i] <- tempbondPrice[i] + reinvestCou[i]
  }
  total[13] <- ((1+y)^t-1)/y*c*f+f
  #dataft <- data.frame(tVector, tempbondPrice, reinvestCou, total)
  total[14] <- total[13]*(1+y)
  total[15] <- total[14]*(1+y)
  return(total)
}
  

t_potfolio(4000,0.05,0.05,12) 

# from t=0 to t=14: [0]4000.000 [1]4200.000 [2]4410.000 [3]4630.500 [4]4862.025 [5]5105.126 [6]5360.383 [7]5628.402 [8]5909.822
# [9] 6205.313 [10]6515.579 [11]6841.357 [12]7183.425 [13]7542.597  [14]7919.726


#(b) Recalculate the values if yield increases to 7% just after the purchase. Do the
# same calculation if yield decreases to 2%

t_potfolio(4000, 0.05, 0.07, 15)
# y=7% : [0]3271.367  [1]3500.363  [2]3745.388  [3]4007.565  [4]4288.095  [5]4588.261  [6]4909.440  [7]5253.100
# [8]5620.817  [9]6014.275  [10]6435.274  [11]6885.743  [12]9025.804  [13]9657.611 [14]10333.643
t_potfolio(4000, 0.05, 0.02, 15)
# y=2% : [0]5541.912 [1]5652.750 [2]5765.805 [3]5881.121 [4]5998.743 [5]6118.718 [6]6241.093 [7]6365.914 [8]6493.233
# [9]6623.097 [10]6755.559 [11]6890.671 [12]7458.683 [13]7607.857 [14]7760.014


#(c) Plot three lines for the portfolio values for the three YTM values (2%, 5%, 7%) as
# a function of time and see where these lines are intersecting and at what time. Is
# this time equal to the duration of the portfolio? Why or why not?

Y_port_chart <- function(chart){
  timeaxis <- 1:15
  y2 <- t_potfolio(4000,0.05,0.02,12)
  y5 <- t_potfolio(4000,0.05,0.05,12)
  y7 <- t_potfolio(4000,0.05,0.07,12)
  dataFrame4c <- data.frame(y2,y5,y7,timeaxis)
  theChart <- ggplot(dataFrame4c, aes(x=timeaxis))+
    geom_line(aes(y=y2),color="pink") +
    geom_line(aes(y=y5),color="darkgreen") +
    geom_line(aes(y=y7),color="darkred")
  return (theChart)
}

Y_port_chart(chart)


#5. An Energy company has a liability to pay out $1,000 to its suppliers in each of the years 5
# to 8 (i.e., at times 5, 6, 7, 8). The yield curve is flat; the annually compounded interest
# rate is 4%. There is a bond market in which two zero coupon bonds are traded: one 5-year
# bond and one 25-year bond. For simplicity, assume that the face value of the bonds is $1
# dollar. Current time is t = 0

# (a) Calculate the present value and the Macauley duration of the company’s liabilities
npv <- function(cashflow,r){
  t <- length(cashflow)
  pv <- 0
  for (i in 1:t){
    pv <- pv + (cashflow[i]/((1+r)^i))
  }
  return (pv)
}

npv(c(0,0,0,0,1000,1000,1000,1000),0.04)
#npv = 3102.85

M_duration <- function(cashflow,r){
  t <- length(cashflow)
  dis <- rep(0,t)
  period <- rep(0,t)
  for (i in 1:t){
    dis[i] <- 1/((1+r)^i)
    period[i] <- i * cashflow[i] * dis[i]
  }
  periodSum <- sum(period)
  macD <- periodSum / npv(cashflow,r)
  return (macD)
}

M_duration(c(0,0,0,0,1000,1000,1000,1000), 0.04)
# m duration: 6.45

# (b) Construct a portfolio with the two zero coupon bonds such that the combined
# asset-liability position is immunised today t = 0 to interest rate fluctuations?

pv <- function(f,r,t){
  pv1 <- f/((1+r)^t)
  return (pv1)
}

# pv5 = 0.8219271
# pv25 = 0.3751168

weight_port <- function(x){
  w1 <- 0
  w2 <- 0
  for (x1 in seq(0,1000,by=0.01)){
    for (x2 in seq(0,3000,by=0.01)){
      if (x1*0.82 + x2*0.37 == 3102 & (x1/(x1+x2))*5+(x2/(x1+x2))*25 == 6.45){
        w1 <- x1
        w2 <- x2
      }
    }
  }
  return (x1,x2)
}

# portfolio: x1 = 3651, x2 = 285

