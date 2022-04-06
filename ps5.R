rm(list =ls())

library(ggplot2)

#µ = 12%, σ = 30% and S0 = 80, ∆t =1/52

u <- 0.12
o <- 0.3
S0 <- 80
dt <- 1/52
nperiods <- 52
time <- 0:52
set.seed(100) #random number generation
random_generation <- rnorm(nperiods) #the normal distribution
final_generation <- append(random_generation, 0, 0)

#1

#a.
exact_solution <- function(final_generation) {
  exact_vector <- rep(NA, nperiods+1)
  exact_vector[1] <- S0
  for(i in 1:52){
    exact_vector[i+1] <- exact_vector[i]*exp((((u-((o^2)/2))*dt)+o*(final_generation[i+1]-final_generation[i])))
  }
  return(exact_vector)
}

exact_result <- exact_solution(final_generation)


#b.
euler_solution <- function(final_generation) {
  euler_vector <- rep(NA, nperiods+1)
  euler_vector[1] <- S0
  for(i in 1:52){
    euler_vector[i+1] <- euler_vector[i] + u*euler_vector[i]*dt + o*euler_vector[i]*(final_generation[i+1]-final_generation[i])
  }
  return(euler_vector)
}
 euler_result <- euler_solution(final_generation)
 
 #c.
 milstein_solution <- function(x){
   milstein_vector <- rep(NA, nperiods+1)
   milstein_vector[1] <- S0
   for(i in 1:52){
     milstein_vector[i+1] <- milstein_vector[i] + u*milstein_vector[i]*dt + o*milstein_vector[i]*(final_generation[i+1]-final_generation[i]) + (1/2)*(o^2)*milstein_vector[i]*((final_generation[i+1]-final_generation[i])^2 - dt)
   }
   return(milstein_vector)
 }
 
 milstein_result <- milstein_solution(x)
 
 #d.
 binomial_solution <- function(x){
   binomial_vector <- rep(NA, nperiods+1)
   binomial_vector[1] <- S0
   for(i in 1:52){
     if(((u-o^2/2)*dt + o*(final_generation[i+1]-final_generation[i])) > 0){
       binomial_vector[i+1] <- binomial_vector[i] * exp(o*(sqrt(dt)))
     }
     else{
       binomial_vector[i+1] <- binomial_vector[i] * exp(-o*(sqrt(dt))) 
     }
   }
   return(binomial_vector)
 }
 
 binomial_result <- binomial_solution(x)
 
 #final dataframe
 final_dataframe <- data.frame(time, exact_result, euler_result, milstein_result, binomial_result)
 
 #plot
 
 fourPathGraph <- ggplot(final_dataframe, aes(x=time)) + 
   geom_line(aes(y=exact_result), color = "#FF99FF") + 
   geom_line(aes(y=euler_result), color = "#6999cc") + 
   geom_line(aes(y=milstein_result), color = "aquamarine") + 
   geom_line(aes(y=binomial_result), color = "burlywood")
 
 

 #2
 exact_1000 <- function(x){
   Exact_vec <- rep(NA,1000)
   for (i in 1:1000){
     set.seed(i)
     Exact_collection <- rnorm(nperiods)
     Exact_collection <- append(Exact_collection,0,0)
     Exact_result <- data.frame(time, exact_solution(Exact_collection))
     Exact_vec[i] <- Exact_result[53,2]
   }
   return(Exact_vec)
 }
 exact_vec <- exact_1000(x)
 plot(x=exact_vec, type = "l", ylim = c(0,300), col = "#009999")
 
 
 #3
 rrdif_1000 <- function(x){
   Euler_vec <- rep(NA,1000)
   Exact_vec <- rep(NA,1000)
   rr <- rep(NA,1000)
   for (i in 1:1000){
     set.seed(i)
     Euler_collection <- rnorm(nperiods)
     Euler_collection <- append(Euler_collection,0,0)
     Euler_result <- euler_solution(Euler_collection)
     Euler_vec[i] <- Euler_result[53]
     Exact_result <- data.frame(time, exact_solution(Euler_collection))
     Exact_vec[i] <- Exact_result[53,2]
     rr[i] <- Euler_vec[i] - Exact_vec[i]
   }
   return (rr)
 }
 rrdif_vec <- rrdif_1000(x)
 #Draw histogram
 hist(rrdif_vec,xlab="rrdifference",col="aquamarine3",border="black",xlim=c(-250,0))
 

 
 
 

 
 
 
 

