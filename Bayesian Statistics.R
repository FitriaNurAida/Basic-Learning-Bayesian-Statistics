# Monte Carlo Simulation #
prev <- 0.00025 #disease prevalance
N <- 100000
outcome <- sample(c("Disease","Healthy"),N,replace=TRUE,prob=c(prev,1-prev))
head(outcome)

N_D <- sum(outcome == "Disease") ; N_D   # number with disease
N_H <- sum(outcome == "Healthy")  ; N_H  # number healthy
# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character",N) #create empty vector
head(test)
test[outcome=="Disease"] <- sample(c("+","-"),N_D,replace=TRUE,prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"] <- sample(c("-","+"),N_H,replace=TRUE,prob=c(accuracy,1-accuracy))
head(test)
table(outcome,test)

# Exercise 1
Pr_1 <- 1/8500 #the probability of the first son dying of Lepra
Pr_2 <- 1/100 #the probability of the second son dying of Lepra

Pr_die <- Pr_1*Pr_2 #the probability of both sons dying of Lepra
Pr_die

#Exercise 2
Pr_1 <- 1/8500 #the probability of the first son dying of Lepra
Pr_2 <- 1/100 #the probability of the second son dying of Lepra
Pr_B <- Pr_1*Pr_2 #the probability of both sons dying of Lepra
Pr_A <- 1/1000000 #the rate of mothers that are murderers
Pr_BA <- 0.50 #the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_AB <- Pr_BA*Pr_A/Pr_B #the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB

#Exercise 3
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

head(polls)
results <- polls %>% summarize(avg= mean(spread), se = sd(spread)/(sqrt(n())))
results

#Exercise 4 continuing exercise 3, Estimate the posterior distribution
results
mu <- 0
tau <- 0.01
sigma <- results$se
sigma
Y <- results$avg
Y
B <- sigma^2/(sigma^2+tau^2)
B
posterior <- (B*mu) +(1-B)*Y #Calculate the expected value of the posterior distribution
posterior

#Exercise 5 continuing excercise 4, Standard error of posterior distribution
se_posterior <- sqrt(1/((1/sigma^2)+(1/tau^2)))
se_posterior

#Exercise 6 continuing excercise 5, Credible interfal
z <- qnorm(0.975)
lower <- posterior - z*se_posterior
ci <- c(lower,posterior + z*se_posterior)
ci

#Exercise 7 calculate the probability that the spread in Florida was less than 0.
pnorm(0,mean=posterior,sd=se_posterior)

#Exercise 8 plot distribution
taus <- seq(0.005, 0.05, len = 100)
p_calc <- function(tau){
  B <- sigma^2/(sigma^2+tau^2)
  prob <- pnorm(0,mean=(B*mu) +(1-B)*Y,sd=sqrt(1/((1/sigma^2)+(1/tau^2))))
  return(prob)
} #generates `B` and calculates the probability of the spread being less than 0
ps <- p_calc(taus)
ps
plot(taus,ps)


