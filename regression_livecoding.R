# In class regression exercise 

library(ggplot2)
library(dplyr)

### Set true population parameters ###
beta0 <- 5 #setting true intercept 
beta1 <- 4 #setting true slope
signma2 <- 25 #setting sigma^2
n <- 30 #setting sample size


### Generate data ###
X <- seq(-2, 10, length.out = n) #30 numbers spanning -2-10
df <- data.frame(X=X) |> 
  mutate(err = rnorm(n, mean = 0, sd = sqrt(signma2)), 
         Y = beta0 + beta1*X + err)

ggplot(data = df, aes(x = X,y = Y)) + 
  geom_point() +
  geom_line(aes(x = X, y = Y_hat))

### Calculate ss and estimate parameters ###
Xbar <- mean(df$X) #using SOS equations
Ybar <- mean(df$Y)

SS_XY <- sum((df$X-Xbar)*(df$Y-Ybar)) 
SS_X <- sum((df$X-Xbar)^2)
b1_hat <-SS_XY/SS_X


b0_hat <- Ybar-b1_hat*Xbar

df$Y_hat <- b0_hat + b1_hat*df$X #allows you to add to graph

RSS <- sum((df$Y-df$Y_hat)^2)
sigma2_hat <- RSS/(n-2)

### Partitioning variance + coeef of determination ###
SS_Y <-sum((df$Y-Ybar)^2)
SS_reg <- SS_Y - RSS

r2 <- SS_reg/SS_Y 


### Compare w lm() results ###
model1 <- lm(Y ~ X, data = df) #response (left), predictor (right)
summary(model1)

