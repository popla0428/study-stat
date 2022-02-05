rm(list=ls())
library(dplyr)
library(ggplot2)

#######################
### 2.1. Likelihoods 
#######################

#Plot likelihood curve----
n = 10 #set total trials
x = n * 0.8 #set successes
theta = seq(0,1,len=100)  #create theta variable, from 0 to 1
like  = dbinom(x,n,theta) #create likelihood function (calculated probs)
plot(theta,like,type='l',xlab=expression(theta), ylab='Likelihood', main="Likelihood Curve")

#Likelihood curves when n gets larger 
len = 6
for (i in 1:len) {
  n = 10^i
  x = n * 0.8
  theta = seq(0,1,len=100)
  like = dbinom(x,n,theta) 
  theta_mle = theta[which(like == max(like))]
  
  data = data.frame(theta, like) 
  assign(x = paste0('p',i),
         value = data %>% ggplot(aes(x = theta, y = like)) + 
           geom_line() +
           geom_vline(xintercept = theta_mle, color = 'red') +
           geom_text(x = theta_mle, y = 0, label = round(theta_mle,3)) +
           labs(subtitle = 
                  paste0('mle: ',round(theta_mle,3),' (n=',n,',x=',x,')')) +
           xlab('theta') + ylab('Likelihood'))
}

#plot = gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,nrow = 5)
plot = gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow = 3)


#Calculate the likelihood ratio----
n = 100 #set total trials
x = 50  #set successes
H0 = 0.4 #specify one hypothesis you want to compare with the likihood ratio
H1 = x/n #specify another hypothesis you want to compare with the likihood ratio (you can use 1/20, or 0.05)
dbinom(x,n,H1)/dbinom(x,n,H0) #Returns the likelihood ratio of H1 over H0
dbinom(x,n,H0)/dbinom(x,n,H1) #Returns the likelihood ratio of H0 over H1

theta = seq(0,1,len=100) #create theta variable, from 0 to 1
like = dbinom(x,n,theta)
#png(file="LikRatio.png",width=4000,height=3000, , units = "px", res = 900)
plot(theta,like,type='l',xlab=expression(theta), ylab='Likelihood', lwd=2)
points(H0,dbinom(x,n,H0))
points(H1,dbinom(x,n,H1))
segments(H0, dbinom(x,n,H0), x/n, dbinom(x,n,H0), lty=2, lwd=2)
segments(H1, dbinom(x,n,H1), x/n, dbinom(x,n,H1), lty=2, lwd=2)
segments(x/n, dbinom(x,n,H0), x/n, dbinom(x,n,H1), lwd=2)
title(paste('Likelihood Ratio H0/H1:',round(dbinom(x,n,H0)/dbinom(x,n,H1),digits=2)," Likelihood Ratio H1/H0:",round(dbinom(x,n,H1)/dbinom(x,n,H0),digits=2)))
#dev.off()