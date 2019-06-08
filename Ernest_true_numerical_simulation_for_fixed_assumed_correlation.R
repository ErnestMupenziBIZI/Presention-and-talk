
##PROJECT###   "Adjusting for multiple test when endpoints are correlated"
### METHOD;   "Adjust for fixed assumed correlation"
##################################################################################
##################################################################################

######Generating the mulrivariate normal distributed data for two endpoints and
###### statistical test 
library(TeachingDemos)
library(MASS)
biv.ttest <- function(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x <- mvrn[,1]
  y <- mvrn[,2]
  #print(cor(x,y))
  
  z.test(x,mu=0,1,"less")
  z.test(y,mu=0,1,"less")
  resx <- z.test(x,mu=0,1,"less")
  resy <- z.test(y,mu=0,1,"less")
  aa<-resx$p.value< alp
  bb<-resy$p.value < alp
  #print(c(resx$p.value, resy$p.value))
  # print(resx)
  
  return (c(aa , bb ,aa|bb))
}

biv.ttest(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

#repeating the process loops times
##Estimating the family wise error rate 
looptest <- function(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  restest <- matrix(0,loops,3)
  for(ii in 1:loops)
    restest[ii,] <- biv.ttest(nn=nn,mu=mu,sigma1=sigma1,sigma2=sigma2,rho=rho,alp=alp)    
  c(mean(restest[,1]),mean(restest[,2]),(mean(restest[,3])))
}

looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

## ploting the estimated FWER against correlation rho.
rhos <- seq(from=-1, to=1, by = 0.1)
As <-rhos
for ( ii in 1:length(rhos))
  As[ii] <- looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=rhos[ii],alp=0.05)[3]

plot(rhos,As, type="l",main = "FWER against Correlation rho unadjusted",ylab = "Alpha level")


###Family wise error rate for three different fixed assumed-

###correlation (1,0.7,0.5,0)
#using the above correlations in analytical function we find the maximum FWER for each correlation.
# alpa " is a vector containing the halves of the maximum of FWERs  computed in analytical
# function to each correlation.
# we took the half of each FWER in order to follow the Bonferroni correction to detect
# where each graph will be starting  ### below is the loop.

rhos <- seq(from=-1, to=1, by = 0.1)
alpa<-c(0.05,0.0301, 0.027405, 0.0253)
As <-rhos
for (iiv in 1:length(alpa)){
  for ( ii in 1:length(rhos)){
    As[ii] <- looptest(loops=1000000,nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=rhos[ii],alp=alpa[iiv])[3] 
  }
  if(alpa[iiv] == 0.05){
    As1 <- As 
  }
  if(alpa[iiv] == 0.0301){
    As2 <- As 
  }
  if(alpa[iiv] == 0.027405){
    As3 <- As 
  }
  if(alpa[iiv] == 0.0253){
    As4 <- As 
  }
}
matplot(rhos,cbind(As1,As2,As3,As4), type="l",xlab = "ρ",ylab = "Family wise error rate ",xlim=c(-1, 1), ylim=c(0, 0.1))
abline(h =0.05, untf = FALSE,col=6,lty=2)
abline(v = 1, untf = FALSE,col=5,lty=2)
abline(v = 0, untf = FALSE,col=8,lty=2)
abline(v = 0.5, untf = FALSE,col=8,lty=2)
abline(v = 0.7, untf = FALSE,col=8,lty=2)
abline(v = -1, untf = FALSE,col=5,lty=2)
legend ("bottomleft",c("r=1","r=0.7","r=0.5","r=0","α=0.05"),lwd=c(1,1),lty=1:2,col=c("black","red","green","blue","violet"),cex =0.57)

