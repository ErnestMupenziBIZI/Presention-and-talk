

#### DISTRIBUTION FOR ESTIMATORS "n=10"####
##### different correlation (ρ=0, ρ=.5, ρ=.8)

######Distribution for ρ=0
set.seed(2653)
library(TeachingDemos)
library(MASS)
### K=nn is the sample size (10,50,200,1000)
k=10
################################## Etimating correlation 
COR.test <- function(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.0,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  #we got problem when we replace rho by rho*sigma1*sigma2
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x1 <- mvrn[,1]
  y1 <- mvrn[,2]
  
  corr<-cor(x1,y1)
  
  covar<-cov(x1,y1)
  
  
  #####r_pool#######
  r_pool<- cor(x1,y1)
  
  ####r_blind########
  r_blind<- cov(x1,y1)
  
  ######fisher####
  r_fisher<- tanh((1/2)*log((1+corr)/(1-corr)))
  
  ######r_OP###########
  r_OP<-corr+(corr)*((1-(corr)^2)/(2*(nn-3)))
  #print(corr)
  #print(covar)
  return (c(r_pool,r_blind,r_fisher,r_OP))
}
COR.test(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.0,alp=0.05)


looptest <- function(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.0,alp=0.05)
{
  restest <- matrix(0,loops,4)
  for(ii in 1:loops)
    restest[ii,] <-COR.test(nn=nn,mu=mu,sigma1=sigma1,sigma2=sigma2,rho=rho,alp=alp)    
  restest
}
looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.0,alp=0.05)
###then

corlooptest<-looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.0,alp=0.05)

pool <- corlooptest[,1]
blind <- corlooptest[,2]
fisher <- corlooptest[,3]
op <- corlooptest[,4]

par(mfrow = c(2,2))
# hist(pool  ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Pooled Estimate for ρ =0"))
# hist(blind ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Blind Estimate for ρ=0"))
# hist(fisher,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Fisher Estimate for ρ=0"))
# hist(op    ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("OP Estimate for ρ=0"))
#cbind(plot(density(pool),plot(density(blind)),plot(density(fisher)),plot(density(op))))

plot(density(pool),col="red",main = "Distribution for ρ =0",ylim=c(0,2) ,xlab ="r'")

lines(density(blind, adjust=1), type="l",col="blue")
lines(density(fisher, adjust=1), type="l",col="green")
lines(density(op, adjust=1), type="l",col="black")
abline(v =0.0, untf = FALSE,col=7,lty=2)
legend ("topleft",c("pool","blind","fisher","op_","ρ =0"),lwd=c(1,1),lty=1:2,col=c("red","blue","green","black","yellow"),cex =0.57)
##################################################################
#####################################################################


####Distribution for ρ=.5
COR.test <- function(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.5,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  #we got problem when we replace rho by rho*sigma1*sigma2
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x1 <- mvrn[,1]
  y1 <- mvrn[,2]
  
  corr<-cor(x1,y1)
  
  covar<-cov(x1,y1)
  
  
  #####r_pool#######
  r_pool<- cor(x1,y1)
  
  ####r_blind########
  r_blind<- cov(x1,y1)
  
  ######fisher####
  r_fisher<- tanh((1/2)*log((1+corr)/(1-corr)))
  
  ######r_OP###########
  r_OP<-corr+(corr)*((1-(corr)^2)/(2*(nn-3)))
  #print(corr)
  #print(covar)
  return (c(r_pool,r_blind,r_fisher,r_OP))
}
COR.test(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.5,alp=0.05)


looptest <- function(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.5,alp=0.05)
{
  restest <- matrix(0,loops,4)
  for(ii in 1:loops)
    restest[ii,] <-COR.test(nn=nn,mu=mu,sigma1=sigma1,sigma2=sigma2,rho=rho,alp=alp)    
  restest
}
looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.5,alp=0.05)

###then
corlooptest<-looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.5,alp=0.05)

pool <- corlooptest[,1]
blind <- corlooptest[,2]
fisher <- corlooptest[,3]
op <- corlooptest[,4]

#par(mfrow = c(2,2))
# hist(pool  ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Pooled Estimate for ρ =0.5"))
# hist(blind ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Blind Estimate for ρ=0.5"))
# hist(fisher,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("Fisher Estimate for ρ=0.5"))
# hist(op    ,probability = 1,breaks = 20,xlim = c(-1,1),xlab = "r' ",main = paste("OP Estimate for ρ=0.5"))
#cbind(plot(density(pool),plot(density(blind)),plot(density(fisher)),plot(density(op))))

plot(density(pool),col="red",main = "Distribution for ρ =0.5",ylim=c(0,2),xlab = "r'")

lines(density(blind, adjust=1), type="l",col="blue")
lines(density(fisher, adjust=1), type="l",col="green")
lines(density(op, adjust=1), type="l",col="black")
abline(v =0.5, untf = FALSE,col=7,lty=2)
legend ("topleft",c("pool","blind","fisher","op_","ρ =0.5"),lwd=c(1,1),lty=1:2,col=c("red","blue","green","black","yellow"),cex =0.57)
# #####################################################""
########################################################

####### Distribution for ρ=.8
COR.test <- function(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  #we got problem when we replace rho by rho*sigma1*sigma2
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x1 <- mvrn[,1]
  y1 <- mvrn[,2]
  
  corr<-cor(x1,y1)
  
  covar<-cov(x1,y1)
  
  
  #####r_pool#######
  r_pool<- cor(x1,y1)
  
  ####r_blind########
  r_blind<- cov(x1,y1)
  
  ######fisher####
  r_fisher<- tanh((1/2)*log((1+corr)/(1-corr)))
  
  ######r_OP###########
  r_OP<-corr+(corr)*((1-(corr)^2)/(2*(nn-3)))
  #print(corr)
  #print(covar)
  return (c(r_pool,r_blind,r_fisher,r_OP))
}
COR.test(nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)


looptest <- function(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  restest <- matrix(0,loops,4)
  for(ii in 1:loops)
    restest[ii,] <-COR.test(nn=nn,mu=mu,sigma1=sigma1,sigma2=sigma2,rho=rho,alp=alp)    
  restest
}
looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
###then
corlooptest<-looptest(loops=100000,nn=k,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

pool <- corlooptest[,1]
blind <- corlooptest[,2]
fisher <- corlooptest[,3]
op <- corlooptest[,4]

#Lower bound X=0.7,0.9,0.95
#LCL<- (14/(qchisq( X ,2*k-6)))*var(estimated corr)
 
#par(mfrow = c(2,2))
# hist(pool  ,probability = 1,breaks = 10,xlim = c(-1,1),xlab = "r' ",main = paste("Pooled Estimate for ρ =0.8"))
# hist(blind ,probability = 1,breaks = 10,xlim = c(-1,1),xlab = "r' ",main = paste("Blind Estimate for ρ=0.8"))
# hist(fisher,probability = 1,breaks = 10,xlim = c(-1,1),xlab = "r' ",main = paste("Fisher Estimate for ρ=0.8"))
# hist(op    ,probability = 1,breaks = 10,xlim = c(-1,1),xlab = "r' ",main = paste("OP Estimate for ρ=0.8"))

plot(density(pool),col="red",main = "Distribution for ρ =0.8",ylim=c(0,5),xlab ="r'")

lines(density(blind, adjust=1), type="l",col="blue")
lines(density(fisher, adjust=1), type="l",col="green")
lines(density(op, adjust=1), type="l",col="black")
abline(v =0.8, untf = FALSE,col=7,lty=2)
legend ("topleft",c("pool","blind","fisher","op_","ρ =0.8"),lwd=c(1,1),lty=1:2,col=c("red","blue","green","black","yellow"),cex =0.57)
