
####PROECT "Adjustin for multiple test when endpoints are correlated "
####Estimated correlation n=50
#######################################################

set.seed(2653)
library(TeachingDemos)
library(MASS)
################################## Etimating correlation 
COR.test <- function(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  #we got problem when we replace rho by rho*sigma1*sigma2
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x <- mvrn[,1]
  y <- mvrn[,2]
  
  x1<- sample(x,50,replace = FALSE)
  x2<-sample(x,50,replace = FALSE)
  
  y1<-sample(y,50,replace = FALSE)
  y2<-sample(y,50,replace = FALSE)
  corr<-cor(x1,y1)
  CORR<-cor(x2,y2)
  covar<-cov(x1,y1)
  COVAR<-cov(x2,y2)
  
  #####r_pool#######
  r_pool<- (cor(x1,y1) + cor(x2,y2))/2
  
  ####r_blind########
  r_blind<- (cov(x1,y1) + cov(x2,y2) )/2
  
  ######fisher####
  z1<-(1/2)*log((1+cor(x1,y1))/(1-cor(x1,y1)))
  z2<- (1/2)*log((1+cor(x2,y2))/(1-cor(x2,y2)))
  Z<-(z1+z2)/2
  r_fisher<- tanh(Z)
  
  ######r_OP###########
  r_1<- cor(x1,y1)+(cor(x1,y1))*((1-(cor(x1,y1))^2)/94)
  r_2<- cor(x2,y2)+(cor(x2,y2))*((1-(cor(x2,y2))^2)/94)
  r_OP<-(r_1+r_2)/2
  #print(covar)
  #print(COVAR)
  return (c(r_pool,r_blind,r_fisher,r_OP))
}
a<-COR.test(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

r_pool<- a[1]    #r_pool   n=50
r_blind<- a[2]   #r_bling  n=50
r_fisher<- a[3]  #r_fiher  n=50
r_op<- a[4]      #r_op     n=50



# Etimator n=10
#Estimated correlation n=10
####
set.seed(2653)
library(TeachingDemos)
library(MASS)
## Etimating correlation 
COR.test <- function(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)
{
  Sigma<-matrix(c(sigma1^2,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2^2),2,2) 
  
  mvrn <- mvrnorm(nn,mu ,Sigma)
  x <- mvrn[,1]
  y <- mvrn[,2]
  
  x1<- sample(x,10,replace = FALSE)
  x2<-sample(x,10,replace = FALSE)
  
  y1<-sample(y,10,replace = FALSE)
  y2<-sample(y,10,replace = FALSE)
  corr<-cor(x1,y1)
  CORR<-cor(x2,y2)
  covar<-cov(x1,y1)
  COVAR<-cov(x2,y2)
  
  #####r_pool#######
  r_pool<- (cor(x1,y1) + cor(x2,y2))/2
  
  ####r_blind########
  r_blind<- (cov(x1,y1) + cov(x2,y2) )/2
  
  ######fisher####
  z1<-(1/2)*log((1+cor(x1,y1))/(1-cor(x1,y1)))
  z2<- (1/2)*log((1+cor(x2,y2))/(1-cor(x2,y2)))
  Z<-(z1+z2)/2
  r_fisher<- tanh(Z)
  
  ######r_OP###########
  r_1<- cor(x1,y1)+(cor(x1,y1))*((1-(cor(x1,y1))^2)/14)
  r_2<- cor(x2,y2)+(cor(x2,y2))*((1-(cor(x2,y2))^2)/14)
  r_OP<-(r_1+r_2)/2
  #print(covar)
  #print(COVAR)
  return (c(r_pool,r_blind,r_fisher,r_OP))
}
b<-COR.test(nn=100,mu=c(0,0),sigma1=1,sigma2=1,rho=0.8,alp=0.05)

r_pool<- b[1]    #r_pool n=10
r_blind<- b[2]   #r_bling n=10
r_fisher<- b[3]  #r_fiher n=10
r_op<- b[4]      #r_op    n=10
