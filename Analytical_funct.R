
#PROECT "Adjustin for multiple test when endpoints are correlated "
#ANALYTICAL FUNCTION FOR FIXED FIXED ASSUMED CORRELATION
#######################################################!

#MAIN FUNCTION#
#function "-f(rho)" alone without parameter "gamma an k"

Asrr<- function (rho) (-0.025^(-rho) )*(1 - 0.025)^(1 + rho) + 0.0
rho<- seq(-1,1,0.02)
matplot(rho,Asrr(rho),type = "l",xlab = "ρ",ylab ="Family wise error rate") 


#analytical function with parameters "gamma and k" for different methods.

#Analytical function for unadjusted family wise error rate.
rho<- seq(-1,1,0.02)
AFun<- function (rho) ((0.1/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.1)
matplot(rho,AFun(rho),type='l',xlab = "ρ",ylab ="Family wise error rate",
        xlim=c(-1, 1), ylim=c(0, 0.1))
abline(v =1, untf = FALSE,col=6,lty=2)
abline(v =-1, untf = FALSE,col=6,lty=2)
abline(v =0.0, untf = FALSE,col=9,lty=2)
abline(h =0.05, untf = FALSE,col=2,lty=2)
legend ("bottomleft",c("α=0.05"),lwd=c(1,1),lty=1:2,col=c("red"),cex =0.67)

# Method1 fixed assumed correlation#
#Analytical function for  fixed assumer coorelation ##!
#("r=1,r=0.9,r=0.8,r=0.7,r=0.5,r=-1")
rho<- seq(-1,1,0.02)
Asor<- function (rho) (0.1/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.1
Asor2<-function (rho) (0.07658/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.07658
Asor3<-function (rho) (0.066/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.066
Asor4<- function (rho) (0.0602/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.0602
Asor5<- function (rho) (0.05481/70)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho) +0.05481 
Asor6<- function (rho) (0.05/76)*(-0.025^(-rho) )*(1 - 0.025)^(1 + rho)+0.05
matplot(rho,cbind(Asor(rho),Asor2(rho),Asor3(rho),Asor4(rho),Asor5(rho),Asor6(rho)),
        type='l',xlab = "ρ",ylab = "Family wise error rate",xlim=c(-1, 1),ylim=c(0, 0.1))
abline(v =1, untf = FALSE,col=6,lty=2)
abline(v =0.9, untf = FALSE,col=8,lty=2)
abline(v =0.8, untf = FALSE,col=8,lty=2)
abline(v =0.7, untf = FALSE,col=8,lty=2)
abline(v =0.5, untf = FALSE,col=8,lty=2)
abline(v =0.0, untf = FALSE,col=9,lty=2)
abline(h =0.05, untf = FALSE,col=8,lty=2)
abline(v =-1, untf = FALSE,col=6,lty=2)
legend ("bottomleft",c("r=1","r=0.9","r=0.8","r=0.7","r=0.5","r=-1","α=0.05"),lwd=c(1,1),
        lty=1:2,col=c("black","red","green","blue","cyan","violet","cadetblue"),cex =0.5)

