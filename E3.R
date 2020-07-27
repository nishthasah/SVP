##E 3
##GIVEN
s0 <- 1065
Vl <- 0.20^2
a <- 0.95
c <- 0.85
rf <- 0.0125
t <- 1/250
X <- seq(100,2000,100)
p <- -0.3
Tannual <- 1



## PART 1
#BSM = S0*N(d1)-K*exp(-rt)*N(d2)
# d1=(ln(s0/X)+(rf+Vl/2)*T)/(sqrt(Vt)*T)
# d2= d1 - sqrt(Vt*T)

d1=(log(s0/X)+(rf+Vl/2)*T)/(sqrt(Vl)*T)
d2= d1 - sqrt(Vl*T)
BSM = s0*pnorm(d1)-X*exp(-rf*T)*pnorm(d2)
BSM
BSM.matrix<-cbind(X,BSM)
BSM.matrix


## PART 2
s0 <- 1065
Vl <- 0.20^2
a <- 0.95
c <- 0.85
rf <- 0.0125
t <- 1/250
X <- seq(100,2000,100)
p <- -0.3
Tannual <- 1
set.seed(123456)
e1<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
set.seed(789101)
e2<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
pathS<-matrix(nrow=250,ncol=1000)
pathS[1,1:1000]<-s0
pathV<-matrix(nrow=250,ncol=1000)
pathV[1,1:1000]<-Vl

funcV <- for(i in 1:250)
  for(j in 1:1000) {
    pathV[i+1,j]= pathV[i,j] + a*(Vl - pathV[i,j])*t + c*(sqrt(t*abs(pathV[i,j])))*(p*e1[i+1,j] + sqrt(1-p^2)*(e2[i+1,j]))
  } 
tail(pathV,n=1)
pathV <- abs(pathV)
range(round(pathV,2))

funcS <- for(i in 1:250)
  for(j in 1:1000) {
    pathS[i+1,j]= pathS[i,j]*exp(((rf-0.5*(pathV[i,j]))*t)+(sqrt(t*pathV[i,j])*e1[i+1,j]))
  } 
tail(pathS, n=1)

str(X)


SVMprice<-pathS[250,1:1000]
str(SVMprice)
head(SVMprice)

payoff<-matrix(rep(0,20000),nrow=20,byrow=T)
for (i in 1:1000)
  for(j in 1:20){
    payoff[j,i]=as.matrix(SVMprice[i]-X[j])
  }


payoff<-ifelse(payoff<0,0,payoff)
head(payoff)
tail(payoff)



as.matrix(payoff)
avgpayoff<-rowMeans(payoff)


call<-exp(-rf*1)*avgpayoff
head(call,n=1)
str(call)
dim(X)
dim(call)
call<-as.matrix(call)
rownames(call)<-X
colnames(call)<-SVMprice
head(call)
call
plot(X,call,xlab = "Strike" , ylab = "Call Price" , main= "SVM Prices",lty = 2, lwd = 2)



###Compute BSM Implied Volatility for SVM
#BSM = S0*N(d1)-K*exp(-rt)*N(d2)
# d1=(ln(s0/X)+(rf+Vl/2)*T)/(sqrt(Vt)*T)
# d2= d1 - sqrt(Vt*T)
#d1=(log(s0/X)+(rf+Vl/2)*T)/(sqrt(Vl)*T)
#d2= d1 - sqrt(Vl*T)
#BSM = s0*pnorm(d1)-X*exp(-rf*T)*pnorm(d2)

####updating the RQuantlib package for my version of R with the following code:

library(RQuantLib)
.onAttach <- function(libname, pkgname) {
  
  ## if it is not interactive (as eg in testing or cronjobs), do nothing
  if (!interactive()) return(invisible(NULL))
  
  ## else let's test the QuantLib version, and then the intraday capability
  qlver <- getQuantLibVersion()
  if (compareVersion(qlver, "1.7") < 0) {
    packageStartupMessage("QuantLib version ", qlver, " detected which is older than 1.7.")
    packageStartupMessage("Intra-daily options analytics unavailable with that version.")
  } else if (!getQuantLibCapabilities()[["intradayDate"]]) {
    packageStartupMessage("Sufficient QuantLib version with insufficient configuration.")
    packageStartupMessage("Try configuring your build with the --enable-intraday argument.")
  }
}
dim(call)
dim(X)
X
X<-as.matrix(X,nrow=20)
dim(X)
call.svm<-cbind(X,call)
colnames(call.svm)<-c("Strike","SVM Call Price")

iv.svm<-rep(0,20)
for (i in 1:20)
{
iv.svm[i]<-EuropeanOptionImpliedVolatility(type="call", value=call.svm[i,2], underlying= s0,
                                strike=call.svm[i,1], dividendYield=0.00, riskFreeRate=rf,
                                maturity=1, volatility=0.2)
    }

iv.svm
iv.svm<-as.matrix(iv.svm,nrow=20)
iv.svm.mat<-cbind(X,iv.svm)
colnames(iv.svm.mat)<-c("Strike","IV for SVM Call") 
iv.svm.mat 



###COMPUTE Implied Volatility from BSM

####
iv<-rep(0,20)
for (i in 1:20)
{
  iv[i]<-EuropeanOptionImpliedVolatility(type="call", value=BSM.matrix[i,2], underlying= s0,
                                         strike=BSM.matrix[i,1], dividendYield=0.00, riskFreeRate=rf,
                                         maturity=1, volatility=0.2)
}

iv.BSM<-iv
iv.BSM.mat<-round(cbind(X,iv.BSM),4)
colnames(iv.BSM.mat)<-c("Strike","IV for BSM")
iv.BSM.mat

####Plot Implied volatility from SVM and BS against their respective Strike Prices

plot(X,iv.svm,xlab = "Strike" , ylab = "SVM and BS " , main= "SVM and BSM IV against Strike",lty = 2, lwd = 2)
par(new = TRUE)
plot(X,iv.BSM,xlab = " " , ylab = " " , main= " ",lty = 2,col="blue", lwd = 6)





#### Implied Distribution for SVM
ivsvm.dist<-rep(0,20)
for (i in 3:20)
ivsvm.dist[i] <- (((call.svm[i,2]-call.svm[i-1,2])/(call.svm[i,1]-call.svm[i-1,1]))-((call.svm[i-1,2]-call.svm[i-2,2])/(call.svm[i-1,1]-call.svm[i-2,1])))/(call.svm[i-1,1]-call.svm[i-2,1])


ivsvm.dist
ivsvmdistmat<-cbind(iv.svm.mat,ivsvm.dist)
round(ivsvmdistmat[c(-1,-2),],8)


plot(X,ivsvm.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = "IV-SVM Dist " , main= "SVM-IV Distribution against Strike") +
  lines(X,ivsvm.dist,col="red",type="b", lwd=1.5)



### Implied Distribution for BS prices
ivBSM.dist<-rep(0,20)
for (i in 3:20)
  ivBSM.dist[i] = (((BSM.matrix[i,2]-BSM.matrix[i-1,2])/(BSM.matrix[i,1]-BSM.matrix[i-1,1]))-((BSM.matrix[i-1,2]-BSM.matrix[i-2,2])/(BSM.matrix[i-1,1]-BSM.matrix[i-2,1])))/(BSM.matrix[i-1,1]-BSM.matrix[i-2,1])


ivBSM.dist
ivBSMdistmat<-cbind(iv.BSM.mat,ivBSM.dist)
round(ivBSMdistmat[c(-1,-2),],8)


##Plot
plot(X,ivBSM.dist,col="blue",xlab = "Strike" , ylab = "IV-BSM Dist " , main= "BSM-IV Distribution against Strike")+
lines(X,ivBSM.dist,col="blue",type="b", lwd=1.5)


##Both TOgether
plot(X,ivBSM.dist,col="blue",xlab = "Strike" , ylab = "IV Dist " , main= "BSM & SVM -IV Distribution against Strike")+
  lines(X,ivBSM.dist,col="blue",type="b", lwd=1.5)
par(new = TRUE)
plot(X,ivsvm.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = "" ) +
  lines(X,ivsvm.dist,col="red",type="b", lwd=3.5)



##############################  ALTERNATIVE 1 #########################################
#######################################################################################

s0 <- 1065
Vl <- 0.20^2
a <- 0.95
c <- 0.85
rf <- 0.0125
t <- 1/250
X <- seq(100,2000,100)
p.alt1 <- 0
Tannual <- 1
set.seed(123456)
e1<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
set.seed(789101)
e2<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
pathS.alt1<-matrix(nrow=250,ncol=1000)
pathS.alt1[1,1:1000]<-s0
pathV.alt1<-matrix(nrow=250,ncol=1000)
pathV.alt1[1,1:1000]<-Vl

funcV.alt1 <- for(i in 1:250)
  for(j in 1:1000) {
    pathV.alt1[i+1,j]= pathV.alt1[i,j] + a*(Vl - pathV.alt1[i,j])*t + c*(sqrt(t*abs(pathV.alt1[i,j])))*(p.alt1*e1[i+1,j] + sqrt(1-p.alt1^2)*(e2[i+1,j]))
  } 
tail(pathV.alt1,n=1)
pathV.alt1 <- abs(pathV.alt1)
range(round(pathV.alt1,2))

funcS.alt1 <- for(i in 1:250)
  for(j in 1:1000) {
    pathS.alt1[i+1,j]= pathS.alt1[i,j]*exp(((rf-0.5*(pathV.alt1[i,j]))*t)+(sqrt(t*pathV.alt1[i,j])*e1[i+1,j]))
  } 
tail(pathS.alt1, n=1)

str(X)


SVMprice.alt1<-pathS.alt1[250,1:1000]
str(SVMprice.alt1)
head(SVMprice.alt1)

payoff.alt1<-matrix(rep(0,20000),nrow=20,byrow=T)
for (i in 1:1000)
  for(j in 1:20){
    payoff.alt1[j,i]=as.matrix(SVMprice.alt1[i]-X[j])
  }


payoff.alt1<-ifelse(payoff.alt1<0,0,payoff.alt1)
head(payoff.alt1)
tail(payoff.alt1)



as.matrix(payoff.alt1)
avgpayoff.alt1<-rowMeans(payoff.alt1)


call.alt1<-exp(-rf*1)*avgpayoff.alt1
head(call.alt1,n=1)
str(call.alt1)
dim(X)
dim(call.alt1)
call.alt1<-as.matrix(call.alt1)
head(call.alt1)
call.alt1

## Call Plots ###

plot(X,call.alt1,xlab = "Strike" , ylab = "Call Price Alternate 1" , main= "SVM-Alt. 1 Prices",lty = 2, lwd = 2,pch="+")

## Compare Call Price with Base Case ##

plot(X,call.alt1,xlab = "Strike" , ylab = "Call Price" , main= "SVM-Alt. 1 Prices vis Base ",lty = 2, lwd = 2,pch="+")
par(new=TRUE)
plot(X,call,xlab = "" , ylab = "" , main= "",lty = 3, lwd = 2)

call.svm.alt1<-cbind(X,call.alt1)

Compare.calls<-cbind(call.svm.alt1,call)
Compare.calls.names<-c("Strike Price","Alternate 1","Base Case")
colnames(Compare.calls)<-Compare.calls.names
Compare.calls

######### Compute BSM Implied Volatility for SVM ########
iv.svm.alt1<-rep(0,20)
for (i in 1:20)
{
  iv.svm.alt1[i] <- EuropeanOptionImpliedVolatility(type="call", value=call.svm.alt1[i,2], underlying= s0,
                                             strike=call.svm.alt1[i,1], dividendYield=0.00, riskFreeRate=rf,
                                             maturity=1, volatility=0.2)
}

iv.svm.alt1
iv.svm.alt1<-as.matrix(iv.svm.alt1,nrow=20)
iv.svm.alt1.mat<-cbind(X,iv.svm.alt1)
comp.names<-c("Strike","Alternate 1","Base Case")
iv.svm.alt1.compare<-cbind(iv.svm.alt1.mat,iv.svm)
colnames(iv.svm.alt1.compare)<-comp.names
iv.svm.alt1.compare

####### Plot Implied volatility from SVM Base Case and Alternative 1 against their respective Strike Prices

plot(X,iv.svm,xlab = "Strike" , ylab = "SVM and Alt1 " , main= "SVM Base Case & Alternate 1 IV against Strike",lty = 2, lwd = 2)
par(new = TRUE)
plot(X,iv.svm.alt1,lty = 3, lwd = 4, xlab=" ", ylab=" " , pch="+")+lines(X,iv.svm.alt1,col="red",type="b", lwd=1,xlab=" ", ylab=" ", pch="+")

####### Implied Distribution for SVM
ivsvmalt1.dist<-rep(0,20)
for (i in 3:20)
  ivsvmalt1.dist[i] <- (((call.svm.alt1[i,2]-call.svm.alt1[i-1,2])/(call.svm.alt1[i,1]-call.svm.alt1[i-1,1]))-((call.svm.alt1[i-1,2]-call.svm.alt1[i-2,2])/(call.svm.alt1[i-1,1]-call.svm.alt1[i-2,1])))/(call.svm.alt1[i-1,1]-call.svm.alt1[i-2,1])


ivsvmalt1.dist
plot(X,ivsvmalt1.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = "SVM Alterantive 1 IV Dist " , main= "SVM Alt 1 -IV Distribution against Strike",pch="+") +
  lines(X,ivsvmalt1.dist,col="red",type="b", lwd=1.5 ,pch="+")

#####    Comparison   #####
###########################

ivsvm.dist.mat<-cbind(X,ivsvm.dist)
ivsvmdist.alt1.compare<-cbind(ivsvm.dist.mat,ivsvmalt1.dist)
compIVdist.names<-c("Strike Price","Base Case","Alternate 1")
colnames(ivsvmdist.alt1.compare)<-compIVdist.names
Alternate1.Comparison <- round(ivsvmdist.alt1.compare,8)
Alternate1.Comparison[c(-1,-2),]

plot(X,ivsvm.dist,xlab = "Strike" , ylab = "SVM and Alt 1 " , main= "SVM Base Case & Alternate 1 IV Dist against Strike",lty = 2, lwd = 6)
par(new = TRUE)
plot(X,ivsvmalt1.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = " " , main= "" ,pch="+") +
  lines(X,ivsvmalt1.dist,col="red",type="b", lwd=3, pch="+")

###Comments : Heavier Tail to the left of the axes for Base Case i.e. p=-0.3


##################################### ALTERNATE 2 ###################################### 
########################################################################################
s0 <- 1065
Vl <- 0.20^2
a <- 0.95
c.alt2 <- 0.15
rf <- 0.0125
t <- 1/250
X <- seq(100,2000,100)
p <- -0.3
Tannual <- 1
set.seed(123456)
e1<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
set.seed(789101)
e2<-matrix((rnorm(250000,mean=0,sd=1)),nrow=250,ncol=1000)
pathS.alt2<-matrix(nrow=250,ncol=1000)
pathS.alt2[1,1:1000]<-s0
pathV.alt2<-matrix(nrow=250,ncol=1000)
pathV.alt2[1,1:1000]<-Vl

funcV.alt2 <- for(i in 1:250)
  for(j in 1:1000) {
    pathV.alt2[i+1,j]= pathV.alt2[i,j] + a*(Vl - pathV.alt2[i,j])*t + c.alt2*(sqrt(t*abs(pathV.alt2[i,j])))*(p*e1[i+1,j] + sqrt(1-p^2)*(e2[i+1,j]))
  } 
tail(pathV.alt2,n=1)
pathV.alt2 <- abs(pathV.alt2)
range(round(pathV.alt2,2))

funcS.alt2 <- for(i in 1:250)
  for(j in 1:1000) {
    pathS.alt2[i+1,j]= pathS.alt2[i,j]*exp(((rf-0.5*(pathV.alt2[i,j]))*t)+(sqrt(t*pathV.alt2[i,j])*e1[i+1,j]))
  } 
tail(pathS.alt2, n=1)

str(X)


SVMprice.alt2<-pathS.alt2[250,1:1000]
str(SVMprice.alt2)
head(SVMprice.alt2)

payoff.alt2<-matrix(rep(0,20000),nrow=20,byrow=T)
for (i in 1:1000)
  for(j in 1:20){
    payoff.alt2[j,i]=as.matrix(SVMprice.alt2[i]-X[j])
  }


payoff.alt2<-ifelse(payoff.alt2<0,0,payoff.alt2)
head(payoff.alt2)
tail(payoff.alt2)



as.matrix(payoff.alt2)
avgpayoff.alt2<-rowMeans(payoff.alt2)


call.alt2<-exp(-rf*1)*avgpayoff.alt2
head(call.alt2,n=1)
str(call.alt2)
dim(X)
dim(call.alt2)
call.alt2<-as.matrix(call.alt2)
head(call.alt2)
call.alt2

## Call Plots ###

plot(X,call.alt2,xlab = "Strike" , ylab = "Call Price Alternate 2" , main= "SVM-Alt. 2 Prices",lty = 2, lwd = 2)

## Compare Call Price with Base Case ##

plot(X,call.alt2,xlab = "Strike" , ylab = "Call Price" , main= "SVM-Alt. 2 Prices  ",lty = 4, lwd = 4 ,pch = "*")
par(new=TRUE)
plot(X,call,xlab = "" , ylab = "" , main= "",lty = 2, lwd = 1)

call.svm.alt2<-cbind(X,call.alt2)

Compare2.calls<-cbind(call.svm.alt2,call)
Compare2.calls.names<-c("Strike Price","Alternate 2","Base Case")
colnames(Compare2.calls)<-Compare2.calls.names
Compare2.calls
diff1<-call[,1]-call.svm.alt2[,2]

######### Compute BSM Implied Volatility for SVM ########
iv.svm.alt2<-rep(0,20)
for (i in 1:20)
{
  iv.svm.alt2[i] <- EuropeanOptionImpliedVolatility(type="call", value=call.svm.alt2[i,2], underlying= s0,
                                                    strike=call.svm.alt2[i,1], dividendYield=0.00, riskFreeRate=rf,
                                                    maturity=1, volatility=0.2)
}

iv.svm.alt2
iv.svm.alt2<-as.matrix(iv.svm.alt2,nrow=20)
iv.svm.alt2.mat<-cbind(X,iv.svm.alt2)
comp.names2<-c("Strike","Alternate 2","Base Case")
iv.svm.alt2.compare<-cbind(iv.svm.alt2.mat,iv.svm)
colnames(iv.svm.alt2.compare)<-comp.names2
iv.svm.alt2.compare

####### Plot Implied volatility from SVM Base Case and Alternative 1 against their respective Strike Prices

plot(X,iv.svm,xlab = "Strike" , ylab = "SVM and Alt2 " , main= "SVM Base Case & Alternate 2 IV against Strike",lty = 2, lwd = 6)
par(new = TRUE)
plot(X,iv.svm.alt2,lty = 2, lwd = 2 , xlab =" " , ylab = " " ,main= " ")+lines(X,iv.svm.alt2,col="red",type="b", lwd=3.5 , pch = "*")

####### Implied Distribution for SVM
ivsvmalt2.dist<-rep(0,20)
for (i in 3:20)
  ivsvmalt2.dist[i] <- (((call.svm.alt2[i,2]-call.svm.alt2[i-1,2])/(call.svm.alt2[i,1]-call.svm.alt2[i-1,1]))-((call.svm.alt2[i-1,2]-call.svm.alt2[i-2,2])/(call.svm.alt2[i-1,1]-call.svm.alt2[i-2,1])))/(call.svm.alt2[i-1,1]-call.svm.alt2[i-2,1])


ivsvmalt2.dist
plot(X,ivsvmalt2.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = "SVM Alterantive 2 IV Dist " , main= "SVM Alt 2 -IV Distribution against Strike") +
  lines(X,ivsvmalt2.dist,col="red",type="b", lwd=1.5)

#####    Comparison   #####
###########################

ivsvm.dist.mat<-cbind(X,ivsvm.dist)
ivsvmdist.alt2.compare<-cbind(ivsvm.dist.mat,ivsvmalt2.dist)
compIVdist2.names<-c("Strike Price","Base Case","Alternate 2")
colnames(ivsvmdist.alt2.compare)<-compIVdist2.names
Alternate2.Comparison <- round(ivsvmdist.alt2.compare,8)
Alternate2.Comparison[c(-1,-2),]

plot(X,ivsvm.dist,xlab = "Strike" , ylab = "SVM and Alt 2 " , main= "SVM Base Case & Alternate 2 IV Dist against Strike",lty = 2, lwd = 6)
par(new = TRUE)
plot(X,ivsvmalt2.dist,col="red",lty = 1, lwd = 2,xlab = "Strike" , ylab = " " , main= "" , pch = "*") +
  lines(X,ivsvmalt2.dist,col="red",type="b", lwd=3 ,pch ="*")




