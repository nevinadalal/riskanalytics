rm(list = ls())
nn<-read.csv(choose.files(),header = TRUE, sep = ",")
CX<- nn$C.X-nn$Q.X
nn$CX<-CX
apply(nn,2, function(x) sum(is.na(x)))
samplesize<-0.70*nrow(nn)
index<-sample(seq_len(nrow(nn)),size = samplesize)
datatrain<-nn[index,]
datatest<-nn[-index,]
View(nn)
maxs<- apply(nn, 2, max)
mins<- apply(nn, 2, min)
scaled<- as.data.frame(scale(nn, center = mins, scale= maxs-mins))
#fit neural network
library(neuralnet)
trainNN<-scaled[index,]
testNN<-scaled[-index,]

##nn
nnet<- neuralnet(CX~T+volatility+S.X, data=trainNN, hidden =c(6), linear.output = FALSE)
plot(nnet)
pr.nn<- compute(nnet, testNN[,c(5,7,8)])
pr.nn_<- pr.nn$net.result*(max(nn$CX)-min(nn$CX))+min(nn$CX)
test.r<-(testNN$CX)*(max(nn$CX)-min(nn$CX))+min(nn$CX)
MSE.nn<- sum((test.r-pr.nn_)^2)/nrow(testNN)
#####
hybrid<-datatest$C.X-pr.nn_
library(plot3D)
x<-datatest$F.X
y<-datatest$T
z<-hybrid
npoints<- 21
scatter3D(x,y,z, box=TRUE, pch=16, bty = "b2", axes=TRUE, label = TRUE, nticks=5, ticktype="detail", theta = 40, phi = 40)
####bootstrap
n<- length(hybrid)
u0<-mean(hybrid)
sh<- sd(hybrid)/sqrt(n)
thetas<- NULL
tstar<- NULL
for(i in 1:3000){
  xx<- sample(hybrid,n, replace = TRUE)
  u<- mean(xx)
  thetas[i]<- u
  tstar[i]<- (u-u0)/(sd(xx)/sqrt(2))##pivotal quantity
}
c(u,mean(thetas))
summary(tstar)
quantile(tstar, probs = c(0.05,0.95))
u0+quantile(tstar, probs = c(0.05,0.95))*sh
plot(datatest$S.X,datatest$Quoted)
uhybrid<-hybrid*datatest$K
View(uhybrid)
u<- uhybrid+(sh*440)
View(l)
l<- uhybrid-(sh*440)
plot(z)
datatesth$C.X
abline(a=-0.6389565,b=0)
abline(a=1.0350254 , b=0)
?abline
lines(y=u, x=datatest$S.X)
lines(y=l,x=datatest$S.X)
View(datatest$Quoted)
