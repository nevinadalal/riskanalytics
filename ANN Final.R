rm(list = ls())
library(neuralnet)
nn<-read.csv(choose.files(),header = TRUE, sep = ",")
View(nn)
apply(nn,2, function(x) sum(is.na(x)))
samplesize<-0.70*nrow(nn)
set.seed(80)
index<-sample(seq_len(nrow(nn)),size = samplesize)
datatrain<-nn[index,]
datatest<-nn[-index,]
#Scaling
maxs<- apply(nn, 2, max)
mins<- apply(nn, 2, min)
scaled<- as.data.frame(scale(nn, center = mins, scale= maxs-mins))
#fit neural network
library(neuralnet)
trainNN<-scaled[index,]
testNN<-scaled[-index,]
#fit neural 
set.seed(2)
#n<- names(trainNN)
View(nn)
# updated Hybrid
CX<-nn$C.X-nn$Q.X
CX[index]
#f<- as.formula(paste("Q.X~.c(T, Volatility, F.X)",paste(n[!n %in% "Q.X"],collapse = "+")))
nnet<- neuralnet(~T+volatility+S.X, data=trainNN, hidden =c(5), linear.output = FALSE)
plot(nnet)
pr.nn<- compute(nnet, testNN[,c(5,7,8)])
pr.nn_<- pr.nn$net.result*(max(nn$Quoted)-min(nn$Quoted))+min(nn$Quoted)
test.r<-(testNN$Quoted)*(max(nn$Quoted)-min(nn$Quoted))+min(nn$Quoted)
MSE.nn<- sum((test.r-pr.nn_)^2)/nrow(testNN)
#######

###Hybrid
nnh<-read.csv(choose.files(),header = TRUE, sep = ",")
indexh<-sample(seq_len(nrow(nnh)),size = samplesize)
datatrainh<-nnh[index,]
datatesth<-nnh[-index,]
hybrid<-datatesth$C.X-pr.nn_
###Values
View(datatesth)

#BSmodified<-read.csv(choose.files(),header = TRUE,stringsAsFactors = FALSE)
#View(BSmodified)
#install.packages("plot3D")
library(plot3D)
x<-datatesth$F.X
y<-datatesth$T
z<-hybrid
npoints<- 21
scatter3D(x,y,z, box=TRUE, pch=16, bty = "b2", axes=TRUE, label = TRUE, nticks=5, ticktype="detail", theta = 40, phi = 40)
scatter3D(x, y, z,col = NULL, colvar = NULL,add = FALSE,theta = 15, phi = 20,bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",fit="smooth",type="l")


#######Bootstrap
maxsh<- apply(hybrid, 2, max)
minsh<- apply(hybrid, 2, min)
scaledh<- as.data.frame(scale(hybrid, center = minsh, scale= maxsh-minsh))
h<- scaledh[hybrid]
shapiro.test(h)
#####
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
plot(datatesth$S.X,hybrid)
u<- hybrid+sh
l<- hybrid-sh
plot(z)
datatesth$C.X
abline(a=-0.6389565,b=0)
abline(a=1.0350254 , b=0)
?abline
lines(y=u, x=datatesth$S.X)
lines(y=l,x=datatesth$S.X)
