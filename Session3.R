#Programming and Data Analysis

#creating functions

NumericPred=read.csv("Session 3/data/NumericPred.csv")
View(NumericPred)

colnames(NumericPred) = c("target","model1","model2") #changing col names


#MAD : Mean Absolute Deviation
#MSE : Mean Squared Error
#MAPE : Mean Absolute Percentage Error 
#MPSE : Mean Percentage Squared Error
#R2 : R Square = 1-(SSE/SST)    ##R2 can be negative when the model is worse than baseline model


a = NumericPred$target
m2 = NumericPred$model1

metrics = c(MAD = 0, MSE=0, MAPE=0,MPSE=0,R2=0,TMAD=0)
metrics["MAD"] = mean(abs(a-m))
metrics["MSE"] = mean((a-m)^2)
metrics["MAPE"] = mean(abs((a-m)/a))
metrics["MPSE"] = mean(((a-m)/a)^2)
SSE=sum((a-m)^2)
SST=sum((a-mean(a))^2)
metrics["R2"] = 1-(SSE/SST)
metrics["TMAD"] = mean(abs(a-m),trim = 0.05)


nummertics = function(a,m) #creating function named nummertrics
{
  #MAD : Mean Absolute Deviation
  #MSE : Mean Squared Error
  #MAPE : Mean Absolute Percentage Error 
  #MPSE : Mean Percentage Squared Error
  #R2 : R Square = 1-(SSE/SST)    ##R2 can be negative when the model is worse than baseline model
  
  metrics = c(MAD = 0, MSE=0, MAPE=0,MPSE=0,R2=0,TMAD=0)
  metrics["MAD"] = mean(abs(a-m))
  metrics["MSE"] = mean((a-m)^2)
  metrics["MAPE"] = mean(abs((a-m)/a))
  metrics["MPSE"] = mean(((a-m)/a)^2)
  SSE=sum((a-m)^2)
  SST=sum((a-mean(a))^2)
  metrics["R2"] = 1-(SSE/SST)
  metrics["TMAD"] = mean(abs(a-m),trim = 0.05)
  
  return(metrics)
}

nummertics(a=NumericPred$target,m=NumericPred$model1)
nummertics(a=NumericPred$target,m=NumericPred$model2)


#Save the function
dump("nummertics" , file = "Mymodelfunction.R")

source(file.choose()) #choose the function to load
nummertics
args(nummertics)


cor(NumericPred$model1,NumericPred$model2)

ensemble = lm(target ~ model1 + model2, data=NumericPred) #creating ensemble model
summary(ensemble)

#save the ensemble model in data set under new col name 'model3'
NumericPred$model3 = -1.113+0.3*NumericPred$model1+0.75*NumericPred$model2
View(NumericPred)

nummertics(NumericPred$target,NumericPred$model3)
nummertics(NumericPred$target,NumericPred$model2)
#It can be observed that model3 performs better 

NumericPred$baseline = mean(NumericPred$target) #create baseline model
nummertics(NumericPred$target,NumericPred$baseline) #R2 for baseline is zero

#plot and lines
plot(NumericPred$target,NumericPred$model3)
lines(NumericPred$target,NumericPred$target, lwd=3, col="blue")

#plot and points
plot(NumericPred$target,NumericPred$target, col="blue", pch=19)
points(NumericPred$target, NumericPred$model3)

s1= subset(NumericPred, abs(NumericPred$target - NumericPred$model3)>5)
#s1 = NumericPred[abs(NumericPred$target-NumericPred$model3)>5,]
points(s1$target, s1$model3, col="red", pch=20)

a=NumericPred$target
m=NumericPred$model3
b=NumericPred$baseline
tcost=sum(ifelse(abs(a-m)<5,0,2*abs(a-m))) #model3 cost
tcost=sum(ifelse(abs(a-b)<5,0,2*abs(a-b))) #baseline cost


#------ Feb 13, 2017--------#

BinaryPred <- read_csv("D:/MSBAPM/Sem 2/OPIM 5503- Data Analytics using R/Session 3/data/BinaryPred.csv")
colnames(BinaryPred) = c("target", "model1", "model2")
View(BinaryPred)
a = BinaryPred$target
m = BinaryPred$model1
k=10

#LL : Log Likelihood
#AIC : -2*LL + 2*K
#BIC : -2*LL + 2*K*log(n)
#R2 : 1-SSE/SST

metrics = c(LL=0, AIC = 0, BIC = 0, R2 = 0)
metrics["LL"] = sum(ifelse(a==1, log(m), log(1-m)))
metrics["AIC"] = -2*metrics["LL"] + 2*k
metrics["BIC"] = -2*metrics["LL"] + 2*k*log(length(a))
SST = sum((a-mean(a))^2)
SSE = sum((a-m)^2)
metrics["R2"] = 1-(SSE/SST)

metrics


binmetrics = function(a,m,k=10){
  
  #LL : Log Likelihood
  #AIC : -2*LL + 2*K
  #BIC : -2*LL + 2*K*log(n)
  #R2 : 1-SSE/SST
  
  metrics = c(LL=0, AIC = 0, BIC = 0, R2 = 0)
  metrics["LL"] = sum(ifelse(a==1, log(m), log(1-m)))
  metrics["AIC"] = -2*metrics["LL"] + 2*k
  metrics["BIC"] = -2*metrics["LL"] + 2*k*log(length(a))
  SST = sum((a-mean(a))^2)
  SSE = sum((a-m)^2)
  metrics["R2"] = 1-(SSE/SST)
  metrics
  
  return(metrics)  
}
binmetrics(a=BinaryPred$target, m=BinaryPred$model1)
binmetrics(a=BinaryPred$target, m=BinaryPred$model2)

m=BinaryPred$model1
p=0.7
mnew = ifelse(m>p,1,0)
contab = table(factor(mnew), factor(a)) #table(row, col)
#note use of factor. Create confusion matrix here
vec = as.vector(contab)
vec
# TN, FP, FN, TP

incre = 0.05
cutoffs = seq(min(m),max(m),by=incre)
cutoffs

binresult  = data.frame()
for(p in cutoffs)
{
  mnew = ifelse(m>p,1,0)
  contab = table(factor(mnew), factor(a))
  vec = as.vector(contab)
  # TN, FP, FN, TP
  vec = c(p,vec)
  binresult = rbind(binresult, vec)
}
colnames(binresult) = c("cutoff", "TN", "FP", "FN", "TP")
View(binresult)

bincf = function(a,m,incre=0.05)
{
  cutoffs = seq(min(m),max(m),by=incre)
  binresult  = data.frame()
  
  for(p in cutoffs)
  {
    mnew = ifelse(m>p,1,0)
    contab = table(factor(mnew), factor(a))
    vec = as.vector(contab)
    # TN, FP, FN, TP
    vec = c(p,vec)
    binresult = rbind(binresult, vec)
  }
  
  colnames(binresult) = c("cutoff", "TN", "FP", "FN", "TP")
  
  #create plot of TP vs FP
  #TP = Sensitivity
  #FP = 1-Specificity
  x1 = binresult$FP/max(binresult$FP)
  y1 = binresult$TP/max(binresult$TP)
  plot(x1,y1,type = "l") #ROC curve
  AUC = round(mean(y1),2) #because it is 0 to 1
  text(0.5,0.9,paste("AUC = ",AUC))
  
  return(binresult)
}

z=bincf(a=BinaryPred$target,m=BinaryPred$model2)
View(z)

x1 = z$FP/max(z$FP)
y1 = z$TP/max(z$TP)
plot(x1,y1,type = "l") #ROC curve
AUC = round(mean(y1),2) #because it is 0 to 1
text(0.5,0.9,paste("AUC = ",AUC))
