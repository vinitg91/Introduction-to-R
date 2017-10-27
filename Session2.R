# Odds and ends

car1 <- read_csv("D:/MSBAPM/Sem 2/OPIM 5503- Data Analytics using R/Session 2/Data/car insurance.csv")
colnames(car1)[5] = "amount" #change column name

mean(car1$veh_value)
attach(car1) #column names become part of search
mean(veh_value)

write.csv(car1,file = "car2.csv")

install.packages("vcd")
library("vcd")
library(car)
library(dplyr)
library(MASS)
View(whiteside)

s1 = whiteside[whiteside$Temp<7,]
View(s1)
mean(s1$Gas)
s2 = whiteside[whiteside$Insul=="Before",]
mean(s2$Gas)

View(Arthritis)
table(Arthritis$Treatment)
x=table(Arthritis$Treatment, Arthritis$Improved)
prop.table(x) #Proportions w.r.t overall data
prop.table(x,margin = 1) #Proportions w.r.t row data. Row sum =100%
prop.table(x,margin = 2) #Proportions w.r.t column data. Col sum =100%

y=table(Arthritis$Treatment, Arthritis$Improved, Arthritis$Sex)
ftable(y)
#create bins
agegroups = cut(Arthritis$Age,breaks = c(23,40,60,74)) #Does not include the lowest value
agegroups
agegroups = cut(Arthritis$Age,breaks = quantile(Arthritis$Age))
agegroups = cut(Arthritis$Age,breaks = c(23,40,60,74),include.lowest = TRUE) #includes lowest
agegroups
table(agegroups)
table(agegroups,Arthritis$Improved)

#Summarizing Numeric Data

View(Salaries)
tapply(Salaries$salary,Salaries$sex, mean)
aggregate(Salaries$yrs.since.phd,list(Salaries$sex, Salaries$rank), mean)
aggregate(Salaries$yrs.since.phd,list(Salaries$sex), mean)

#dplyr
attach(whiteside)
attach(Salaries)
select = dplyr::select
x = select(whiteside,c(Insul,Gas))
View(x)
formula1 = (sex=="Male" & rank=="Prof")|salary>100000 
f1 = filter(Salaries,formula1) 
View(f1)
Salaries = arrange(Salaries,sex,rank) 
Salaries = arrange(Salaries,sex,desc(rank)) 
View(Salaries)


# Graph and visualization
plot(Insul)
plot(Insul, Gas)
plot(Temp, Gas, main="Graph", pch=19,cex=2,type="l")

plot(Temp, Gas)
abline(v=mean(Temp),lwd=3,col="magenta")
label1 = paste("Avg Temp =",mean(Temp))
text(5,6,label1, srt=90)
abline(h=mean(Gas),lwd=3,col="yellow")
label2 = paste("Avg Gas =",round(mean(Gas),2))
text(3.8,4.3,label2)

s1 = whiteside[Insul=="Before",]
points(s1$Temp,s1$Gas, pch =19, col="green")

s2 = whiteside[Temp<mean(Temp) & Gas<mean(Gas),]
points(s2$Temp,s2$Gas, pch =19, cex=2, col="red")


opar=par()
par(bg="white", mfrow=c(2,2), las=2,col="red")
plot(Temp,Gas) 
plot(Temp,Gas,type="h") 
plot(Insul,Gas,ylab="Gas") 
plot(Insul,Temp,ylab="Temp")
par(opar)

#Other graph types
boxplot(Gas~Insul,horizontal=T,
        col=c("green","yellow"), notch=T)
#Notch is 95% confidence interval for the median value

stripchart(Gas~Insul,pch=19,col=c("red","blue"),method="stack")
stripchart(Gas~Insul,pch=19,col=c("red","blue"),method="jitter")

x = aggregate(salary,list(rank),mean)
pie(x$x,labels = x$Group.1,col=rainbow(3))
dotchart(x$x,labels = x$Group.1,col=rainbow(3),pch=10)


#--------6 Feb, 2017-----------------------------------------------------------

hist(whiteside$Gas)
density(whiteside$Gas) #density of Gas. Range always 0 to 1 as it shows probability
plot(density(whiteside$Gas)) #draw density plot
polygon(density(whiteside$Gas),col="red") #draw and color the density plot

symbols(Salaries$yrs.service,Salaries$salary, circles = Salaries$yrs.since.phd, inches = 0.1, bg="green")
x = as.matrix(UScereal[,c(3,4)]) #creating a matrix to capture only col 3 and 4
symbols(UScereal$sugars,UScereal$calories,rectangles = x, inches = 0.4, bg="blue") #plotting rectangles

x1=rnorm(10000) #random numbers
y1=rnorm(10000)
plot(x1,y1)
smoothScatter(x1,y1)

x1=runif(10000) #uniform numbers
y1=runif(10000)
plot(x1,y1)

plot(Boston$ptratio,Boston$tax)
sunflowerplot(Boston$ptratio,Boston$tax)
sunflowerplot(UScereal$fat,UScereal$protein)
smoothScatter(x1,y1)

install.packages("rgl") #3D plots ke liye 
library(rgl)
plot3d(Boston$crim,Boston$nox,Boston$ptratio)


l1 = c("US","Germany", "Spain", "UK")
v1 = c(17,12,6,31)
pie(v1 ,labels = l1, col = rainbow(4))

install.packages("plotrix")
library(plotrix)

fan.plot(v1,labels = l1)

install.packages("vioplot")
library(vioplot)

vioplot(whiteside$Gas) #Box plot and density plot together


install.packages("corrgram")#very useful for correlation
library(corrgram)

corrgram(Boston)
corrgram(Boston,order = TRUE)
corrgram(Boston,order = TRUE,upper.panel = panel.pie)
corrgram(Boston,order = TRUE,upper.panel = panel.pie,lower.panel = panel.cor)


