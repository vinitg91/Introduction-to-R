
#--------------------------------------------------------------------------------------------
#Data Objects

weight = c(60,72,57,95,90,72)
height = c(1.75,1.8,1.65,1.9,1.74,1.91)
mean(weight)
sd(height)
quantile(weight)
quantile(weight,0.75)
quantile(weight,probs = c(0.2,0.75))
length(height)
range(weight)
t.test(weight,mu = 80)
bmi = (weight)/(height^2)
bmi
plot(height,weight)
hist(weight)
plot(height,weight, pch=7,col="Blue", main="Height vs Weight")
colors()
weight = c(weight,86)
height = c(height,NA)
height
mean(height,na.rm = TRUE)
gender = c("M","F","M","M","F","M","F")
names(gender)
names(gender)=c("Bob","Susan","Jim","Mary","Jane","Tim","Nicole")
plot(factor(gender),weight) #Important here is to treat Gender as factor as its categorical


#--------------------------------------------------------------------------------------------
#Creating Vectors

x= seq(1,15,2) #seq(Start, End, StepSize)
x
x =seq(1,15,length.out = 4)
x

rep("A",5)
rep(c("A","B"),5)
rep(c("A","B"),c(5,7))
c(rep("A",5),rep("B",7))

weight[weight>70]

#--------------------------------------------------------------------------------------------
#Indexing and Subsetting

weight[-3]
height[height>1.7 & is.na(height)==F]



#--------------------------------------------------------------------------------------------
#Data Frame

ghw=data.frame(gender, height, weight)
ghw
str(ghw)
summary(ghw)
ghw[1,]
ghw[c(3,5),]


#--------------------------------------------------------------------------------------------
#Viewing and Editing

edit(ghw) #Very Useful
fix(ghw)
View(ghw)



#--------------------------------------------------------------------------------------------
#Indexing data frames

ghw$height

s1=ghw[ghw$gender=="F",]
s1
edit(s1)


View(whiteside)
plot(whiteside$Insul,whiteside$Gas)


#--------------------------------------------------------------------------------------------
#data frame
str(whiteside)
str(ghw)
summary(ghw)
dim(ghw)
row.names(whiteside)
row.names(ghw)

rownames(ghw)
