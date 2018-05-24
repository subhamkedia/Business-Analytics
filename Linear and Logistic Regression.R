#Answer-1
#Part-(a)
#setwd("~/Desktop/Studies/Business Analytics/Assignment-1")
eggData = read.csv("EggProduction.csv")  
summary(eggData)
attach(eggData)
plot(feed,eggs)
plot(temperature,eggs)

#Part-(b)
fit1= lm(eggs ~ feed, data=eggData)
summary(fit1)

#Part-(c)
fit2= lm (eggs ~feed + temperature, data=eggData)
summary(fit2)

#Part-(d)
plot(temperature,feed)
eggData$TempCat <- cut(eggData$temperature, breaks=c(-Inf, 0, 35, Inf), labels=c(1,2,3))
head(eggData)

attach(eggData)
summary(TempCat)

#Part-(e)
fit3=lm (eggs~ feed + temperature + TempCat, data=eggData)
summary(fit3)

#Part-(g)
confint(fit3, level=0.95)

#Part-(h)
predict(fit3, data.frame(feed=26, temperature=-2, TempCat="1"), interval="prediction", level=0.99)



#Answer-(2)
#Part-(a)
cardata=read.csv("NomisB_e-Car_Data.csv")
head(cardata)
mask1= (FICO>=675 & FICO<=725 & Amount>=30000 & Amount<=40000 & Term==60 & Car_Type=="N")

attach(cardata)
cardata1=cardata[mask1,]
head(cardata1)
detach(cardata)

#plot(Rate,Outcome)

attach(cardata1)
fit1= lm(Outcome ~ Rate, data=cardata1)
summary(fit1)
plot(Rate,Outcome)
abline(fit1)

#Part-(b)
fit2 = glm(Outcome ~ Rate, family = binomial)
summary(fit2)
plot(Rate,Outcome)
curve(predict(fit2, newdata = data.frame(Rate = x), type = "response"), add=TRUE)

res_fit1 = (Outcome - fitted.values(fit1))
res_fit2 = (Outcome - fitted.values(fit2))
mse_fit1 = mean(res_fit1^2)
mse_fit2 = mean(res_fit2^2)
print (mse_fit1)
print (mse_fit2)

#Part-(c)
fit3=lm(Outcome ~ Rate + Competition_Rate, data=cardata1)
summary(fit3)
detach(cardata1)

#Part-(d) by dividing the data set into train data and test data and then calculating MSE
attach(cardata)
head(cardata)
mask2= (FICO>=675 & FICO<=725 & Amount>=30000 & Amount<=40000 & Term==60 & Car_Type=="N" & Partner_Bin>=2 & Partner_Bin<=3)
traindata=cardata[mask2,]
head(traindata)

mask3= (FICO>=675 & FICO<=725 & Amount>=30000 & Amount<=40000 & Term==60 & Car_Type=="N" & Partner_Bin==1)
testdata=cardata[mask3,]
head(testdata)
detach(cardata)

fit4=lm(Outcome ~ Rate + Competition_Rate, data=traindata)
summary(fit4)
coeff= array(fit4$coefficients)
print (coeff)

testdata["pred"] <- NA
testdata$pred <- (coeff[1]+(coeff[2]*testdata$Rate)+(coeff[3]*testdata$Competition_Rate))
head(testdata)

testdata["pred1"] <- NA
testdata$pred1= (testdata$pred - testdata$Outcome) ** 2
head(testdata)

mse=(sum(testdata$pred1))/length(testdata$pred1)
print(mse)

#Part-(d) using the predict() function
predct = predict(fit4, newdata=testdata)
y=testdata$Outcome
mse1=mean((predct-y)**2)
print(mse1)
