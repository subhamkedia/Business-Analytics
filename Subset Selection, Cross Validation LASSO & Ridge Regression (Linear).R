
#Answer-1)a)
setwd("~/Desktop/Assignment-2")
data=read.csv("GE.csv")
head(data)

data["return"] <- NA
for (i in 2:nrow(data)){
  data$return[i] = ((data$Close[i] - data$Close[i-1])/(data$Close[i-1]))
}

data["return_1D"] <- NA
for (i in 3:nrow(data)){
  data$return_1D[i] = (data$return[i-1])
}

head(data)

data["return_5D"] <- NA
for (i in 7:nrow(data)){
  data$return_5D[i] = (data$Close[i-1] - data$Close[i-6])/(data$Close[i-6])
}
data = na.omit(data)
head(data)
tail(data)

avg_return_1D = mean(data$return_1D)
avg_return_5D = mean(data$return_5D)
avg_return_1D
avg_return_5D

#Answer-1)b)
data$Date = as.Date(data$Date, format='%Y-%m-%d')

mask = (format(data$Date, "%Y") == "2016")
traindata = data[mask, ]
testdata = data[!mask, ]

fit1 = lm(return ~ return_1D + return_5D, data=traindata)
summary(fit1)

#Answer-1)c)
testdata["pred"] <- NA
testdata$pred = predict(fit1, newdata=testdata)

average_return = mean(testdata$pred)
average_return

inv=1
for (j in 1:nrow(testdata)){
  tmp = testdata$pred[j]
  act = testdata$return[j]
  if(tmp >= 0){
    inv = (inv * (1+act))}
  else{
    inv = (inv * (1-act))}
}
print (inv)


##Answer-2)a)
data2=read.csv("CollegeData.csv")
data_dict=read.csv("CollegeDataDictionary.csv")

data2 = na.omit(data2)
dim(data2)

#Answer-2)b)
head(data2)
data2["col1"] <- NA
data2["col2"] <- NA
data2["col3"] <- NA
data2["col4"] <- NA
data2$col1 = sqrt(data2$COSTT4_A)
data2$col2 = sqrt(data2$TUITIONFEE_OUT)
data2$col3 = sqrt(data2$TUITFTE)
data2$col4 = sqrt(data2$AVGFACSAL)
head(data2)

data2.mat = model.matrix(~(COSTT4_A+TUITIONFEE_OUT+TUITFTE+AVGFACSAL)^2, data2)
matrix1 <- as.data.frame(data2.mat)

data2["int1"] <- NA
data2["int2"] <- NA
data2["int3"] <- NA
data2["int4"] <- NA
data2["int5"] <- NA
data2["int6"] <- NA

data2$int1 =  matrix1$`COSTT4_A:TUITIONFEE_OUT`
data2$int2 = matrix1$`COSTT4_A:TUITFTE`
data2$int3 = matrix1$`COSTT4_A:AVGFACSAL`
data2$int4 = matrix1$`TUITIONFEE_OUT:TUITFTE`
data2$int5 = matrix1$`TUITIONFEE_OUT:AVGFACSAL`
data2$int6 = matrix1$`TUITFTE:AVGFACSAL`

head(data2)
dim(data2)

mean(data2$COSTT4_A)
mean(data2$TUITIONFEE_OUT)
mean(data2$TUITFTE)
mean(data2$AVGFACSAL)
mean(data2$col1)
mean(data2$col2)
mean(data2$col3)
mean(data2$col4)
mean(data2$int1)
mean(data2$int2)
mean(data2$int3)
mean(data2$int4)
mean(data2$int5)
mean(data2$int6)

#Answer-2)c)
set.seed(4574)
train = sample(1:nrow(data2),0.75*nrow(data2))
test = -train

mean(data2[train,]$SAT_AVG)
mean(data2[test,]$SAT_AVG)

#Answer-2)d)
#install.packages("ISLR")
#install.packages("leaps")
#install.packages("glmnet")
library(ISLR)
library(leaps)
library(glmnet)

data2.train = data2[train,]
data2.test = data2[test,]
data2.train = data2.train[,-1]
data2.test = data2.test[,-1]

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

regfit.best=regsubsets(SAT_AVG~., data=data2.train, nvmax=8, really.big = T, method="forward")
k=5
set.seed(1)
folds=sample(1:k,nrow(data2.train),replace=TRUE)
cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))
for(j in 1:k){
  best.fit=regsubsets(SAT_AVG~., data=data2.train[folds!=j,], nvmax=8, really.big = T, method="forward")
  for(i in 1:8){
    pred=predict.regsubsets(best.fit,data2.train[folds==j,],id=i)
    cv.errors[j,i]=mean((data2.train$SAT_AVG[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

best.model = which.min(mean.cv.errors)
best.model
regfit.full = regsubsets(SAT_AVG~., data=data2.train, really.big = T, nvmax=8, method = "forward")
summary(regfit.full)

pred=predict.regsubsets(regfit.full, data2.test, best.model)
actual = data2.test$SAT_AVG
mean((actual - pred)^2)

#Answer-2)e)
x=model.matrix(SAT_AVG~.,data2[,-1])
y=data2$SAT_AVG
k=5
grid=c(0,0.001,0.01,0.1,1,10,100,1000)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,lambda=grid,nfolds=5)
bestlam=cv.out$lambda.min
bestlam
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

#Answer-2)f)
mean((lasso.pred-y[test])^2)

#Answer-2)e)
out=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

