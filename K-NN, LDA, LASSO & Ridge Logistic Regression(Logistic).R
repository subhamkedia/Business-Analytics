
#Assignment-3
setwd("~/Desktop/Assignment-3")
data=read.csv("OrangeJuice.csv")
head(data)

data=data[,-(12:14)]
head(data)
data$StoreID = as.factor(data$StoreID)

#Answer-1)a)
set.seed(1337)
train = sample(1:nrow(data),0.5*nrow(data))
temp = -train
head(data)
data.train = data[train,-1]
data.temp = data[temp,-1]

valid = sample(1:nrow(data.temp),0.5*nrow(data.temp))
test = -valid

data.valid = data.temp[valid,]
data.test = data.temp[test,]

summary(data.train)

#Answer-1)b)
glm.fit = glm(Purchase~. , data=data.train, family = binomial)
summary(glm.fit)

#Answer-1)c)
library(glmnet)
grid=10^(-3:3)
x=model.matrix(Purchase~.,data.train)[,-1]
y=data.train$Purchase

logfit = glmnet(x, y, alpha = 1, family="binomial")
cv.out = cv.glmnet(x, y, alpha=1, lambda=grid, family="binomial", nfolds=10)
names(cv.out)
bestlam = cv.out$lambda.min
bestlam
lasso.mod = glmnet(x, y, alpha=1, lambda=bestlam, family="binomial")
lasso.mod

#Answer-1)d)
#install.packages("tree")
library(tree)
fit_tree = tree(Purchase~., data.train)
summary(fit_tree)
plot(fit_tree)
text(fit_tree,pretty=0)
fit_tree

#Considering Deviance and doing cross validation
cv.fit_tree = cv.tree(fit_tree)
par(mfrow=c(1,2))
plot(cv.fit_tree$size,cv.fit_tree$dev,type="b")
plot(cv.fit_tree$k,cv.fit_tree$dev,type="b")
prune.fit_tree=prune.tree(fit_tree,best=4)
plot(prune.fit_tree)
text(prune.fit_tree,pretty=0)

#Considering Classification Error Rate and doing cross validation
cv.fit_tree1 = cv.tree(fit_tree, FUN=prune.misclass)
par(mfrow=c(1,2))
plot(cv.fit_tree1$size,cv.fit_tree1$dev,type="b")
plot(cv.fit_tree1$k,cv.fit_tree1$dev,type="b")
prune.fit_tree1 = prune.tree(fit_tree,best=2)
plot(prune.fit_tree1)
text(prune.fit_tree1,pretty=0)

#Answer-1)e)
library(MASS)
lda.fit = lda(Purchase~.,data = data.train)
lda.fit
lda.pred=predict(lda.fit, data.train)
lda.class = lda.pred$class
table(lda.class,data.train$Purchase)
mean(lda.class!=data.train$Purchase)

#Answer-1)f)
library(class)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
temp1 = c(1,3)
orange_norm <- as.data.frame(lapply(data.train[,-temp1], normalize))
orange_norm["StoreID"] <- NA
orange_norm$StoreID = data.train[,3]
#ind <- sample(2, nrow(data.train), replace=TRUE, prob=c(0.67, 0.33))
#orange.training <- orange_norm[ind==1, 1:9]
#orange.test <- orange_norm[ind==2, 1:9]
#orange.trainLabels <- data.train[ind==1, 1]
#orange.testLabels <- data.train[ind==2, 1]
#orange_pred <- knn(train = orange.training, test = orange.test, cl = orange.trainLabels, k=3)
#table(orange_pred, orange.testLabels)
#dim(data.train)
err_mean = c()
for (i in 1:25){
  orange_err <- knn.cv(train = orange_norm, cl = data.train[,1], k=i)
  err_mean[i]=mean(orange_err!=data.train[,1])
}
best_k=which.min(err_mean)
print(best_k)
orange_avg <- knn(train=orange_norm, test=orange_norm, cl=data.train[,1], k=best_k)
avg_err=mean(orange_avg!=data.train[,1])
print(avg_err)

#Answer-1)g)
#LASSO error on validation data
x1=model.matrix(Purchase~.,data.valid)[,-1]
y1=data.valid$Purchase
pred = predict(lasso.mod, x1, type="class")
table(pred, y1)
lasso_error = mean(pred!=y1)
lasso_error

#TREE error on validation data
tree.pred1 = predict(prune.fit_tree, data.valid, type="class")
table(tree.pred1,data.valid$Purchase)
tree_error = mean(tree.pred1!=data.valid$Purchase)
tree_error

#LDA error on validation data
lda.pred1=predict(lda.fit, data.valid)
names(lda.pred1)
lda.class1 = lda.pred1$class
table(lda.class1,data.valid$Purchase)
lda_error = mean(lda.class1!=data.valid$Purchase)
lda_error

#KNN error on validation data
temp3 = c(1,3)
temp4 = c(3)
orange_norm1 <- as.data.frame(lapply(data.valid[,-temp3], normalize))
orange_norm1["StoreID"] <- NA
orange_norm1$StoreID = data.valid[,3]
orange_avg1 <- knn(train=orange_norm, test=orange_norm1, cl=data.train[,1], k=best_k)
table(orange_avg1,data.valid$Purchase)
knn_avg_err1 = mean(orange_avg1!=data.valid[,1])
knn_avg_err1

plotting = c(lasso_error, tree_error, lda_error, knn_avg_err1)
barplot(plotting, xlab="Models",ylab="Error Rate",names.arg=c("LASSO","TREE","LDA","KNN"))

#Answer-1)h)
#Fitting best model on (test+validation) data and running on test data
data2 = rbind(data.train, data.valid)
dim(data2)
x2=model.matrix(Purchase~.,data2)[,-1]
y2=data2$Purchase
x3=model.matrix(Purchase~.,data.test)[,-1]
y3=data.test$Purchase
final.mod = glmnet(x2, y2, alpha=1, lambda=bestlam, family="binomial")
final_pred = predict(final.mod, x3, type="class")
table(final_pred, y3)
final_error = mean(final_pred!=y3)
final_error

