setwd("~/Desktop/Assignment-4")

cdata = read.csv("CollegeData.csv")
dim(cdata)
head(cdata)
cdata = na.omit(cdata)
dim(cdata)

# Answer-1)a)
data1 = cdata[,-1]
head(data1)
lin_fit = lm(SAT_AVG ~., data = data1)
summary(lin_fit)

# Answer-1)b)
cdata$INSTNM = as.factor(cdata$INSTNM)
data.scaled = scale(data1)
data.scaled = data.frame(cdata[,1], data.scaled)
colnames(data.scaled)[1] <- "INSTNM"
k_error = matrix(nrow = 50, ncol=2)
k_error[,1] = c(1:50) 
for (i in 1:50){
  km.out1=kmeans(data.scaled[,2:10], centers=i)
  k_error[i,2] = km.out1$tot.withinss
}
temp = which.min(k_error[,2])
best_k = temp
k_error
best_k
km.out=kmeans(data.scaled[,2:10], centers=temp)
km.out$tot.withinss
plot(x = k_error[,1], y = k_error[,2], xlab = "K", ylab = "WSS" )
plot(data.scaled[,2:10], col=km.out$cluster+1, pch=20, lwd=3)

# Answer-1)c)
cdata1.scaled = data.scaled[order(data.scaled$INSTNM),]
cdata1.scaled = data.scaled[(1:100),]
cdata2 = cdata[order(cdata$INSTNM),]
cdata2 = cdata2[(1:100), -1]
cdata2.scaled = scale(cdata2)
km1.out=kmeans(cdata1.scaled[,2:10], centers=4)
km1.out$tot.withinss
plot(cdata1.scaled[,2:10], col=km1.out$cluster+1, pch=20, lwd=3)
names(km1.out)
km1.out$centers

hc.average = hclust(dist(cdata1.scaled[,2:10]), method="average")
plot(hc.average)
clusters = cutree(hc.average, 4)
plot(cdata1.scaled[,2:10], col=clusters+1, pch=10, lwd=3)
names(hc.average)

hc.average1 = hclust(dist(cdata2.scaled), method="average")
plot(hc.average1)
clusters1 = cutree(hc.average1, 4)
plot(cdata2.scaled, col=clusters1+1, pch=10, lwd=3)
names(hc.average1)

fun <-  function (data, clust) {
  nvars=length(data[1,])
  ntypes=max(clust)
  centroids<-matrix(0,ncol=nvars,nrow=ntypes)
  for(i in 1:ntypes) {
    c<-rep(0,nvars)
    n<-0
    for(j in names(clust[clust==i])) {
      n<-n+1
      c<-c+data[j,]
    }
    centroids[i,]<-c/n
  }
  rownames(centroids)<-c(1:ntypes)
  colnames(centroids)<-colnames(data)
  centroids
}
fun(cdata2.scaled, clusters1)

# Answer-1)d)
pr.out = prcomp(data1, scale=T)
pr.out
summary(pr.out)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
par(mfrow=c(1,2))
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", 
     ylim=c(0,1) ,type="b")
plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type="b")

# Answer-1)e)
par(mfrow=c(1,1))
biplot(pr.out, scale=0)

# Answer-1)f)
# install.packages("pls")
library(pls)
y=data1$SAT_AVG
set.seed(1)
train = sample(1:nrow(data1),0.75*nrow(data1))
test = -train
y.test=y[test]

pcr.fit = pcr(SAT_AVG~., data=data1, subset=train, scale=T, 
              validation = "CV", nfolds=5, segments=5)
validationplot(pcr.fit,val.type="MSEP")
x = model.matrix(SAT_AVG~., data1)[,-1]
pcr.pred = predict(pcr.fit, x[test,], ncomp=4)
mean((pcr.pred - y.test)^2)

pls.fit = plsr(SAT_AVG~., data=data1, subset=train, scale=T, 
               validation ="CV", nfolds=5, segments=5)
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")

pls.pred = predict(pls.fit, data1[test,], ncomp=4)
mean((pls.pred - y.test)^2)

# Answer-2)a)
hdata1 = read.csv("home_and_kitchen_online_sales.csv")
hdata2 = read.csv("home_and_kitchen_b&m_sales.csv")
hdata1 = hdata1[,-(8:17)]
hdata2 = hdata2[,-(8:11)]
colnames(hdata1)[1] <- "dma_id"
colnames(hdata2)[1] <- "store_id"
hdata1 = na.omit(hdata1)
hdata2 = na.omit(hdata2)
hdata1$sales = as.numeric(gsub(",","",hdata1$sales))
#hdata1$dma_id = as.factor(hdata1$dma_id)
#hdata1$year = as.factor(hdata1$year)
#hdata1$month = as.factor(hdata1$month)
#hdata1$week = as.factor(hdata1$week)
#hdata1$after = as.factor(hdata1$after)

hdata2$sales = as.numeric(gsub(",","",hdata2$sales))
#hdata2$store_id = as.factor(hdata2$store_id)
#hdata2$year = as.factor(hdata2$year)
#hdata2$month = as.factor(hdata2$month)
#hdata2$week = as.factor(hdata2$week)
#hdata2$after = as.factor(hdata2$after)
#hdata2$usa = as.factor(hdata2$usa)
newdata = data.frame(store_id = unique((hdata2$store_id)), before=NA, after=NA, sales_change=NA, affected_US=NA)
store_change = c()
store_change1 = c()
store_change2 = c()
x = 1
y = 1
w = 1
for (i in unique(hdata2$store_id)){
  usa = unique(hdata2[hdata2$store_id == i,]$usa)
  mask1 = (hdata2$after == 0 & hdata2$store_id == i)
  mask2 = (hdata2$after == 1 & hdata2$store_id == i)
  a = sum(hdata2[mask1,]$sales)
  b = sum(hdata2[mask2,]$sales)
  store_change[i] = ((b - a)/a)*100
  if(newdata[w,]$store_id == i){
    w = w+1
    print(i)
    newdata[newdata$store_id == i,]$before = a
    newdata[newdata$store_id == i,]$after = b
    newdata[newdata$store_id == i,]$affected_US = unique(hdata2[hdata2$store_id == i,]$usa)
    newdata[newdata$store_id == i,]$sales_change = ((b-a)/a)*100
    }
  if(usa == 1){
    store_change1[x] = store_change[i]
    x = x+1
    }
  else if (usa == 0)
    store_change2[y] = store_change[i]
    y = y+1
}
store_change
store_change1
store_change2
store_change2 = na.omit(store_change2)
avg_usa = sum(store_change1)/length(store_change1)
avg_canada = sum(store_change2)/length(store_change2)
avg_usa
avg_canada

# Answer-2)b)
fit1 = lm(sales_change~ affected_US, data = newdata)
summary(fit1)

# Answer-2)c)
dma_change = c()
t = 1
newdata1 = data.frame(dma_id = unique((hdata1$dma_id)), before=NA, after=NA, sales_change=NA, affected=NA)
for (j in unique(hdata1$dma_id)){
  affected = unique(hdata1[hdata1$dma_id == j,]$close)
  mask3 = (hdata1$after == 0 & hdata1$dma_id == j)
  mask4 = (hdata1$after == 1 & hdata1$dma_id == j)
  p = sum(hdata1[mask3,]$sales)
  q = sum(hdata1[mask4,]$sales)
  dma_change[j] = ((q - p)/p)*100
  if(newdata1[t,]$dma_id == j){
    t = t+1
    print(j)
    newdata1[newdata1$dma_id == j,]$before = p
    newdata1[newdata1$dma_id == j,]$after = q
    newdata1[newdata1$dma_id == j,]$affected = unique(hdata1[hdata1$dma_id == j,]$close)
    newdata1[newdata1$dma_id == j,]$sales_change = ((q-p)/p)*100
  }
}
mask5 = (newdata1$affected == 1)
avg_change = (sum(newdata1[mask5, ]$sales_change)/length(newdata1[mask5, ]$sales_change))
avg_change

# Answer-2)d)
fit2 = lm(sales_change~ affected, data = newdata1)
summary(fit2)

# Answer-2)e)
store_change1 = c()
s = 1
newdata2 = data.frame(store_id = unique((hdata2$store_id)), before=NA, after=NA, sales_change=NA, affected_US=NA)
for (i in unique(hdata2$store_id)){
  usa = unique(hdata2[hdata2$store_id == i,]$usa)
  mask5 = (hdata2$after == 0 & hdata2$store_id == i & ((hdata2$week >= 30 & hdata2$week <= 53) | (hdata2$week >= 1 & hdata2$week <= 2)))
  mask6 = (hdata2$after == 1 & hdata2$store_id == i & ((hdata2$week >= 30 & hdata2$week <= 53) | (hdata2$week >= 1 & hdata2$week <= 2)))
  a = sum(hdata2[mask5,]$sales)
  b = sum(hdata2[mask6,]$sales)
  store_change1[i] = ((b - a)/a)*100
  if(newdata2[s,]$store_id == i){
    s = s+1
    print(i)
    newdata2[newdata2$store_id == i,]$before = a
    newdata2[newdata2$store_id == i,]$after = b
    newdata2[newdata2$store_id == i,]$affected_US = unique(hdata2[hdata2$store_id == i,]$usa)
    newdata2[newdata2$store_id == i,]$sales_change = ((b-a)/a)*100
  }
}

head(newdata2)
mas1 = (newdata2$affected_US == 1)
mas2 = (newdata2$affected_US == 0)
avg_usa1 = (sum(newdata2[mas1, ]$sales_change)/length(newdata2[mas1, ]$sales_change))
avg_canada1 = (sum(newdata2[mas2, ]$sales_change)/length(newdata2[mas2, ]$sales_change))
avg_usa1
avg_canada1

fit3 = lm(sales_change~ affected_US, data = newdata2)
summary(fit3)

dma_change1 = c()
u = 1
newdata3 = data.frame(dma_id = unique((hdata1$dma_id)), before=NA, after=NA, sales_change=NA, affected=NA)
for (j in unique(hdata1$dma_id)){
  affected = unique(hdata1[hdata1$dma_id == j,]$close)
  mask7 = (hdata1$after == 0 & hdata1$dma_id == j & ((hdata1$week >= 30 & hdata1$week <= 53) | (hdata1$week >= 1 & hdata1$week <= 2)))
  mask8 = (hdata1$after == 1 & hdata1$dma_id == j & ((hdata1$week >= 30 & hdata1$week <= 53) | (hdata1$week >= 1 & hdata1$week <= 2)))
  p = sum(hdata1[mask7,]$sales)
  q = sum(hdata1[mask8,]$sales)
  dma_change1[j] = ((q - p)/p)*100
  if(newdata3[u,]$dma_id == j){
    u = u+1
    print(j)
    newdata3[newdata3$dma_id == j,]$before = p
    newdata3[newdata3$dma_id == j,]$after = q
    newdata3[newdata3$dma_id == j,]$affected = unique(hdata1[hdata1$dma_id == j,]$close)
    newdata3[newdata3$dma_id == j,]$sales_change = ((q-p)/p)*100
  }
}

head(newdata3)
mas3 = (newdata3$affected == 1)
avg_change1 = (sum(newdata3[mas3, ]$sales_change)/length(newdata3[mas3, ]$sales_change))
avg_change1

fit4 = lm(sales_change~ affected, data = newdata3)
summary(fit4)
