#Load and view data
x <- read.table("http://www.stat.cmu.edu/~cshalizi/350/hw/06/cadata.dat", header=TRUE)
summary(x)
require(tree) #very similar to library function
#treefit1
treefit = tree(log(MedianHouseValue) ~ Longitude+Latitude,data=x)
plot(treefit)
text(treefit,cex=0.75)
price.deciles = quantile(x$MedianHouseValue,0:10/10)
cut.prices = cut(x$MedianHouseValue,price.deciles,include.lowest=TRUE)
plot(x$Longitude,x$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude")
partition.tree(treefit,ordvars=c("Longitude","Latitude"),add=TRUE)
summary(treefit)
#treefit2
treefit2 <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=x, mindev=0.001)
plot(treefit2)
text(treefit, cex=.75)
plot(x$Longitude,x$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude")
partition.tree(treefit2,ordvars=c("Longitude","Latitude"),add=TRUE,cex=0.3)
summary(treefit2)
#treefit3
treefit3 <- tree(log(MedianHouseValue) ~., data=x)
plot(x$Longitude,x$Latitude,col=grey(10:2/11)[cut.prices],pch=20,
     xlab="Longitude",ylab="Latitude")
cut.predictions = cut(predict(treefit3),log(price.deciles),include.lowest=TRUE)
plot(x$Longitude,x$Latitude,col=grey(10:2/11)[cut.predictions],pch=20,
     xlab="Longitude",ylab="Latitude")
summary(treefit3)
