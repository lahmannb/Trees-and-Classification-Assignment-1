#Problem 2

wine = read.csv("http://www.nd.edu/~mclark19/learn/data/goodwine.csv")
summary(wine)
library(caret)
library(lattice)
library(ggplot2)
library(nnet)
library(randomForest)
library(kernlab)
#parallel processing
library(foreach)
library(doSNOW)
registerDoSNOW(makeCluster(3, type = "SOCK"))
#Correlation Matrix
library(corrplot)
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)
#Indices in Training Set
set.seed(1234) #so that the indices will be the same when re-run
trainIndices = createDataPartition(wine$good, p = 0.8, list = F)
wanted = !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality",
                                "color", "white")
wine_train = wine[trainIndices, wanted] #remove quality and color, as well as density and others
wine_test = wine[-trainIndices, wanted]
#initial Peek at wine trainplot and feature plot
wine_trainplot = predict(preProcess(wine_train[,-10], method="range"), wine_train[,-10])
featurePlot(wine_trainplot, wine_train$good, "box")
#set values of k
library(e1071)
set.seed(1234)
cv_opts = trainControl(method="cv", number=10)
knn_opts = data.frame(.k=c(seq(3, 11, 2), 25, 51, 101)) #odd to avoid ties
results_knn = train(good~., data=wine_train, method="knn",
                    preProcess="range", trControl=cv_opts,
                    tuneGrid = knn_opts)
results_knn
#testing the model with k=3
preds_knn = predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,10], positive='Good')

