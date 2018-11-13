setwd("~/Desktop/School/PredAn/SemProject")
getwd()
Disney<-read.csv("DisneyDataE.csv", header = TRUE)
names(Disney)
cor(Disney)
dim(Disney)
getwd()
#liner model
smp_size<-floor(0.5*nrow(Disney))
smp_size
train_ind<-sample(seq_len(nrow(Disney)),size=smp_size)
training_set<-Disney[train_ind,]
#fix(train_set)
test_set<-Disney[-train_ind,]
#fix(test_set)
save(training_set,file="trainingset.rda")
save(test_set,file = "testset.rda")
load(file="trainingset.rda")
dim(training_set)
lm.fit=lm(Ticket~Year+Rides+Following, data=training_set)
summary(lm.fit)
#decision trees
library(MASS)
library(tree)
set.seed(1)
tree.disney=tree(Ticket~Year+Rides+Following, data=training_set)
plot(tree.disney)
text(tree.disney, pretty=0)
summary(tree.disney)
abline(0,1)
yhat=predict(tree.disney,newdata=training_set)
mean((yhat-training_set$Ticket)^2)
#bagging p.329
library(randomForest)
bag.disney=randomForest(Ticket~Year+Rides+Following, data=training_set, mtry=3)
bag.disney
yhat.bag = predict(bag.disney, newdata = training_set)
plot(yhat.bag)
mean((yhat-training_set$Ticket)^2)
rf.disney = randomForest(Ticket~Year+Rides+Following, data=training_set, mtry=3, importance = TRUE)
importance(rf.disney)
varImpPlot(rf.disney)
lmresults<-predict(lm.fit,test_set)
lmresults
sqrt(MSE<-sum((lmresults-test_set$Ticket)^2)/nrow(test_set))
nlmresults<-predict(bag.disney,test_set)
nlmresults
sqrt(MSE<-sum((nlmresults-test_set$Ticket)^2)/nrow(test_set))
