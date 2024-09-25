library(caret)

X = read.csv(file="C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA2\\Q2_X.csv", header=TRUE)
Y = read.csv(file="C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA2\\Q2_Y.csv", header=FALSE, stringsAsFactors=TRUE)[,1]
X.valid = read.csv(file="C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA2\\Q2_Xvalid.csv", header=TRUE)
Y.valid = read.csv(file="C:\\Users\\zachb\\OneDrive\\Documents\\Machine Learning and Math Modelling\\Semester 2\\ST4061 - Statistical Methods for Machine Learning II\\CA2\\Q2_Yvalid.csv", header=FALSE, stringsAsFactors=TRUE)[,1]

#View the data
head(X)
dim(X)
head(Y)
length(Y)
#X has 8 predictors and 164 records
#There are six levels for response variable Y

length(Y.valid)
#Validation set has 50 entries

#(1)

set.seed(6041)	

#Set 5 fold CV for train function
trC = trainControl(method="cv", number=5)

#train knn models with 5 fold CV to find optimal k value
knn.o = train(X, Y, method='knn', trControl=trC)

#(a)
#Best model:
knn.o$bestTune
#K=5

#(b)

#Accuracy on training data
knn.o$results$Accuracy[1]
#0.6764037

knn.o$finalModel

#Predict validation data
knn.p <- predict(knn.o, X.valid) 

#Create table
(tb = table(Y.valid,knn.p))

#Find accuracy on test data
(acc = sum(diag(tb)) / sum(tb))
#0.66




#(2)

set.seed(6041)

subsets <- c(1:8)#Look at sizes 1 to 8

#Set rfeControl with CV and random forest
rfeC <- rfeControl(functions = rfFuncs, method = "cv",number = 5)

#Run rfe
rf.rfe <- rfe(X,Y,sizes = subsets,rfeControl = rfeC)

#View results
rf.rfe
#Plot accuracy for each number of variables
plot(rf.rfe, type=c("g", "o"), colour = "r",ylim=c(0,1))
#Clearly the model with 6 variables performs the best.


#(a)
#View predictors
rf.rfe$optVariables
#"Mg" "Al" "Ca" "Ba" "Na" "K" 

#(b)
#View CV accuracy
max_acc_idx <- which.max(rf.rfe$results$Accuracy)
rf.rfe$results$Accuracy[max_acc_idx]
#Accuracy = 0.7443998


#(3)
#Predict validation data
rf.p = predict(rf.rfe$fit, X.valid )
#Create table
(tb = table(Y.valid,rf.p))

#Find accuracy on test data
(acc = sum(diag(tb)) / sum(tb))
#Accuracy = 0.7


#(4)

set.seed(6041)	

#Set 5 fold CV for train function
trC.nnet = trainControl(method="cv", number=5)

#train neural network models with 5 fold CV to find optimal hidden layer size
nnet.o = train(X, Y, method='nnet', trControl=trC.nnet)

nnet.o

#(a)
#8 predictors used => 8 predictors in the input layer

#(b)
#Find best hidden layer size
nnet.o$bestTune$size
#Hidden layer size = 5


nnet.p = predict(nnet.o, X.valid )
#Create table
(tb = table(Y.valid,nnet.p))

#Find accuracy on test data
(acc = sum(diag(tb)) / sum(tb))
#Accuracy = 0.62

