#Loading the libraries

library(kernlab)
library(readr)
library(caret)
library(ggplot2)
library(dplyr)
library(gridExtra)

Data_MNIST_Train <- read.csv("mnist_train.csv",header=FALSE)
Data_MNIST_Test <- read.csv("mnist_test.csv",header=FALSE)

#Understanding Dimensions
dim(Data_MNIST_Train)
dim(Data_MNIST_Test)

#Structure of the dataset

str(Data_MNIST_Train)
str(Data_MNIST_Test)

#Exploring the dataset
summary(Data_MNIST_Train)
summary(Data_MNIST_Test)

#Checking for na values
sum(is.na(Data_MNIST_Train))
sum(is.na(Data_MNIST_Test))


Data_MNIST_Train$V1<-factor(Data_MNIST_Train$V1)

#Taking 6000 samples of training data

Data_Train <- sample_n(Data_MNIST_Train,6000,replace = F)

#Scaling of training and test datasets

Data_Train_scaled<- Data_Train
Test_data_scaled <- Data_MNIST_Test


Data_Train_scaled[,-1] <- Data_Train_scaled[,-1]/255
Test_data_scaled[,-1] <- Test_data_scaled[,-1]/255
############################################################################################
#Constructing Linear Model
Model_linear_scaled <- ksvm(V1~ ., data = Data_Train_scaled, scale = FALSE, kernel = "vanilladot")
Eval_linear_scaled<- predict(Model_linear_scaled, Test_data_scaled)

#Confusion Matrix:- Linear Kernel
confusionMatrix(Eval_linear_scaled,Test_data_scaled$V1)

#Accuracy:- 0.9033


#Constructing polydot kernel
Model_Poly_Scaled <- ksvm(V1~ ., data = Data_Train_scaled, scale = FALSE, kernel = "polydot")
Eval_Poly_Scaled<- predict(Model_Poly_Scaled, Test_data_scaled)

confusionMatrix(Eval_Poly_Scaled,Test_data_scaled$V1)
#Accuracy for Polydot:- 0.9033




#Constructing RBF Kernel
Model_RBF_Scaled <- ksvm(V1~ ., data = Data_Train_scaled, scale = FALSE, kernel = "rbfdot")
Eval_RBF_Scaled<- predict(Model_RBF_Scaled,Test_data_scaled )

#confusion matrix:- RBF Kernel(Scaled)
confusionMatrix(Eval_RBF_Scaled,Test_data_scaled$V1)

#Accuracy:- 0.9526
#############################################################################################

#traincontrol function to control the computational nuances of the train function.
trainControl <- trainControl(method="cv", number=5)

metric <- "Accuracy"

set.seed(7)


#Hyperparameter tuning and cross validation using Linear Kernel

# making a grid of C values. 
grid_Linear_scaled <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_Linear_scaled <- train(V1~., data=Data_Train_scaled, method="svmLinear", metric=metric, 
                        tuneGrid=grid_Linear_scaled, trControl=trainControl)

print(fit.svm_Linear_scaled)

#Plotting model results
plot(fit.svm_Linear)

#Accuracy:- 0.8955 at c=1

#Hyperparameter tuning and cross validation using Polydot 

grid_Poly_scaled <- expand.grid(.degree=c(2,3), .scale=c(0.1,0.3),.C=c(1,2,3,4) )

fit.svm_Poly_Scaled <- train(V1~., data=Data_Train_scaled, method="svmPoly", metric=metric, 
                      tuneGrid=grid_Poly_scaled, trControl=trainControl)

print(fit.svm_Poly_Scaled)
plot(fit.svm_Poly_Scaled) #Plotting model results

#Accuracy of 0.9479961 at degree=2,scale=0.1 and c=1


#Hyperparameter tuning using RBF Kernel
grid_RBF_scaled <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )
fit.svm_RBF_scaled <- train(V1~., data=Data_Train_scaled, method="svmRadial", metric=metric, 
                     tuneGrid=grid_RBF_scaled, trControl=trainControl)

print(fit.svm_RBF_scaled)
plot(fit.svm_RBF_scaled) #Plotting model results

#Accuracy:- 0.9581 at c=2 and sigma=0.025

#Since, the highest accuracy is obtained for RBF kernel, therefore we are considering RBF Kernel in our final model.
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_RBF_scaled, Test_data_scaled)
confusionMatrix(evaluate_non_linear, Test_data_scaled$V1)

#Accuracy :- 0.966

#End of Assignment#




