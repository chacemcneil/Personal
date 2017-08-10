#load packages
library(randomForest)
library(e1071)
library(AUC)
library(MASS)
library(cvTools)
library(kknn)
library(glmnet)
require(ada)
require(caret)
require(boot)
require(class)

setwd("r:/users/cmcneil/Projects/Miscellaneous/StatTraining")

#load data
dat <- read.csv("train.csv")
test <- read.csv("test.csv") #Update as needed

#convert outcome variable to factor
dat$AorB <- as.factor(dat$AorB)

#shuffle data
set.seed(1854)
#dat <- dat[sample(1:length(dat[,1]), length(dat[,1])),]
dat <- dat[sample(1:nrow(dat), nrow(dat))]

## create model prediction functions (binary classifiers)

#random forest
m1 <- function(train, test, prob = T, thresh = 0.5) {
  
  model <- randomForest(AorB ~ ., data = train)
  test.pred <- predict(model, test, type = "prob")[, 2]
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))
  
}

#svm
m2 <- function(train, test, prob = T, thresh = 0.5) {
  
  model <- svm(AorB ~ ., data = train, probability = T)
  test.pred <- attributes(predict(model, test, probability = T))$probabilities[, "1"]
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))
  
}

#stepwise logistic regression
m3 <- function(train, test, prob = T, thresh = 0.5) {
  
  model <- stepAIC(glm(as.numeric(as.character(AorB)) ~ ., data = train, family = "binomial"), direction = "both", trace = 0)
  test.pred <- predict(model, newdata = test, type = "response")
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))
  
}


#boosted trees
m4 <- function(train, test, prob = T, thresh = 0.5) {
  
  model <- ada(AorB~., data=train)
  test.pred <- predict(model, test, type = "prob")[, which(levels(train$AorB) == "1")]
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))
  
}


#k-nearest neighbors (caret package)
m5 <- function(train, test, prob = T, thresh = 0.5) {
  
  levels(train$AorB) <- paste0("X", levels(train$AorB))
  levels(test$AorB) <- paste0("X", levels(test$AorB))
  
  train_control <- trainControl(method="repeatedcv", number=5, repeats=3)
  model <- train(AorB~., data=train, trControl=train_control, method="knn")
  test.pred <- predict.train(model, newdata = test, type = "prob")[, "X1"]
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))

}


#lasso regression
m6 <- function(train, test, prob = T, thresh = 0.5) {
  
  #train <- datTrain
  #test <- datTest
  
  cols <- colnames(train)
  cols <- cols[cols != "AorB"]
  
  test <- test[, cols]
  
  indx <- sapply(train, is.integer)
  train[indx] <- lapply(train[indx], function(x) as.numeric(as.character(x)))              #Rescale for LASSO
  train[,-1] <- scale(train[,-1])
  
  indx <- sapply(test[,-1], is.integer)
  test[indx] <- lapply(test[indx], function(x) as.numeric(as.character(x)))               #Rescale for LASSO
  test[,-1] <- scale(test[,-1])
  
  model <- cv.glmnet(data.matrix(train[, cols]), y=as.factor(train$AorB), alpha=1, family="binomial")              #Initial model
  lambda <- model$lambda.min                                                               #Retrieve optimal lambda
  
  model <- glmnet(data.matrix(train[, cols]), y=as.factor(train$AorB), alpha=1, family="binomial", lambda=lambda) #Remodel with optimal lambda
  test.pred <- predict(model, data.matrix(test[, cols]), type="response")
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))
  
}

# Linear Discriminate Analysis (LDA), (MASS package)
m7 <- function(train, test, prob = T, thresh = 0.5) {
  
  model <- lda(AorB~., data=train)
  test.pred <- predict(model, newdata = test, type = "prob")$posterior[, "1"]
  if(!prob)
    test.pred <- ifelse(test.pred > thresh, 1, 0)
  return(as.numeric(as.character(test.pred)))

}




#list of models
m <- list(RF = m1,
          SVM = m2,
          LogReg = m3,
          BT = m4,
          KNN = m5,
          LASSO = m6,
          LDA = m7)


#function to calculate 5-fold cross-validation error
calcError <- function(w, prob = T, thresh = 0.5) {
  
  dat <- as.data.frame(dat)
  dat <- data.frame(sapply(dat, function(x) as.numeric(as.character(x))))
  
  set.seed(1854)
  #dat <- dat[sample(1:length(dat[,1]), length(dat[,1])),]
  dat <- dat[sample(1:nrow(dat), nrow(dat)),]
  
  dat$AorB <- as.factor(dat$AorB)
  
  set.seed(1854)
  
  error <- c()
  
  for(i in 1:5) {
    
    if(i > 1) {
      
      datTrain <- dat[-((floor(.N / 5) + ceiling(.N / 5) * (i - 2) + 1):(floor(.N / 5) + ceiling(.N / 5) * (i - 1))),]
      datTest <- dat[(floor(.N / 5) + ceiling(.N / 5) * (i - 2) + 1):(floor(.N / 5) + ceiling(.N / 5) * (i - 1)),]
      
    } else {
      
      datTrain <- dat[-(1:floor(length(dat[,1]) / 5)),]
      datTest <- dat[1:floor(length(dat[,1]) / 5),]
      
    }
    
    pred <- (w[1] * m[[1]](datTrain, datTest, prob = prob) + 
               w[2] * m[[2]](datTrain, datTest, prob = prob) + 
               w[3] * m[[3]](datTrain, datTest, prob = prob) + 
               w[4] * m[[4]](datTrain, datTest, prob = prob) + 
               w[5] * m[[5]](datTrain, datTest, prob = prob) + 
               w[6] * m[[6]](datTrain, datTest, prob = prob) ) / sum(w)
    
    error[i] <- sum(ifelse(pred > thresh, 1, 0) != datTest$AorB) / length(datTest[,1])
    
  }
  
  cat(mean(error), "\n")
  
  return(sum(error) / length(error))
  
}

#function to optimize choice of ensemble
optimEnsemble <- function(mods, maxwt = 1, threshes = seq(.4, .6, .01)) {
  
  #mat <- expand.grid(0:maxwt, 0:maxwt, 0:maxwt, 0:maxwt, 0:maxwt, 0:maxwt)[-1,]
  mat <- do.call(expand.grid, rep(list(0:1), length(mods)))
  #mat <- mat[apply(mat, 1, sum) %% 2 != 0, ]
  #mat <- t(mat)
  
  err <- numeric()
  ensemb <- list()
  for(i in 1:nrow(mat)) {
    
    cat(paste(i, "of", nrow(mat), "ensembles. Error: "))
    
    err[i] <- calcError(unlist(mat[i,]))
  }
  
  ensemb <- unlist(mat[which.min(err),])
  
  err2 <- numeric()
  
  for(i in 1:length(threshes)) {
    
    cat(paste(i, "of", length(threshes), "thresholds. Error: "))
    
    err2[i] <- calcError(ensemb, thresh = threshes[i])
  }
  
  ensemb <- data.frame(mat, Error = err)
  threshes <- data.frame(Thresh = threshes, Error = err2)
  
  return(list(Ensemble = ensemb, Threshold = threshes))
  
}

#optimize choice
res <- optimEnsemble()

w <- unlist(res$Ensemble[which.min(res$Ensemble$Error), -7])

test <- as.data.frame(answers)
test <- data.frame(sapply(test, function(x) as.numeric(as.character(x))))
test$AorB <- factor(test$AorB, levels = 0:1)
train <- dat[, -11]

pred <- (w[1] * m[[1]](train, test, prob = prob, thresh = thresh) + 
           w[2] * m[[2]](train, test, prob = prob, thresh = thresh) + 
           w[3] * m[[3]](train, test, prob = prob, thresh = thresh) + 
           w[4] * m[[4]](train, test, prob = prob, thresh = thresh) + 
           w[5] * m[[5]](train, test, prob = prob, thresh = thresh) + 
           w[6] * m[[6]](train, test, prob = prob, thresh = thresh) ) / sum(w)

pROC::roc(test$AorB, pred, plot = T)
table(test$AorB, pred > 0.5)


summary(mod <- lm(Error~., data = res$Ensemble))
