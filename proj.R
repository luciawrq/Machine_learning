library(doParallel)
registerDoParallel(cores=2)
library(caret)
data <- read.csv("pml-training.csv", na.strings=c("", "NA"), header = TRUE)
data <- data[, c(8:11, 37:49, 60:68,84:86,102,113:124,140, 151:160)]
sum(is.na(training))

inTrain <- createDataPartition(y=data$classe, p=0.75, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

set.seed(123)
ss <- sample(1:dim(training)[1], size = 5000, replace =FALSE)
ss2 <- sample(1:dim(training)[1], size = 500, replace =FALSE)
subtrain <- training[ss,]
subtest <- training[ss2,]

Fitrp <- train(training$classe~., method="rpart", data=trainPC)

preProc <- preProcess(subtrain[,-53], method="pca")
trainPC <- predict(preProc, subtrain[,-53])
testPC <- predict(preProc, testing[,-53])
confusionMatrix(testing$classe, predict(FitrfPC, testPC))

FitrfPC <- train(subtrain$classe ~., method="rf", data=trainPC, prox=TRUE, model="FALSE")

pred <- predict(Fit, newdata=subtest)
table(pred, testing$classe)
subtest$predRight <- pred==subtest$classe
sum(subtest$predRight)/500

saveRDS(Fitrf, "Fitrfsmall.Rds")
Fitbt <- "Fitbt.Rds"
Fitbt <- readRDS(Fitbt)