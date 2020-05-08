#loading data
data<-read.csv("./data.csv", sep=";", dec = ",")


#basic statistics and structure of data
summary(data)
str(data)

#changing types 0-1 as factor, rest to num or int
data$admit<-as.factor(data$admit)
data$prestige<-as.factor(data$prestige)
data$gpa<-as.numeric(data$gpa)


#delete empty values
data<-na.omit(data)


#sorting data
data<-data[sample(1:nrow(data)), ]


#split to train and test data
sample <- sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

#checking proportions 
table(train$admit)
table(test$admit)


#check first 5 rows
head(train)
head(test)


#building initial model
LRegModel<-glm(admit~gre+gpa+prestige, data=train, family = 'binomial')
summary(LRegModel)


#if p > 0.05 delete unnecessary variables

LRegModelOptimized<-glm(admit~gre+prestige, data=train, family = 'binomial')
summary(LRegModelOptimized)


#predicting test cases
predicted<-predict(LRegModel, test, type = 'response')
head(predicted)

#if value is > than 0.5 = 1, wif not = 0
predicted_binomial<- ifelse(predicted>0.5, 1, 0)
head(predicted_binomial)

#confusion matrix =  TP, TN, FP, FN
matrix<-table(Predicted = predicted_binomial, Actual = test$admit)
matrix

#1- accuracy = % of bad predictions
1-sum(diag(matrix))/sum(matrix)


#comparing
actual<-matrix(test$admit, ncol=1)
predicted<-matrix(predicted_binomial, ncol=1)

compared<-cbind(actual, predicted)
compared

