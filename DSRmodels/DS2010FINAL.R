library(MASS)
library(class)
library(tree)
data <- read.csv('Sleep_Efficiency.csv')

head(data)
num_missing <- colSums(is.na(data))
num_missing
data[is.na(data)] <- 0
num_missing <- colSums(is.na(data))
num_missing
data$Smoking.status <- ifelse(data$Smoking.status == "Yes", 1, 0)
data$Gender <- ifelse(data$Gender == "Male", 1, 0)
head(data)
data <- data[,-4]
data <- data[,-4]
data <- data[,-1]
data <- data[,-5]
data <- data[,-5]
data <- data[,-5]


trn = sample(nrow(data), nrow(data)*.7)
test = data[-trn, ]
train = data[trn, ]

tree.data = tree(Sleep.duration ~., data = train)
summary(tree.data)
plot(tree.data)
text(tree.data, pretty = 1)
pred.tree = predict(tree.data, test)
MSE = mean((test$Sleep.duration - pred.tree )^2)
MSE







