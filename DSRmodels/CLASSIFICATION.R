library(MASS)
library(class)

#10 a)
Weekly <- read.csv('Weekly.csv')

summary(Weekly)

Week <- lm(Volume ~ Year  ,data = Weekly)

plot(Weekly$Year, Weekly$Volume, ylab = "Shares Traded in billions", xlab = "Year") 
abline(Week , lwd = 3, col = "red")

cor(Weekly[ ,-9])

# e) LDA

trn = (Weekly$Year <= 2008)
tst = (Weekly$Year > 2008)


model.lda = lda(Direction ~ Lag2, ndata = trn)
model.lda
predict.lda = predict(model.lda, Weekly[!trn, ])
predict.lda
confusionmtrx = table(predict.lda$class, Weekly[!trn, ]$Direction)
confusionmtrx
mean(predict.lda$class == Weekly[!trn, ]$Direction)

# f) QDA

model.qda = qda(Direction ~ Lag2, data = Weekly, subset = trn)
model.qda
predict.qda = predict(model.qda, Weekly[!trn, ])
cmtrx = table(predict.qda$class, Weekly[!trn, ]$Direction )
cmtrx
mean(predict.qda$class == Weekly[!trn, ]$Direction)

# g) KNN

trn.x = data.frame(Weekly[trn, ]$Lag2)
tst.x = data.frame(Weekly[tst, ]$Lag2)
trn.dir = Weekly[trn, ]$Direction

set.seed(1)

knn.predict = knn(trn.x, tst.x, trn.dir, k = 1)
table(knn.predict, Weekly[tst, ]$Direction )
mean(knn.predict == Weekly[tst, ]$Direction )

knn.predict = knn(trn.x, tst.x, trn.dir, k = 3)
table(knn.predict, Weekly[tst, ]$Direction )
mean(knn.predict == Weekly[tst, ]$Direction )

knn.predict = knn(trn.x, tst.x, trn.dir, k = 5)
table(knn.predict, Weekly[tst, ]$Direction )
mean(knn.predict == Weekly[tst, ]$Direction )
