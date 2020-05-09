#1.collecting data
credit <- read.csv(file.choose(), header = TRUE)

#2.exploring data
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
summary(credit$default)

##Question 1##

#3. splitting the data
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


###Question 2##
#install.packages("randomForest")
#install.packages("caret")
library(randomForest)
library(caret)

#evaluating the model

set.seed(300)
rf <- randomForest(default ~ ., data = credit_train)
summary(rf)

credit_pred1 <- predict(rf, credit_test)
CrossTable(credit_test$default, credit_pred1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

##improve performance for random forest
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))
m_rf <- train(default ~ ., data = credit_train, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)

credit_pred2 <- predict(m_rf, credit_test)
CrossTable(credit_test$default, credit_pred2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))