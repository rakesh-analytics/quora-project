#divide the data into train and test
# to get same data in each time
set.seed(123) 
library(caret)

train = text_process[sample(nrow(text_process), 200, replace = F), ]

test = text_process[!(1:nrow(text_process)) %in% as.numeric(row.names(train)), ]

#Decision tree using C50 for classification
#cannot used for regression
library(C50)
ruleModel = C5.0(is_duplicate ~ diff_length + dist + jw_meth +
                         cosine_meth + simi + lcs + jaccard, data = train)
summary(ruleModel)

#predict using test data
test_pred = predict(ruleModel, test[,-3], type = "class")

#Visualize the confusion matrix
xtab = table(observed = test[,3], predicted = test_pred)
confusionMatrix(xtab)

#write rules into disk
write(capture.output(summary(ruleModel)), "c50Rules.txt")

#Save model
save(ruleModel, file = "DT.rda")
load("DT.rda")


##Decision tree using rpart for classification
library(rpart)
fit = rpart(is_duplicate ~ diff_length + dist + jw_meth +
                    cosine_meth + simi + lcs + jaccard, data = train, method = "class")
pred = predict(fit, test[,-3], type = "class")
xtab = table(observed = test[,3], predicted = pred)
confusionMatrix(xtab)

