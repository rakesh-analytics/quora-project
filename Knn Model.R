
#KNN Model
library(class)
set.seed(123)

train = text_process[sample(nrow(text_process), 200, replace = F), ]

test = text_process[!(1:nrow(text_process)) %in% as.numeric(row.names(train)), ]


#Apply KNN with different K values
#K = 1
pred = knn(train[,-c(1,2,4,5)], test[,-c(1,2,4,5)], train$is_duplicate, k = 1)
Conf_matrix = table(pred, test$is_duplicate)
Conf_matrix

accuracy = sum(diag(Conf_matrix))/nrow(test)
accuracy

#K = 3
pred = knn(train[,-c(1,2,4,5)], test[,-c(1,2,4,5)], train$is_duplicate, k = 3)
Conf_matrix = table(pred, test$is_duplicate)
Conf_matrix

accuracy = sum(diag(Conf_matrix))/nrow(test)
accuracy

#K = 5
pred = knn(train[,-c(1,2,4,5)], test[,-c(1,2,4,5)], train$is_duplicate, k = 5)
Conf_matrix = table(pred, test$is_duplicate)
Conf_matrix

accuracy = sum(diag(Conf_matrix))/nrow(test)
accuracy

rm(data,t_matrix,test,text,corpus)

