rm(list = ls(all = TRUE))

setwd("D:/rakesh/data science/Project/quora")

#load data into R
data = read.csv("train.csv", header = T , stringsAsFactors = F)[1:1000,]
# str(data)


library(stringr)
library(randomForest)
library(dplyr)
library(caret)
library(slam)
library(class)



#Text preprocessing
library(stringi)

preprocess = function(all_text){
        
        #Converting all words to lower case
        all_text = data.frame(tolower(as.matrix(all_text)) , stringsAsFactors = FALSE)
        
        
        all_text = data.frame(apply(all_text,2 , function(y)
                stri_trans_general(y , "latin-ascii")) , stringsAsFactors = FALSE)
        
        
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("'ve", " have ", y)),stringsAsFactors = FALSE)
        
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("'s", " is ", y)),stringsAsFactors = FALSE)
        
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("can't", " cannot ", y)),stringsAsFactors = FALSE)
                        
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("hadn't", " had not ", y)),stringsAsFactors = FALSE)
                
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("i'm", " i am ", y)),stringsAsFactors = FALSE)
                
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("'re", " are ", y)),stringsAsFactors = FALSE)
                
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("'d", " would ", y)),stringsAsFactors = FALSE)
                
        all_text = data.frame(apply(all_text,2 , function(y)
                gsub("'ll", " will ", y)), stringsAsFactors = FALSE)
        
        
        #Removing punctuation marks
        all_text = data.frame( apply(all_text, 2, function(y)
                gsub("[[:punct:]]", " ", y, perl = T)))
        
        
         
        #Delete extra spaces
        all_text = data.frame(lapply(all_text, function(y) 
                gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)), stringsAsFactors = FALSE)
        
        
        
        return(all_text)
        
}



#Performing different methods on text
library(stringdist)

ques_matrix = function(all_text){
        
        m = matrix(NA, nrow = nrow(all_text), ncol = 11)
        
        #Length of Question 1
        m[,1] = sapply(all_text[,1], function(x) length(strsplit(x , " ")[[1]]))
        
        #Length of Question 2
        m[,2] = sapply(all_text[,2], function(x) length(strsplit(x , " ")[[1]]))
        
        #Differnece in lenght(positive)
        m[,3] = abs(m[,1]-m[,2])
        
        # Compute Distance with qgram method
        m[,4] = stringdist(all_text[,1],all_text[,2], method = "qgram")
        
        #measuring dissimilarity between strings
        m[,5] = stringdist(all_text[,1],all_text[,2] , method = "jw" , p=0)
        
        #Cosine Method
        m[,6] = stringdist(all_text[,1],all_text[,2] , method = "cosine")
        
        # Compute similarity scores between strings
        m[,7] = stringsim(all_text[,1],all_text[,2])
        
        #Longest Common Substring (LCS)
        m[,8] = stringdist(all_text[,1],all_text[,2] , method = "lcs" )
        
        # Full Damerau-Levenshtein distance.
        m[,9] = stringdist(all_text[,1],all_text[,2] , method = "dl" )
        
        # Jaccard Distance Method
        m[,10] = stringdist(all_text[,1],all_text[,2] , method = "jaccard")
        
        #Optimal String Alignment distance
        m[,11] = stringdist(all_text[,1],all_text[,2] , method = "osa" )
        
        colnames(m) = c("q1length","q2length","diff_length","dist",
                        "jw_meth","cosine_meth","simi","lcs","dl","jaccard","osa")
        
        return(m)
}



#Plot Functions
line = function(dat,X,Y,title,nameX,nameY){
        l =  ggplot(aes_string(x=X,y=Y), data = dat) +
                geom_line(aes(color = 'red' ), stat='summary',fun.y=median) +
                labs(x = nameX , y = nameY) + ggtitle(title)
        return(l)
}

line2 = function(dat,X,Y,title,nameX,nameY){
        l =  ggplot(aes_string(x=X,y=Y), data = dat) +
                geom_line(aes(color = is_duplicate ), stat='summary',fun.y=median) +
                labs(x = nameX , y = nameY) + ggtitle(title)
        return(l)
}



#Text Minning

text_process = preprocess(data[,4:5])
text_process$is_duplicate = data$is_duplicate

#Replacing missing values with NA
text_process = data.frame(apply(text_process, 2, function(x) gsub("^$|^ $", NA, x)))
sum(is.na(text_process))

#Store Values in text_process frame
# Missingtext_process = text_process.frame(varaibles = colnames(text_process), 
#                          MissingInfo = apply(text_process,2,function(x)sum(is.na(x))))

#Removing incomplete observations
text_process = text_process[complete.cases(text_process),]
str(text_process)


#Converting variables to their respective classes
text_process$question1 = as.character(text_process$question1)
text_process$question2 = as.character(text_process$question2)

t_matrix = ques_matrix(text_process[,1:2])

#Combining the performed methods to rest of the data
text_process = cbind(text_process,t_matrix)

# data = text_process
str(text_process)
summary(text_process)



#Checking for multicolinearity
format(cor(t_matrix),digits = 2)
library(usdm)
vif(as.data.frame(t_matrix))

vifcor(t_matrix[,1:11], th=0.95)



#Observations
{


length(unique(data$question2))
length(unique(data$question2))
table(data$is_duplicate)

summary(text_process[,4:5])

sum(data$is_duplicate==0)/nrow(data)*100
sum(data$is_duplicate==1)/nrow(data)*100

sum(text_process$q1length>=7 & text_process$q1length<=13)/nrow(text_process)*100
sum(text_process$q1length<7)/nrow(text_process)*100
sum(text_process$q1length>13)/nrow(text_process)*100

sum(text_process$q2length>=7 & text_process$q2length<=13)/nrow(text_process)*100
sum(text_process$q2length<7)/nrow(text_process)*100
sum(text_process$q2length>13)/nrow(text_process)*100


table(text_process$is_duplicate[which(text_process$q2length>20)])
table(text_process$is_duplicate[which(text_process$diff_length>1)])


sum(text_process$is_duplicate==0 & text_process$diff_length>10)/nrow(text_process)*100
sum(text_process$is_duplicate==1 & text_process$diff_length>10)/nrow(text_process)*100


}

#All Plots
{
        library(gridExtra)
        library(ggplot2)
        library(ggthemes)
        library(psych)
        multi.hist(text_process[,6:14], main = NA, dcol = c("blue", "red"),
                   dlty = c("solid", "solid"), bcol = "grey95")
        
        h1=qplot(q1length, data = text_process, geom = "histogram", binwidth = 1, main = "Distribution of Q1 Length")
        h2=qplot(q2length, data = text_process, geom = "histogram", binwidth = 1, main = "Distribution of Q2 Length")
        grid.arrange(h1,h2)
        
        #Q1 & Q2 length boxplot
        boxplot(text_process[,4:5],col = "green", xlab = "Q1 & Q2 length boxplot")
        
        # Q1 Length Boxplot
        qplot(is_duplicate,q1length, data = text_process, geom = "boxplot",
              main = "Boxplot of q1 length", xlab = "Duplicate", ylab = "Q1 Length")
        
        # Q2 Length Boxplot
        qplot(is_duplicate,q2length, data = text_process, geom = "boxplot",
              main = "Boxplot of q2 length", xlab = "Duplicate", ylab = "Q2 Length")
        
        
        
        
        d1=qplot(q1length, data = text_process, geom = "density", fill = is_duplicate)
        d2=qplot(q2length, data = text_process, geom = "density", fill = is_duplicate)
        grid.arrange(d1,d2)
        
        b1=qplot(q1length, data = text_process, geom = "bar", fill = is_duplicate)
        b2=qplot(q2length, data = text_process, geom = "bar", fill = is_duplicate)
        grid.arrange(b1,b2)
        
        qplot(diff_length, data = text_process, geom = "bar", fill = is_duplicate)
        
        
        
        
        l1=line(text_process,"q1length","dist","Q1 Length VS Distance","Q1 Length","Distance")
        l2=line(text_process,"q2length","dist","Q2 Length VS Distance","Q2 Length","Distance")
        grid.arrange(l1,l2)
        
        l3=line(text_process,"q1length","q2length","Q1 Length VS Q2 Length","Q1 Length","Q2 Length")
        l4=line2(text_process,"q1length","q2length","Q1 Length VS Q2 Length","Q1 Length","Q2 Length")
        grid.arrange(l3,l4)
        
        l5=line(text_process,"q1length","simi","Q1 Length VS Similarity","Q1 Length","Similarity")
        l6=line(text_process,"q2length","simi","Q2 Length VS Similarity","Q2 Length","Similarity")
        grid.arrange(l5,l6)
        
        
        l7=line(text_process,"diff_length","dist","Difference in Length VS Distance","Length Difference","Distance")
        l8=line2(text_process,"diff_length","dist","Difference in Length VS Distance","Length Difference","Distance")
        grid.arrange(l7,l8)
        
        
        l9=line(text_process,"diff_length","simi","Difference in Length VS String Similarity","Length Difference","String Similarity")
        l10=line2(text_process,"diff_length","simi","Difference in Length VS String Similarity","Length Difference","String Similarity")
        grid.arrange(l9,l10)
        
        
        rm(b1,b2,d1,d2,h1,h2,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
        
}

#Word Cloud
#Word Cloud Function

library(wordcloud)
library(wordcloud2)
library(tm)

wc = function(text)
{
        
        corpus = Corpus(VectorSource(text))
        
        #Remove Stopwords and our predefined words
        corpus = tm_map(corpus, removeWords, c('i','its','it','us','use','want',
                                               'added','used','using','will','yes','say',
                                               'can','take','one',stopwords('english')))
        
        #remove unnecesary spaces
        corpus = tm_map(corpus, stripWhitespace)
        
        
        library(RWeka)
        BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
        # ... other tokenizers
        tok <- BigramTokenizer(corpus)
        
        tdmgram <- TermDocumentMatrix(corpus, control = list(tokenize = tok))
        #... create wordcloud
        #Word cloud
        
        
        w = wordcloud(tok, max.words = 50, scale=c(6, 1), colors=brewer.pal(8, "Dark2"))
        
        return(w)
        
}

wc(text_process[,1])
wc(text_process[,2])



#divide the data into train and test
# to get same data in each time
set.seed(123) 


train = text_process[sample(nrow(text_process), 200000, replace = F), ]

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




#Random Forest Model
rf_model <- randomForest(factor(is_duplicate) ~ diff_length + dist + jw_meth +
                                 cosine_meth + simi + lcs +jaccard, data = train, 
                         importance = TRUE ,  ntree = 100)
importance(rf_model, type = 2)

## Predict using the test set
prediction <- predict(rf_model, test[,-3])
xtab = table(observed = test[,3], predicted = prediction)
confusionMatrix(xtab)
rf_model



# Plotting model error
plot(rf_model, ylim=c(0.2,0.5))
legend("top", colnames(rf_model$err.rate), 
       col=1:4, fill = 9:13, bty = "n", horiz = TRUE)

# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                           Importance = round(importance[ , 'MeanDecreaseGini'], 2))

# Create a rank variable based on importance
rankImportance = varImportance %>%
        mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
        geom_bar(stat='identity') + 
        geom_text(aes(x = Variables, y = 0.5, label = Rank),
                  hjust=0, vjust=0.55, size = 6, colour = 'yellow') +
        labs(x = 'Variables') + coord_flip() + theme_few()


#Analysis on predicted classes
test_model = test
test_model$predicted = prediction
test_model = test_model[which(test_model$is_duplicate==test_model$predicted),]


summary(test_model[which(test_model$is_duplicate==test_model$predicted),])

table(test_model$is_duplicate)

sum(test_model$is_duplicate==0 & test_model$diff_length>10)/nrow(test_model)*100

sum(test_model$is_duplicate==1 & test_model$diff_length>10)/nrow(test_model)*100

summary(test_model$diff_length)




#KNN Model
set.seed(123)

train = text_process[sample(nrow(text_process), 200000, replace = F), ]

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






# 
# # #Applying model on test data
untrained_test = read.csv("test.csv", header = T , stringsAsFactors = F)
# 

set.seed(123)
#Taking sample of untrained data
untrained_test = untrained_test[sample(nrow(untrained_test), 200000, replace = F), ]


untrained_test = preprocess(untrained_test[,2:3])

str(untrained_test)

#Replacing missing values with NA
untrained_test = data.frame(apply(untrained_test, 2, function(x) gsub("^$|^ $", NA, x)))
sum(is.na(untrained_test))

#Store Values in untrained_test frame
# Missinguntrained_test = data.frame(varaibles = colnames(untrained_test),
#                          MissingInfo = apply(untrained_test,2,function(x)sum(is.na(x))))


#Removing incomplete observations
untrained_test = untrained_test[complete.cases(untrained_test),]


#Converting variables to their respective classes
untrained_test$question1 = as.character(untrained_test$question1)
untrained_test$question2 = as.character(untrained_test$question2)
untrained_test$is_duplicate = ""
summary(untrained_test)
t_matrix = ques_matrix(untrained_test[,1:2])

#Combining the performed methods to rest of the data
untrained_test = cbind(untrained_test,t_matrix)
str(untrained_test)
summary(untrained_test)


#Checking for multicolinearity
format(cor(t_matrix),digits = 2)
library(usdm)
vif(as.data.frame(t_matrix))

vifcor(t_matrix[,1:11], th=0.95)



wc(untrained_test[,1])
wc(untrained_test[,2])


#Applying Random Forest Model
rf_prediction = predict(rf_model, untrained_test[,-3])
#Merging the predicted values with the test file
untrained_test$is_duplicate = rf_prediction
write.csv(untrained_test, file = 'rf_solution.csv',row.names = F)

#Applying KNN on untrained data
# train1 = train[,-c(1,2,4,5)]
# untrained_test = untrained_test[,-c(1,2,4,5)]
knn_pred = knn(train[,-c(1,2,4,5)],untrained_test[,-c(1,2,4,5)], train$is_duplicate, k = 1)

#Merging the predicted values with the test file
untrained_test$is_duplicate = knn_pred

write.csv(untrained_test, file = 'knn_solution.csv',row.names = F)

rm(importance,rankImportance,xtab,accuracy,Conf_matrix,fit,i,data)
save.image("untrained_test")
load("untrained_test")
 