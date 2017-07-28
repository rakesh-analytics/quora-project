rm(list = ls(all = TRUE))

setwd("D:/rakesh/data science/Project/quora")

#load data into R
data = read.csv("train.csv", header = T , stringsAsFactors = F)[1:1000,]
# str(data)


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
