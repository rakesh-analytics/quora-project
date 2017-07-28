#Random Forest Model
library(randomForest)
library(dplyr)

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

