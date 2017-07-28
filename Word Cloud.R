
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
        
        
 	#Word cloud
        w = wordcloud(tok, max.words = 50, scale=c(6, 1), colors=brewer.pal(8, "Dark2"))
        
        return(w)
        
}

wc(text_process[,1])
wc(text_process[,2])

