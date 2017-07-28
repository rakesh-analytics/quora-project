#Plot Functions

        library(gridExtra)
        library(ggplot2)
        library(ggthemes)

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

#All Plots
{
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
