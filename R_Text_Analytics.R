rm(list=ls(all=TRUE)) #Clear all history
setwd("/Users/Prateek/Desktop/MyWork/DataScience")
txtdata <- read.csv("forums_text.csv", header=TRUE)

dim(txtdata)
attributes(txtdata)

typeof(txtdata)
is.numeric(txtdata$Message_Id)
#txtdata$date <- as.Date(txtdata$dt)

format(txtdata$date, format="%d %b %Y")
hist(txtdata$date, breaks=10)

#attach(txtdata)
library(sqldf)
sqldf("select Oper_System_Name, count(*) as count from txtdata group by Oper_System_Name")
comment <- sqldf("select count(*) as count, Body_Txt from txtdata group by Body_Txt order by count desc")
data_sort <- txtdata[!duplicated(txtdata$Body_Txt),]
data_txt <- data_sort[,2]
new_data_txt <- iconv(data_txt, to = "utf-8", sub="")

library(tm)
vs_txt <- VectorSource(new_data_txt, encoding = "UTF-8")
txt_corpus <- VCorpus(vs_txt)
tdm <- TermDocumentMatrix(txt_corpus,control = list(removePunctuation = TRUE, removeWords = 'NA',tolower=TRUE, removeNumbers = TRUE,stopwords("english"),stopwords = TRUE))
inspect(tdm)
findFreqTerms(tdm, 50)
findAssocs(tdm, "apple", 0.2)
tdmc = removeSparseTerms(tdm, .80) #Remove Sparse Terms and Keep Common Terms
dim(tdmc)
inspect(tdmc)
findFreqTerms(tdm, 10)
txt_matrix <- as.matrix(tdm)
row_sum <- sort(rowSums(txt_matrix),decreasing=TRUE)
txt_df <- data.frame(word = names(row_sum),freq=row_sum)
table(txt_df$freq) # Distribution of Frequency

library(wordcloud)
wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5))

wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5), min.freq=5, max.words=Inf, random.order=FALSE, rot.per=.05, colors="red")

require(RColorBrewer)

display.brewer.all() #see what it does?
brewer.pal.info #have a look at it

color_range <- brewer.pal(8,"Blues")
wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5), min.freq=20, max.words=Inf, random.order=FALSE, rot.per=.05, colors=color_range)

color_range <- brewer.pal(8,"Greens")
wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5), min.freq=15, max.words=Inf, random.order=FALSE, rot.per=.05, colors=color_range)

color_range <- brewer.pal(8,"Set1")
wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5), min.freq=5, max.words=Inf, random.order=FALSE, rot.per=.05, colors=color_range)

color_range <- brewer.pal(11,"Spectral")
wordcloud(txt_df$word,txt_df$freq, scale=c(2,.5), min.freq=20, max.words=Inf, random.order=FALSE, rot.per=.05, colors=color_range)







tdm
inspect(tdm)
#Sparsity = percentage of elements in the matrix that are equal to zero




#http://www.graphviz.org
#http://www.graphviz.org/Gallery.php
#http://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html



findFreqTerms(tdm,20) # Atleast 20 occurences
findFreqTerms(tdm, 15)
findFreqTerms(tdm, 80)

## S3 method for class 'TermDocumentMatrix'
#findAssocs(x, terms, corlimit) 
# terms - character vector holding terms
#corlimit - numeric vector of same length as terms

trms <- c("apple","iphone","ios")
corr_trms <- c(0.3,0.2,0.3)

findAssocs(tdm,trms, corr_trms)




#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

library(Rgraphviz)
#set.seed(65432)
attrs <- list(node = list(shape = "circle",fillcolor = "lightblue",fontsize = 30, fixedsize =FALSE))

g1 <-plot(tdm,terms=findFreqTerms(tdm,150),corThreshold=.1,weighting = TRUE,attrs = attrs)