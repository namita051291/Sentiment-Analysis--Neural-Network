## Loading the packages 

library(tidyverse)
install.packages("tidytext")
library(tidytext)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("text2vec")
library(text2vec)
install.packages("glmnet")
library(glmnet)

install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)

install.packages("qdap")
install.packages("tm")
library(qdap)
library(tm)

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"
setwd("C:/Users/nemab/OneDrive/Desktop/Projects/Python")
train <- read_tsv("train.tsv")    # Reading the train tab-separated file
test <- read_tsv("test.tsv")      # Reading the test tab-separated file

glimpse(train)
summary(train)


## Tokenisation

train <- train %>%
  rename(text = Phrase)

test <- test %>%
  rename(text = Phrase)

train %>%
  unnest_tokens(word, text) %>%
  head(10)


## Removing the Stop Words

data = train %>% select(text) %>% 
  filter(text != "nan") %>% 
  mutate(doc_id=1) %>% 
  select(doc_id,text) %>% 
  rename(text= text)
head(data,10)

corpus = Corpus(DataframeSource(data))
corpus=tm_map(corpus,removeWords,stopwords("en"))
corpus=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)

wordcloud(corpus,max.words=70,random.order = T,colors = rainbow(8), rot.per=0.5)
frequent_terms <- freq_terms(corpus, 10)

plot(frequent_terms)

# Stemming the phrase words 

stem_words <- stemDocument(c("charachter", "charachters"))
stem_words
stemCompletion(stem_words, c("charachter"))

stemCompletion(stem_words, corpus)

head(corpus)

# Stop words Array

movie_stopwords = c("movie","film","lrb","rrb","the", "NA", "ca","n't","movies", "director", "ca", "ve", "action", "people")

data %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% movie_stopwords) %>% 
  head(10)

# Plotting Top Ten most Common Words

createBarPlotCommonWords = function(data,title)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    filter(!word %in% movie_stopwords) %>% 
    count(word,sort = TRUE) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    head(10) %>%
    
    ggplot(aes(x = word,y = n)) +
    geom_bar(stat='identity',colour="white", fill =fillColor) +
    geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Word', y = 'Word Count', 
         title = title) +
    coord_flip() + 
    theme_bw()
  
}

createBarPlotCommonWords(train,'Top 10 Most Common Words')



#   Making WordCloud of the Common Words

createWordCloud = function(train)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    filter(!word %in% movie_stopwords) %>% 
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(train)


# #0 = Negative 

createWordCloud(train %>% filter(Sentiment == 0))

#1 = somewhat negative 

createWordCloud(train %>% filter(Sentiment == 1))


#2 = neutral 

createWordCloud(train %>% filter(Sentiment == 2))


#3 = somewhat positive 

createWordCloud(train %>% filter(Sentiment == 3))

#4 = positive 

createWordCloud(train %>% filter(Sentiment == 4))




# Creating plots for Most Common Bigrams
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

visualize_bigrams_individual <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

train %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigramWord, word1, word2, sep = " ") %>%
  group_by(bigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(bigramWord = reorder(bigramWord,n)) %>%
  head(10) %>%
  
  ggplot(aes(x = bigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Bigram', 
       y = 'Count', 
       title = 'Bigram and Count') +
  coord_flip() + 
  theme_bw()




# Plotting the Most Common Trigrams
train %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigramWord, word1, word2, word3,sep = " ") %>%
  group_by(trigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(trigramWord = reorder(trigramWord,n)) %>%
  head(10) %>%
  
  ggplot(aes(x = trigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = trigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Trigram', 
       y = 'Count', 
       title = 'Trigram and Count') +
  coord_flip() + 
  theme_bw()
#




#Relationship among words
trainWords <- train %>%
  count_bigrams()

trainWords %>%
  filter(n > 50) %>%
  visualize_bigrams()
#



# Trigram

train %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigramWord, word1, word2, word3,sep = " ") %>%
  group_by(trigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(trigramWord = reorder(trigramWord,n)) %>%
  head(10) %>%
  
  ggplot(aes(x = trigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = trigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Trigram', 
       y = 'Count', 
       title = 'Trigram and Count') +
  coord_flip() + 
  theme_bw()


