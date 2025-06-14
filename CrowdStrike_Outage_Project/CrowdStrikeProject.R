#This project looks at the CrowdStrike outage event of 2024, 
#and the subsequent fallout in terms of public perception. 
#This data was collected through the use of NodeXL and 
#the following analysis was performed:
###Data cleaning and formatting
###Token frequency analysis
###TF-IDF
###Sentiment analysis
###Token analysis
###Exploration of bigrams 
###Creation of a topic model
###Creation of a Word Cloud

#Import necessary libraries
library(tidyverse)
library(tidytext)
library(textdata)
library(readxl)
library(writexl)
library(wordcloud)
library(wordcloud2)
library(igraph)
library(ggraph)
library(topicmodels)
library(tidyr)
library(widyr)
library(dplyr)

#Import data file
tweets <- read_excel("CrowdStrike_X_Data.xlsx", 
                     sheet = "Edges"
) %>%
  as_tibble() %>%
  mutate(Linenumber = row_number())

glimpse(tweets)
colnames(tweets)
#Remove URLs from Tweet column
tweets <- tweets %>%
  mutate(Tweet = gsub("http[s]?://\\S+|www\\.\\S+", "", Tweet))
tweets

#Break tweets into tidy text format - 1 token per line
tidy_tweets <- tweets %>%
  unnest_tokens(word, 
                Tweet,
                to_lower = TRUE)

#Remove stop words 
tidy_tweets <- tidy_tweets %>%
  anti_join(stop_words)
tidy_tweets
colnames(tidy_tweets)
####Token frequency
#choose only necessary columns
columnstokeep <- c("Vertex 1", "Vertex 2", "Linenumber", "word")
tf_tweets <- tidy_tweets[columnstokeep]

#Sort tokens by frequency
#Sort the tokens by frequency
tf_tweets %>%
  count(word, sort = TRUE) 

#Plot the frequency
#Plot the frequency of the tokens using ggplot2 using a filter
tf_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

####TF-IDF
#Tokenizing tweets and counting up words by Vertex 1 
tweet_words <- tweets %>%
  unnest_tokens(word, 
                Tweet,
                to_lower = TRUE) %>%
  mutate(word =  gsub("@\\w+", "", word))%>%
  count(`Vertex 1`, word, sort = TRUE)
tweet_words

#Count the total number of words by Vertex 1 
total_words <- tweet_words %>% 
  group_by(`Vertex 1`) %>% 
  summarize(total = sum(n))
total_words

#Join tweet_words and total_words on common column Vertext 1
tweet_words <- left_join(tweet_words, total_words)

#Calculating the term term frequency and rank of the term frequency
freq_by_rank <- tweet_words %>% 
  group_by(`Vertex 1`) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()
freq_by_rank

#Calculating the TF-IDF for each term by each Vertex1 
tweet_tf_idf <- tweet_words %>%
  bind_tf_idf(word, `Vertex 1`, n)

#Sort the results so that most important tokens are on top
tweet_tf_idf %>%
  arrange(desc(tf_idf))
tf_idf_20<-head(tweet_tf_idf,20)
write_xlsx(tf_idf_20, "tf-idf.xlsx")

###Pairwise analysis
#Break tweets into tidy text format, one token per line, remove any stop words
pair_tidy_tweets <- tweets %>%
  unnest_tokens(word, 
                Tweet,
                to_lower = TRUE) %>%
  filter(!word %in% stop_words$word)

#Select only the needed columns
columnstokeep <- c("Vertex 1", "Vertex 2", "Linenumber", "word", "Date")

#Remove the columns that we do not need
pair_tidy_tweets <- pair_tidy_tweets[columnstokeep]
pair_tidy_tweets

#Split the data into two seperate data frame based on the date of the event 7/19/24
#this is prior
pre_tidy_tweets <- pair_tidy_tweets %>%
  filter(Date < as.Date("2024-07-19"))
pre_tidy_tweets

#Split the data into two seperate data frame based on the date of the event 7/19/24
#this is after
post_tidy_tweets <- pair_tidy_tweets %>%
  filter(Date > as.Date("2024-07-19"))
post_tidy_tweets

#reduced the size of post data as it was causing R to crash
post_tidy_tweets_small<-head(post_tidy_tweets,25000)

#calculate the counts of word pairs within tweets for pre event
word_pair_count_pre <- pre_tidy_tweets %>%
  pairwise_count(word, Linenumber, sort = TRUE)
word_pair_count_pre
word_pair_count_pre_10<-head(word_pair_count_pre,10)
write_xlsx(word_pair_count_pre_10, "pairprecount10.xlsx")

#calculate the correlations between word pairs within tweets pre event
word_pair_cor_pre <- pre_tidy_tweets %>%
  pairwise_cor(word, Linenumber, sort = TRUE)
word_pair_cor_pre
word_pair_cor_pre_10<-head(word_pair_cor_pre,10)
write_xlsx(word_pair_cor_pre_10, "pairprecor10.xlsx")

#calculate the counts of word pairs within tweets for post event
word_pair_count_post <- post_tidy_tweets_small %>%
  pairwise_count(word, Linenumber, sort = TRUE)
word_pair_count_post
word_pair_count_post_10<-head(word_pair_count_post,10)
write_xlsx(word_pair_count_post_10, "pairpostcor10.xlsx")

#calculate the correlations between word pairs within tweets post event
word_pair_cor_post <- post_tidy_tweets_small %>%
  pairwise_cor(word, Linenumber, sort = TRUE)
word_pair_cor_post
word_pair_cor_post_10<-head(word_pair_cor_post,10)
write_xlsx(word_pair_cor_post_10, "pairpostcorr10.xlsx")

###Sentiment Analysis using AFINN
#preview the AFINN lexicon
get_sentiments("afinn")

#Count words by line number
word_count <- tidy_tweets %>%
  count(Linenumber)
word_count

#Sentiment analysis using afinn
tweets_score_sentiment_afinn <- tidy_tweets %>%
  inner_join(get_sentiments("afinn")) %>%
  inner_join(word_count) %>%
  rename (total_lines = n)
tweets_score_sentiment_afinn

tweets_score_sentiment_afinn <- tweets_score_sentiment_afinn %>%
  group_by(Date, total_lines) %>%
  summarise(Total_Sentiment = sum(value))

tweets_score_sentiment_afinn <- tweets_score_sentiment_afinn %>%
  mutate (Mean_Sentiment = Total_Sentiment/total_lines)

tweets_score_sentiment_afinn
write_xlsx(tweets_score_sentiment_afinn, "sentiment_afinn.xlsx")

#Graph afinn analysis
ggplot(tweets_score_sentiment_afinn, aes(Date, Mean_Sentiment)) +
  geom_col(show.legend = FALSE)

#Filter the date to produce a better graph
filtered_data <- tweets_score_sentiment_afinn %>%
  filter(Date >= as.Date("2024-07-01") & Date <= as.Date("2024-09-26"))

#Plot again
ggplot(filtered_data, aes(Date, Mean_Sentiment)) +
  geom_col(show.legend = FALSE)

#Plot using scatter option
#Use of weighted aesthetic and geometrically smoothed trend line
ggplot(filtered_data, aes(Date, Mean_Sentiment)) + 
  geom_point(shape = 1, alpha = 1/3) + 
  geom_smooth(method = "auto", linewidth = 1)


###FIND BIGRAMS
tweet_bigrams <- tweets %>%
  unnest_tokens(bigram, Tweet, token = "ngrams", n = 2, to_lower = TRUE) %>%
  filter(!is.na(bigram))
tweet_bigrams

#Count and sort bigrams
tweet_bigrams %>%
  count(bigram, sort = TRUE)

#Seperate, filter and unite bigrams in order to remove stop words
tweet_bigrams_separatedunited <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) 
tweet_bigrams_separatedunited

#Plot the the frequencies of bigrams, greater than 200
tweet_bigrams_separatedunited %>%
  filter(n > 200) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL)

#Network diagram using bigrams

#Need to generate bigram counts first
#Split the biagrams into 2 words
tweet_bigrams_separated <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#remove stop words
tweet_bigrams_filtered <- tweet_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Create new counts from filtered data frame
tweet_bigram_counts <- tweet_bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
tweet_bigram_counts

#Filter by 100 occurences
tweet_bigram_graph <- tweet_bigram_counts %>%
  filter(n > 100) %>%
  graph_from_data_frame()
tweet_bigram_graph

#Set random number
set.seed(2018)

#Plot graph of bigrams
tweet_bigram_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Define an arrow type
a <- grid::arrow(type = "closed", length = unit(.25, "inches"))

#Alter layout and weight of arrows in graph
tweet_bigram_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.08, 'inches')) +
  geom_node_point(color = "lightgreen", size = 8) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###TOPIC MODEL

#Stop words should already be removed so sort by frequency
tweet_word_counts <- tidy_tweets %>%
    count(Linenumber, word, sort = TRUE)
tweet_word_counts

#Create a document term matrix (dtm) for each tweet
tweet_dtm <- tweet_word_counts %>%
  cast_dtm(Linenumber, word, n)

#Set model to have 4 topics
tweet_lda <- LDA(tweet_dtm, k = 4, control = list(seed = 1234))
tweet_lda

#Capture the beta weights from the LDA
tweet_topics <- tidy(tweet_lda, matrix = "beta")
tweet_topics

#find the top 10 terms of each topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

#Plot top ten terms for each topic
top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


###WORD CLOUD


#Remove stop words, count the words, and rename column, and only keep top words  
word_counts <- tidy_tweets%>%
  anti_join(stop_words) %>%
  count(word) %>%
  slice_max(n, n = 200)
word_counts

#graph word cloud
wordcloud2(word_counts, color = "random-light", backgroundColor = "grey")

#Remove the word "crowdstrike" for comparison and graph again
custom_stop_words <- bind_rows(tibble(word = c("crowdstrike"),  
                                      lexicon = c("Custom")), 
                               stop_words)

word_counts_updated <- tidy_tweets%>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  slice_max(n, n = 200)
word_counts_updated

wordcloud2(word_counts_updated, color = "random-light", backgroundColor = "grey")
