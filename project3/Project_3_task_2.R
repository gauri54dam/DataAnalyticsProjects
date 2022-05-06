library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)
library(readr)
library(igraph)

# By performing analysis on Elon Musks tweet's understood that his number of tweets got increased year by year
# He moved from marketing of his product Electric Cars (Tesla) to Mars Missions (Space X)
# Also his words contain the outside world problem in the end like pandemic, covid 19, china cars, taxes, 
# owners, neural nets, AI,etc


# Define a custom function to check if the attribute is date
# Reference
# https://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

stop_words_ <- stop_words$word
stop_words_ <- stop_words_[stop_words_ != "s"]
stop_word <- as.data.frame(stop_words_)

# Change the column name of the stop_word to words
colnames(stop_word)[1] <- "words"
colnames(stop_word)[1]

# Elon Musk words contains some of the letters which can be removed so these are considered as stop words
tweets_specific_stop_words = as.data.frame(c("t.co","https","http", "@", "1","2",
                                             "3","4","5","6","7", "8", "9","0","amp","pretty","yeah","it's",
                                             "you're"))
tweets_specific_stop_words_list <- unlink(tweets_specific_stop_words)
colnames(tweets_specific_stop_words)[1] <- "new words"
colnames(tweets_specific_stop_words)[1]

stop_words_list = data.frame(stop_word = c(stop_word[,"words"], tweets_specific_stop_words[,"new words"]))
stop_words_list

# Convert the stop words list to a vector
stop_words_list <- unlist(stop_words_list)





tweet_data_2017 <- read_csv("2017.csv")
tweet_data_2018 <- read_csv("2018.csv")
tweet_data_2019 <- read_csv("2019.csv")
tweet_data_2020 <- read_csv("2020.csv")
tweet_data_2021 <- read_csv("2021.csv")
tweet_data_2022 <- read_csv("2022.csv")



# find the type in which date is stored, find the format 
# typeof(tweet_data_2017$date[1])
# tweet_data_2017$date[1]
# is.Date(tweet_data_2017$date[1])


# Convert the tweets time stamp to date as hh:mm:ss is not necessary for analysis
# as.Date(tweet_data_2017$date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

tweet_data_2017$date <- as.Date(tweet_data_2017$date)
tweet_data_2018$date <- as.Date(tweet_data_2018$date)
tweet_data_2019$date <- as.Date(tweet_data_2019$date)
tweet_data_2020$date <- as.Date(tweet_data_2020$date)
tweet_data_2021$date <- as.Date(tweet_data_2021$date)
tweet_data_2022$date <- as.Date(tweet_data_2022$date)



# is.Date(tweet_data_2017$date[3])

tweet_data_2017 <- tweet_data_2017 %>% filter(date >= "2017-01-01" & date <= "2017-12-31")
tweet_data_2018 <- tweet_data_2018 %>% filter(date >= "2018-01-01" & date <= "2018-12-31")
tweet_data_2019 <- tweet_data_2019 %>% filter(date >= "2019-01-01" & date <= "2019-12-31")
tweet_data_2020 <- tweet_data_2020 %>% filter(date >= "2020-01-01" & date <= "2020-12-31")
tweet_data_2021 <- tweet_data_2021 %>% filter(date >= "2021-01-01" & date <= "2021-12-31")
tweet_data_2022 <- tweet_data_2022 %>% filter(date >= "2022-01-01" & date <= "2022-12-31")

# Compute word frequencies for each year. Exclude the stop words

# tweet corresponds to the column in the dataset, get all the words from the tweets and remove the stop words
tweet_data_2017_words <- tweet_data_2017 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2017_words <- tweet_data_2017_words %>% filter(!words_in_doc %in% stop_words_list)

tweet_data_2018_words <- tweet_data_2018 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2018_words <- tweet_data_2018_words %>% filter(!words_in_doc %in% stop_words_list)

tweet_data_2019_words <- tweet_data_2019 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2019_words <- tweet_data_2019_words %>% filter(!words_in_doc %in% stop_words_list)

tweet_data_2020_words <- tweet_data_2020 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2020_words <- tweet_data_2020_words %>% filter(!words_in_doc %in% stop_words_list)

tweet_data_2021_words <- tweet_data_2021 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2021_words <- tweet_data_2021_words %>% filter(!words_in_doc %in% stop_words_list)

tweet_data_2022_words <- tweet_data_2022 %>%  unnest_tokens(words_in_doc, tweet) %>% count(words_in_doc, sort = T)
tweet_data_2022_words <- tweet_data_2022_words %>% filter(!words_in_doc %in% stop_words_list)

# Show top 10 words (for each year) by the highest value of word frequency

head(tweet_data_2017_words,10)
head(tweet_data_2018_words,10)
head(tweet_data_2019_words,10)
head(tweet_data_2020_words,10)
head(tweet_data_2021_words,10)
head(tweet_data_2022_words,10)

# Plot histogram of word frequencies for each year

total_words_2017 <- tweet_data_2017_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2017_words <- left_join(tweet_data_2017_words, total_words_2017[1:2])
# tweet_data_2017_words
ggplot(tweet_data_2017_words, aes(n/total))+ 
  ggtitle("2017 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F) 

total_words_2018 <- tweet_data_2018_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2018_words <- left_join(tweet_data_2018_words, total_words_2018[1:2])
# tweet_data_2018_words
ggplot(tweet_data_2018_words, aes(n/total))+ 
  ggtitle("2018 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F) 

total_words_2019 <- tweet_data_2019_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2019_words <- left_join(tweet_data_2019_words, total_words_2019[1:2])
# tweet_data_2019_words
ggplot(tweet_data_2019_words, aes(n/total))+ 
  ggtitle("2019 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F) 

total_words_2020 <- tweet_data_2020_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2020_words <- left_join(tweet_data_2020_words, total_words_2020[1:2])
# tweet_data_2020_words
ggplot(tweet_data_2020_words, aes(n/total))+ 
  ggtitle("2020 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F)

total_words_2021 <- tweet_data_2021_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2021_words <- left_join(tweet_data_2021_words, total_words_2021[1:2])
# tweet_data_2021_words
ggplot(tweet_data_2021_words, aes(n/total))+ 
  ggtitle("2021 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F) 

total_words_2022 <- tweet_data_2022_words %>% 
  group_by(words_in_doc) %>% 
  count(total = sum(n))
tweet_data_2022_words <- left_join(tweet_data_2022_words, total_words_2022[1:2])
# tweet_data_2022_words
ggplot(tweet_data_2022_words, aes(n/total))+ 
  ggtitle("2022 Words Frequency") + 
  geom_histogram(bins = 30,show.legend = F) 

# Use Zipf’s law and plot log-log plots of word frequencies and rank for each year

# Zipf's law

tweet_data_2017_words <- tweet_data_2017_words %>% mutate(`rank` = row_number())

tweet_freq_2017 <- tweet_data_2017_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2017 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2017 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2017 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2017 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.728, slope = -0.624, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2017 frequency")



tweet_data_2018_words <- tweet_data_2018_words %>% mutate(`rank` = row_number())

tweet_freq_2018 <- tweet_data_2018_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2018 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2018 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2018 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2018 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.6696, slope = -0.6635, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2018 frequency")


tweet_data_2019_words <- tweet_data_2019_words %>% mutate(`rank` = row_number())

tweet_freq_2019 <- tweet_data_2019_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2019 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2019 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2019 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2019 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.4594, slope = -0.7429, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2019 frequency")


tweet_data_2020_words <- tweet_data_2020_words %>% mutate(`rank` = row_number())

tweet_freq_2020 <- tweet_data_2020_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2020 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2020 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2020 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2020 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.4847, slope = -0.7335, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2020 frequency")



tweet_data_2021_words <- tweet_data_2021_words %>% mutate(`rank` = row_number())

tweet_freq_2021 <- tweet_data_2021_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2021 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2021 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2021 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2021 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.3907, slope = -0.7632, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2021 frequency")


tweet_data_2022_words <- tweet_data_2022_words %>% mutate(`rank` = row_number())

tweet_freq_2022 <- tweet_data_2022_words %>%
  group_by(words_in_doc) %>%
  mutate(`term frequency` = n/total) %>%
  ungroup()

tweet_freq_2022 %>% 
  ggplot(aes(rank, `term frequency`, color = "black")) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  ggtitle("2022 frequency") +
  scale_x_log10() + scale_y_log10()

rank_subset <- tweet_freq_2022 %>%
  filter(rank<1100, rank>50)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

tweet_freq_2022 %>% 
  ggplot(aes(rank, `term frequency`, color = "blue")) + 
  geom_abline(intercept = -1.595, slope = -0.633, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() + ggtitle("2022 frequency")



# Create bigram network graphs for each year

bigram_2017 <- tweet_data_2017 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2017 <- bigram_2017 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2017 <- bigram_2017 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2017 <- bigram_2017 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2017 <- bigram_2017 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2017, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")


bigram_2018 <- tweet_data_2018 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2018 <- bigram_2018 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2018 <- bigram_2018 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2018 <- bigram_2018 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2018 <- bigram_2018 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2018, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")



bigram_2019 <- tweet_data_2019 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2019 <- bigram_2019 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2019 <- bigram_2019 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2019 <- bigram_2019 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2019 <- bigram_2019 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2019, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")



bigram_2020 <- tweet_data_2020 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2020 <- bigram_2020 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2020 <- bigram_2020 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2020 <- bigram_2020 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2020 <- bigram_2020 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2020, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")


bigram_2021 <- tweet_data_2021 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2021 <- bigram_2021 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2021 <- bigram_2021 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2021 <- bigram_2021 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2021 <- bigram_2021 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2021, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")



bigram_2022 <- tweet_data_2022 %>%
  select(tweet) %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)

bigram_2022 <- bigram_2022 %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 != "NA" & word2 != "NA")

bigram_2022 <- bigram_2022 %>%
  filter(!word1 %in% stop_words_list) %>%
  filter(!word2 %in% stop_words_list)

bigram_2022 <- bigram_2022 %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2022 <- bigram_2022 %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2000)

ggraph(bigram_graph_2022, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "red")


# words_in_doc <-"it's"

# words_in_doc %in% tweets_specific_stop_words


# online references, These references were taken to get an idea on what R inbuild functions do

# 1. https://rpubs.com/siddharth2711/624628
# https://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r