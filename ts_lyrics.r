library(dplyr)
library(plyr)
library(foreign)

#DATAFRAME
library(tibble)
library(tm)
library(tokenizers)
library(tidytext)

#PLOT
library(ggplot2)
library(scales)

#WORDCLOUD
library(wordcloud)
library(reshape2)

#PAIRWISE CORRELATION
library(widyr)
library(igraph)
library(ggraph)

#################
###IMPORT DATA###
#################

ts <-tibble(read.csv("taylor_swift_lyrics.csv")) #import data as tibble
ts<- mutate(ts, text= as.character(ts$lyric)) #change format of lyrics from factor into characters

###################
###CLEANING DATA###
###################

ts$text <-removePunctuation(ts$text, #remove punctuations
                  preserve_intra_word_contractions = TRUE, #keep contractions
                  preserve_intra_word_dashes = TRUE) #keep hyphenated words

#defining our stopwords
custom_stop_words <- bind_rows(
  tibble(
    word = c('ay',"la","ey","ah","ahh", "aah", "whoa", "ooh", "mmm","eh", "yeah", "gonna","ha", "wanna", "hey", "york"),
    lexicon = c("custom")
  ),
  tidytext::stop_words
)

ts <- ts %>% 
  tidytext::unnest_tokens(input = text, output= word, to_lower= TRUE)#separating each word

################################
###FREQUENCY DIAGRAM OF WORDS###
################################

#Creates frequency diagram of most common words in lyrics
ts %>%
  anti_join(custom_stop_words)%>%
  dplyr::count(word, sort = TRUE) %>% #counts the instances of each word
  filter(n > 40) %>% #presents words that appear more than 50 times
  mutate(word = reorder(word, n)) %>%
  #plotting
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

################################
###FREQUENCY DIAGRAM BY ALBUM###
################################

albumname <- as.character(unique(ts$album)) #extracts all the album names

#creates the dataframe for the frequency of each word within each album
frequency <- ts %>% 
  anti_join(custom_stop_words)%>%
  dplyr::count(album, word) %>% #counts the frequency of each word
  group_by(album) %>% #groups the count by album
  mutate(proportion = n / length(n)) %>% #calculates the proportion in relation to the album
  select(-n) %>%
  
  #organise data
  tidyr::spread(album, proportion)%>%
  tidyr::gather(album, proportion, all_of(albumname))

#plot data
ggplot(frequency, aes(x = album, y=proportion)) + #data input
  geom_point(alpha = 0.1, size = 2.5) + #data point
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1.2) #label

########################
###SENTIMENT ANALYSIS###
########################

#AFINN Method - Score between -5 to 5 for sentiment
afinn <- ts %>%
  anti_join(custom_stop_words)%>%
  inner_join(get_sentiments("afinn")) %>%
  dplyr::group_by(album) %>%  #group by album
  dplyr::summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

#Bing Method - binary categorisation into positive and negative
#NRC Method - categorises in binary between range of categories
bing_and_nrc <- bind_rows(
  ts %>%
    anti_join(custom_stop_words)%>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  ts %>%
    anti_join(custom_stop_words)%>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c(
                   "positive",
                   "negative"
                 ))) %>%
    mutate(method = "NRC")
) %>%
  dplyr::count(method, album, sentiment) %>%
  tidyr::spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#plot sentiments
bind_rows(
  afinn,
  bing_and_nrc
) %>%
  ggplot(aes(album, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#most common positive and negative words
bing_word_counts <- ts %>%
  anti_join(custom_stop_words)%>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

#plot
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>% #number of words in each category
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    y = "Contribution to sentiment",
    x = NULL
  ) +
  coord_flip()

###############
###WORDCLOUD###
###############

#comparison word cloud
ts %>%
  anti_join(custom_stop_words)%>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("gray20", "gray80"),
    max.words = 100
  )

################################
###SENTIMENT ANALYSIS BY SONG###
################################

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative") #filter by negative sentiments

wordcounts <- ts %>%
  anti_join(custom_stop_words)%>%
  group_by(album, track_title) %>%
  dplyr::summarize(words = n())

ts %>%
  semi_join(bingnegative) %>%
  group_by(album,track_title) %>%
  dplyr::summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("album", "track_title")) %>%
  mutate(ratio = negativewords / words) %>%
  top_n(1) %>%
  ungroup()

###########################################
###BIGRAMS CONTEXT TO SENTIMENT ANALYSIS###
###########################################

#seperate every word into pairs
ts_bigrams <- ts %>%
  tidytext::unnest_tokens(output= bigram, input = lyric, token = "ngrams", n = 2)

#split each pair of words into columns
bigrams_separated <- ts_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ")

#assign sentiment score to each word
AFINN <- get_sentiments("afinn")

#analyse words that are preceded by negation words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word2, value, sort = TRUE)

#plot
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by negation words") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

##########################
###PAIRWISE CORRELATION###
##########################

#pairwise correlation
word_cors <- ts %>%
  anti_join(custom_stop_words)%>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, track_title, sort = TRUE)

#plot to see the words associated with the filter items
word_cors %>%
  filter(item1 %in% c("love", "heart", "shake", "time")) %>% #change here
  group_by(item1) %>%
  top_n(10) %>% #top x number of associated words
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~item1, scales = "free") +
  coord_flip()

set.seed(2016)

#visualisation in cluster
word_cors %>%
  filter(correlation > .175) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
