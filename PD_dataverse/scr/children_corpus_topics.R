library(tidyverse)
library(tidytext)

library(ghibli)
library(paletteer)
theme_set(theme_minimal())

setwd("../detcorp/")

# get the dataset from: https://dataverse.pushdom.ru/dataset.xhtml?persistentId=doi:10.31860/openlit-2021.4-C001

# load data
mdt <- read.csv("~/Downloads/metadata.csv")
doc_topics <- read_table("doc-topics100.txt")
labels_topics <- read_table("labels100.txt", col_names = F)

# renaming
colnames(labels_topics) <- c("topic_no", "topic_id", "key_words")
colnames(doc_topics) <- c("doc_no", "doc_id", "fragment_id", "topic_id", "topic_prob")

#### genres overview

mdt %>% 
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade, genre) %>% 
  count() %>% 
  filter(n > 3) %>% 
  ggplot(aes(x = decade, y = n, fill = genre)) + geom_col() + 
  scale_fill_manual(values = rep(
    paletteer_d("ghibli::PonyoLight"), 3)) +
  labs(x = "Decade", 
       y = "Number of texts",
       fill = "Genre (manually assigned)")


glimpse(labels_topics)
glimpse(doc_topics)

# average topic prob. in a document (summarised from fragments)
doc_topics_avg <- doc_topics %>% 
  group_by(doc_id, topic_id) %>% 
  summarise(topic_avg_prob = mean(topic_prob))

# join table with key_words & topic probabilities
topics <- left_join(labels_topics, doc_topics_avg, by = "topic_id") %>% 
  select(doc_id, topic_id, topic_avg_prob, key_words)

glimpse(topics)
glimpse(mdt)

mdt_short <- mdt %>% select(id, year, genre)
colnames(mdt_short) <- c("doc_id", "year", "genre")

# full table with metadata & topic probs
topics <- left_join(mdt_short, topics, by = "doc_id")

glimpse(topics)

# distribution of genres tags in the 1920s
genres <- topics %>% 
  filter(year > 1919 & year < 1931) %>% 
  group_by(genre) %>% 
  count(sort = T)
top_genres <- as.vector(genres$genre[1:6])
top_genres <- c(top_genres, "skazka")

# last preprocessing
topics %>% 
  # keep topic_id with key words
  mutate(topic_id = paste0(topic_id, "__", key_words)) %>% 
  # delete most general & messy genre tag
  filter(genre %in% top_genres & genre != "realism") %>% 
  # only 1920s
  filter(year > 1919 & year < 1931) %>% 
  group_by(topic_id, genre) %>% 
  summarise(avg_dec = mean(topic_avg_prob)) %>% 
  ungroup() %>% 
  group_by(genre) %>% 
  top_n(7) %>% 
  
  ## plot
  
  ggplot(aes(x = reorder_within(topic_id, avg_dec, genre), 
             y = avg_dec, fill = genre)) + 
  geom_col() + 
  
  # grouping & ordering columns
  coord_flip() + 
  facet_wrap(~genre, scales = "free") +
  scale_x_reordered() + 
  
  # labs
  labs(y = "Average topic probability in a genre",
       x = "", 
       title = "Topics in the 1920s Russian children literary genres",
       subtitle = "7 most present topics on average; genres tagged manually", 
       caption = "data: dataverse.pushdom.ru -- Corpus of Russian Children's Literature \nviz: @tonyamartt") + 
  
  # colors & sizes
  theme(legend.position = "None") + 
  scale_fill_manual(values = paletteer_d("ghibli::PonyoMedium")) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 