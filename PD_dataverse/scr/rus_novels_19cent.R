library(tidyverse)
library(tidytext)

library(scales) # scales transform from 4e+05 to 400,000
library(ghibli)
library(paletteer)
library(patchwork) # plots combination
library(cowplot) # ggdraw

theme_set(theme_minimal())

# Data from: https://dataverse.pushdom.ru/dataset.xhtml?persistentId=doi:10.31860/openlit-2020.10-C004 

#### data load & corpus creation ####
setwd("../novels_500/")

# unzip texts.zip
files <- list.files(path = "texts", full.names = T)

corpus <- tibble(textpath = files,
                    texts = sapply(files, read_file))

corpus <- corpus %>% 
  mutate(filename = str_remove_all(textpath, "texts/"))

str(corpus)

meta <- read_tsv("metadata.tsv")

glimpse(meta)

# check number of novels in each decade
meta %>% 
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade) %>% 
  count() %>% 
  ggplot(aes(x = decade, y = n)) + geom_col()

# store decades with few nowels
few_novels_decades <- c("1810", "1820", "1920", "1930")

# merge full dataset
full_dataset <- left_join(meta, corpus, by = "filename")

# check
str(full_dataset)

# count words in each novel
token_count <- full_dataset %>% 
  mutate(filename = paste0(filename, "___", year)) %>% 
  group_by(filename) %>% 
  unnest_tokens(input = texts, output = word, token = "words") %>% 
  count(word) %>% 
  summarise(n_words = sum(n))


# separate years & decades from filenames
tokens <- token_count %>% 
  separate(filename, into = c("filename", "year_test"), sep = "___") %>% 
  mutate(decade = floor(as.numeric(year_test)/10)*10)

glimpse(tokens)

# set decades as factor for boxplot
tokens$decade <- as.factor(tokens$decade)

# count number of novels for each author (author name separated by full stops)
n_novels <- tokens %>% 
  separate(filename, into = c("name", "title"), sep = "\\.") %>% 
  group_by(name) %>% 
  count(sort = T)

# separate authors in token count table
tokens_n <- tokens %>% 
  separate(filename, into = c("name", "title"), sep = "\\.")

# join
tokens_n <- left_join(tokens_n, n_novels, by = "name")

# filter authors who wrote more than 5 novels & delete the ones before 1830 and after 1900 (few data)
novel_means <- tokens_n %>% 
  filter(!decade %in% few_novels_decades) %>% 
  filter(n > 5) %>% 
  group_by(name) %>% 
  summarise(novel_length = mean(n_words)) %>% 
  arrange(desc(novel_length))

# check  
glimpse(novel_means)

# check ghibli colours
ghibli_palettes$MarnieLight1

# text plot for authors with longest mean length of a novel
names_plot <- novel_means %>% 
  mutate(name = paste0(name, " · ", round(novel_length))) %>% # create labels
  mutate(name = toupper(name)) %>% # upper case
  top_n(20) %>% 
  ggplot() + 
  geom_text(aes(x = reorder_within(name, novel_length, name), 
                y = 0, label = name),
            size = novel_means$novel_length[1:20]/17000, # label size as mean lenght
            hjust = "left",
            family = "sans",
            col = "#3D4F7DFF") +
  coord_flip() + 
  scale_x_reordered() + 
  scale_y_continuous(limits = c(-0.005, 0.05)) + 
  theme_void() + 
  labs(subtitle = "Although the longest novel in the dataset\nis (unsurprisingly) 'War and Peace'\nby Leo Tolstoy, on average his writings are\nnot that long!\n\nDostoyevski holds the prize here!\n\n\n            Mean number of tokens in novels \n            by Russian writers (1830—1900):\n") + 
  theme(plot.subtitle = element_text(colour = "#15110EFF",
                                     face = "bold"))

# the longest novel in the dataset
tolstoy <- tokens[which.max(tokens$n_words),]

# boxplot with absolute lengths of novels
box_plot <- tokens %>% 
  filter(!decade %in% few_novels_decades) %>% 
  ggplot(aes(x = decade, y = n_words, fill = decade)) + 
  geom_boxplot(width = 0.5, 
               outlier.colour = "#3D4F7DFF", 
               outlier.shape = 19,
               outlier.size = 3) + 
  geom_text(data = tolstoy, label = "'War and Peace'", # additional label for max value
            fontface = "bold",
            col = "#635143FF",
            hjust = -0.13) + 
  scale_fill_manual(values = c(rep(paletteer_d("ghibli::MarnieLight1"),10))) +
  scale_y_continuous(labels = comma) + # library(scales) non-scientific notation 4e+05
  theme(legend.position = "None") + 
  labs(x = "", y = "Number of tokens in a novel") +
  theme(axis.text = element_text(size = 14),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "#95918EFF"))


# library(cowplot) 
# add an image
bxpl <- ggdraw() + 
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/a/a3/Leo_Tolstoy%2C_portrait.jpg/800px-Leo_Tolstoy%2C_portrait.jpg", x = 0.4, y = 0.35, scale = 0.3) + 
  draw_plot(box_plot)

# library(patchwork)
# arrange plots according to a layout
layout <- "
AAAA##
AAAABB
AAAABB
"

patchwork <- bxpl + names_plot + plot_layout(design = layout)

patchwork + plot_annotation(
  title = "\nWho wrote the longest novels in Russian?",
  theme = theme(plot.title = element_text(colour = "#3D4F7DFF", size = 18, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0,
                                             vjust = -20)),
  caption = "data: OpenLit Dataverse @pushdomDV & @oleg_sobchuk\nviz: @tonyamartt")


