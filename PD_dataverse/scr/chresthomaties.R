library(tidyverse)
library(tidytext)
library(wesanderson)

theme_set(theme_minimal())

# data from the dataverse dataset "Хрестоматии Российской Империи с 1805 по 1912 гг."
chrstm <- read.delim("anthologies.tsv", sep = "\t")

glimpse(chrstm)

# add periodization column
chrstm <- chrstm %>% 
  mutate(period = ifelse(year < 1826, "1805-1825", "")) %>% 
  mutate(period = ifelse(year > 1825 & year < 1856, "1826-1855", period)) %>% 
  mutate(period = ifelse(year > 1855 & year < 1882, "1856-1881", period)) %>% 
  mutate(period = ifelse(year > 1881 & year < 1895, "1882-1894", period)) %>% 
  mutate(period = ifelse(year > 1894, "1895-1912", period))

# test period column
chrstm %>% 
  group_by(year, period) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n, fill = period)) + geom_col()

# filter top texts in three main genres
top <- chrstm %>% 
  filter(kind_of_literature != "undefined" & kind_of_literature != "критика") %>% 
  # join author & text title for labels
  mutate(title = paste0(author, " ", text))  %>% 
  group_by(period, kind_of_literature, title) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  group_by(period) %>% 
  slice(1:10)

# count n of observations in the data (for subtitle)
after1825 <- chrstm %>% 
  filter(period != "1805-1825")

length(unique(after1825$biblio_id))
length(unique(after1825$author))

# wrap lables
top$labels <- str_wrap(top$title, width = 30)

# wes anderson pallete
pal <- wes_palette("Cavalcanti1")

top %>% 
  # filter period with low number of observations
  filter(period != "1805-1825") %>% 
  ggplot(aes(x = reorder_within(labels, n, n), y = n, fill = kind_of_literature)) + 
  geom_col() + 
  scale_x_reordered() +
  coord_flip() + 
  facet_wrap(~period, scales = "free") + 
  scale_fill_manual(values = c(pal[2], pal[1], pal[4])) + 
  labs(y = "Number of appearances", 
       x = "",
       fill = "Genre", 
       title = "The most republished texts in Russian chrestomathies",
       subtitle = "Dataset includes 11151 entries from 132 chrestomathies and 323 authors (1826-1912)",
       caption = "Data: @pushdomDV\nviz: @tonyamartt") + 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 11, colour = "#06141FFF"),  
        axis.text = element_text(size = 9, color = "#06141FFF"),
        plot.title = element_text(size = 12, hjust = 0.5), 
        plot.subtitle = element_text(size = 11, color = "#3D4F7DFF", hjust = 0.5),
        legend.text = element_text(size = 10), 
        plot.caption = element_text(size = 10, color = "#3D4F7DFF"),
        plot.margin = unit(c(1,2,1,1), "cm")) 

ggsave(file = "chrestomaties.png", plot = last_plot(),
       dpi = 300, height = 8, width = 10,
       bg = "white")
