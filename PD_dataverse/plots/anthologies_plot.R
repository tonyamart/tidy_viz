library(tidyverse)
library(igraph)
library(ggraph)
library(ghibli)
library(paletteer)

setwd("../dataverse_files/")

#### preprocessing data ####
dat <- read.csv("anthologies_1853-1871.csv", sep = ";")
dat <- as_tibble(dat)
glimpse(dat)

# shortening titles
titles <- dat %>% 
  group_by(book_id, title, year) %>% 
  count() %>%
  ungroup() %>% 
  mutate(title_cln = str_remove_all(title, "[[:punct:]]")) %>% 
  mutate(title_cln = str_replace_all(
    title_cln, "^(\\w+)([[:space:]].*)", "\\1")) %>% 
  mutate(title_cln = paste0(year, "_", title_cln, "_", book_id)) %>%
  mutate(title_cln = ifelse(str_detect(title_cln, "_Для_"), 
                            str_replace_all(
                              title_cln, "_Для_", "_Для_легкого_чтения_"), 
                            title_cln))

titles$title_cln[15] <- "1857_Новый_поэтический_свет_15"
titles$title_cln[16] <- "1858_Литературная_eралаш_16"
titles$title_cln[19] <- "1859_Русская_поэзия_19"
titles$title_cln[22] <- "1860_Русская_лира_22"
titles$title_cln[30] <- "1863_Старые_знакомые_30"
titles$title_cln[36] <- "1868_Новые_писатели_36" 

glimpse(titles)
glimpse(dat)

# merge titles with main dataset
temp <- titles %>% select(book_id, title_cln)

data <- left_join(dat, temp, by = "book_id")

glimpse(data)

# cleaning text names & authors
data <- data %>% 
  mutate(text_cln = paste(author_std, text_std)) %>% 
  mutate(text_cln = str_remove_all(text_cln, "[[:punct:]]")) 
  
# filter only texts which appeared in at least 2 books
texts <- data %>% group_by(text_cln) %>% 
  count(sort = T) %>% 
  filter(n > 1) 

texts <- as.vector(texts$text_cln)

head(texts)
glimpse(data)

# filter books with frequent texts
mini <- data %>% 
  select(book_id, year, title_cln, pages, text_cln) %>% 
  filter(text_cln %in% texts) %>% 
  group_by(text_cln) %>% 
  count(title_cln) %>% 
  ungroup() 


# check
mini
mini %>% 
  group_by(title_cln) %>% 
  count(sort = T)

# detect pairs

x = NULL
y = NULL
z = NULL
#non_pair = NULL
for (i in 1:(nrow(mini) - 1)) {
    if (mini$text_cln[i] == mini$text_cln[i+1]) {
      x[i] = paste0(mini$title_cln[i], "--", mini$title_cln[i+1])
      y[i] = 1 
      z[i] = mini$text_cln[i]
    } 
    #else (non_pair[i] = paste0(mini$text_cln[i], "_", mini$text_cln[i+1]))
  }


#### graph data ####
result <- tibble(pair = x, 
       n = y, 
       text = z) %>% na.omit() %>% 
  group_by(pair) %>% 
  count(sort = F) %>% 
  ungroup() %>% 
  separate(pair, c("Book_1", "Book_2"), "--") %>% 
  arrange(factor(Book_1))

result

# raw graph (non-readable layout)
graph1 <- graph_from_data_frame(result, directed = F)

ggraph(graph1, layout = "linear") + geom_edge_arc(aes(width = n), alpha = 0.5)

communities <- walktrap.community(graph1)

# reorder nodes
nodes <- unique(c(unique(result$Book_1), unique(result$Book_2)))
t <- tibble(nodes = nodes,
            groups = communities$membership) %>% 
  arrange(groups) %>% 
  mutate(nodes = factor(nodes, nodes))

# size of nodes
values_nodes <- mini %>% 
  group_by(title_cln) %>% 
  count(sort = T) %>% 
  ungroup() %>% 
  rename(nodes = title_cln, 
         size = n)

t <- left_join(t, values_nodes, by = "nodes")

#c heck
result 
t

graph2 <- graph_from_data_frame(result, vertices = t, directed = F)

ggraph(graph2, layout = "linear") + 
  geom_edge_arc(edge_alpha = 0.95, 
                aes(width = n, colour = n)) + 
  scale_edge_colour_continuous(
    low = "#2D3740FF",
    high = "#742D33FF"
  ) +
  geom_node_point(aes(size = t$size, color=as.factor(t$groups)), 
                  alpha = 1.5) +
  geom_node_text(aes(label=t$nodes), 
                 angle = 65, hjust = 1, nudge_y = -0.3, size=3,
                 color = "darkslategrey",
                 fontface = "bold") + 
  # nodes colours
  scale_color_manual(values = c(rep(paletteer_d("ghibli::PonyoMedium"), 2))) +
  theme_void() + 
  labs(title = "Количество общих текстов в хрестоматиях",
       subtitle = "Ширина линии соответствует количеству текстов-пересечений",
       caption = "Данные: Анастасия Олещук\nВизуализация: Антонина Мартыненко") + 
  theme(
    legend.position = "none",
    plot.margin = unit(c(0,0,0.1,0), "null"),
    plot.title = element_text(hjust = 0.26, color = "#14454CFF", 
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.3, color = "#14454CFF"),
    plot.caption = element_text(hjust = 0.95, vjust = 0,
                                face = "italic", 
                                color = "#14454CFF"),
    plot.background = element_rect(fill = "white")
  ) + 
  expand_limits(x = c(-1,1),
                y = c(-3.5,2))

ggsave("plot4.png", plot = last_plot(), width = 8, height = 7, dpi = 300)


# paletteer_d("ghibli::PonyoDark")