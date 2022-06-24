############################
##### 3. Co-occurrence #####
############################

library(here)
library(tidyverse)
library(tidytext)
library(data.table)
library(scales)
library(ggthemr)
library(widyr)
library(igraph)
library(ggraph)

ggthemr("light")

## Load workspace

load(here("Data/preprocessed/data_ws.RData"))

pairs.16.20.c.b <- lapply(text.16.20.c, function(y){
    y %>% 
    pairwise_count(text, filename, sort = TRUE, upper = FALSE)
}) 


set.seed(1234)
p.16 <- pairs.16.20.c.b[[1]] %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/p.16.pdf"), plot = p.16, device = 'pdf')


p.20 <- pairs.16.20.c.b[[2]] %>%
  filter(n >= 80) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/p.20.pdf"), plot = p.20, device = 'pdf')


## Correlations between words

correlations.16.20 <- lapply(text.16.20.c, function(y){
    y %>% 
    group_by(text) %>%
    filter(n() >= 50) %>%
    pairwise_cor(text, filename, sort = TRUE, upper = FALSE)
}) 

set.seed(1234)
co.16 <- correlations.16.20[[1]] %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/co.16.pdf"), plot = co.16, device = 'pdf')


co.20 <- correlations.16.20[[2]] %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/co.20.pdf"), plot = co.20, device = 'pdf')

