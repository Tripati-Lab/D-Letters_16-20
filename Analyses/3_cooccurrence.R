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

p.68 <- pairs.16.20.c.b[[1]] %>%
  #filter(quantile(n, 0.99999) < n) %>%
  top_n(50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/p.68.pdf"), plot = p.68, device = 'pdf')

p.16 <- pairs.16.20.c.b[[2]] %>%
  #filter(quantile(n, 0.99999) < n) %>%
  top_n(50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/p.16.pdf"), plot = p.16, device = 'pdf')


p.20 <- pairs.16.20.c.b[[3]] %>%
  #filter(quantile(n, 0.99999) < n) %>%
  top_n(50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/p.20.pdf"), plot = p.20, device = 'pdf')


## Correlations between words

correlations.16.20 <- lapply(seq_along(text.16.20.c), function(y){
  
  if ( y == 1) {
    text.16.20.c[[y]] %>%
      group_by(text) %>%
      filter(n() >= 10) %>%
      pairwise_cor(text, filename, sort = TRUE, upper = FALSE)
  }else{
    text.16.20.c[[y]] %>%
      group_by(text) %>%
      filter(n() >= 50) %>%
      pairwise_cor(text, filename, sort = TRUE, upper = FALSE)
  }
})

exclude <- c("mental", "health")

set.seed(1234)

co.68 <- correlations.16.20[[1]] %>%
  #filter(quantile(correlation, 0.99) < correlation) %>%
  filter(! item1 %in% exclude ) %>%
  filter(! item2 %in% exclude ) %>%
  top_n(50) %>%  #filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/co.68.pdf"), plot = co.68, device = 'pdf')


co.16 <- correlations.16.20[[2]] %>%
  #filter(quantile(correlation, 0.99) < correlation) %>%
  filter(! item1 %in% exclude ) %>%
  filter(! item2 %in% exclude ) %>%
  top_n(50) %>%
  #filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/co.16.pdf"), plot = co.16, device = 'pdf')


co.20 <- correlations.16.20[[3]] %>%
  #filter(quantile(correlation, 0.9995) < correlation) %>%
  filter(! item1 %in% exclude ) %>%
  filter(! item2 %in% exclude ) %>%
  #filter(correlation > .4) %>%
  top_n(50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

ggsave(filename = here("Figures/co.20.pdf"), plot = co.20, device = 'pdf')

