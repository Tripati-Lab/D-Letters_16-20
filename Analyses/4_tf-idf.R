############################
######## 4. tf-idf ########
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


tf_idf.16.20.c.b <- lapply(text.16.20.c, function(y){
  y %>% 
  count(filename, text, sort = TRUE) %>%
  bind_tf_idf(text, filename, n)
})

tf_idf.16.20.c.b <- rbindlist(tf_idf.16.20.c.b, idcol = 'year')


tf_idf <- tf_idf.16.20.c.b %>% 
  filter(!near(tf, 1)) %>%
  arrange(desc(tf_idf)) %>%
  group_by(year) %>%
  distinct(text, year, .keep_all = TRUE) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(text = factor(text, levels = rev(unique(text)))) %>%
  ggplot(aes(tf_idf, text, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 3, scales = "free") +
  labs(title = "Highest tf-idf words in antiracism letters",
       caption = "Comparison between 2016 and 2020 letters",
       x = "tf-idf", y = NULL)

ggsave(filename = here("Figures/tf_idf.pdf"), plot = tf_idf, device = 'pdf')


