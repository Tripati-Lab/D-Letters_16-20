############################
######## 5. Topic ########
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
library(topicmodels)

ggthemr("light")
ggthemr_reset()
## Load workspace

load(here("Data/preprocessed/data_ws.RData"))

text.16.20.c.b <- rbindlist(text.16.20.c, idcol = 'year')

text.16.20.c.counts <- text.16.20.c.b %>%
  count(filename, text, sort = TRUE) %>%
  ungroup()

dtm.16.20 <- text.16.20.c.counts %>%
  cast_dtm(filename, text, n)

lda.16.20 <- LDA(dtm.16.20, k = 24, control = list(seed = 1234))
lda.16.20.tidy <- tidy(lda.16.20)

lda.16.20.top <- lda.16.20.tidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top.lda <- lda.16.20.top %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

ggsave(filename = here("Figures/top.lda.pdf"), plot = top.lda, device = 'pdf')


lda.16.20.gamma <- tidy(lda.16.20, matrix = "gamma")

probtopics <- ggplot(lda.16.20.gamma, aes(gamma)) +
  geom_histogram(alpha = 0.8) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

ggsave(filename = here("Figures/probtopics.pdf"), plot = probtopics, device = 'pdf')


dist.prob.each <- ggplot(lda.16.20.gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

ggsave(filename = here("Figures/dist.prob.each.pdf"), plot = dist.prob.each, device = 'pdf')


lda_gamma <- full_join(lda.16.20.gamma, text.16.20.c.b, by = c("document" = "filename"))

lda_gamma <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  distinct(document, .keep_all= TRUE) %>% 
  count(document, topic, year, sort = TRUE)

top.lda.year <- lda_gamma %>%
    group_by(year) %>%
  ungroup %>%
  mutate(year2 = reorder_within(year, n, topic)) %>%
  ggplot(aes(x=n, y=factor(topic), fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top LDA topics per year",
       x = "Number of documents", y = "LDA topic") +
  scale_y_reordered() +
  facet_wrap(~ year, ncol = 4, scales = "free")

ggsave(filename = here("Figures/top.lda.year.pdf"), plot = top.lda.year, device = 'pdf')

