############################
####### 2. Word Usage ######
############################

library(here)
library(tidyverse)
library(tidytext)
library(data.table)
library(scales)
library(ggthemr)

ggthemr("light")

## Load workspace

load(here("Data/preprocessed/data_ws.RData"))

## Words that more or less likely to be used each year (log odds ratio)

text.16.20.c.b <- rbindlist(text.16.20.c, idcol = 'year')

ratios.16.20 <- text.16.20.c.b %>%
  count(text, year) %>%
  group_by(text) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio20.16 = log(data20 / data16)) %>%
  mutate(logratio20.68 = log(data20 / data60)) %>%
  mutate(logratio16.68 = log(data16 / data60))


wu1 <- ratios.16.20 %>%
          group_by(logratio20.16 < 0) %>%
          slice_max(abs(logratio20.16), n = 15) %>% 
          ungroup() %>%
          mutate(text = reorder(text, logratio20.16)) %>%
          ggplot(aes(text, logratio20.16, fill = logratio20.16 < 0)) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          ylab("log odds ratio (2020/2016)") +
          scale_fill_discrete(name = "", labels = c("2020", "2016"))

ggsave(filename = here("Figures/wu.1.pdf"), plot = wu1, device = 'pdf')


wu2 <- ratios.16.20 %>%
  group_by(logratio20.68 < 0) %>%
  slice_max(abs(logratio20.68), n = 15) %>% 
  ungroup() %>%
  mutate(text = reorder(text, logratio20.68)) %>%
  ggplot(aes(text, logratio20.68, fill = logratio20.68 < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (2020/1968)") +
  scale_fill_discrete(name = "", labels = c("2020", "1968"))

ggsave(filename = here("Figures/wu.2.pdf"), plot = wu2, device = 'pdf')


wu3 <- ratios.16.20 %>%
  group_by(logratio16.68 < 0) %>%
  slice_max(abs(logratio16.68), n = 15) %>% 
  ungroup() %>%
  mutate(text = reorder(text, logratio16.68)) %>%
  ggplot(aes(text, logratio16.68, fill = logratio16.68 < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (2020/1968)") +
  scale_fill_discrete(name = "", labels = c("2016", "1968"))

ggsave(filename = here("Figures/wu.3.pdf"), plot = wu3, device = 'pdf')




