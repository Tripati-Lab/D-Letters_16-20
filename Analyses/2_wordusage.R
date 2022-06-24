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
  mutate(logratio = log(data20 / data16)) %>%
  arrange(desc(logratio))

ratios.16.20 %>% 
  arrange(abs(logratio))

wu1 <- ratios.16.20 %>%
          group_by(logratio < 0) %>%
          slice_max(abs(logratio), n = 15) %>% 
          ungroup() %>%
          mutate(text = reorder(text, logratio)) %>%
          ggplot(aes(text, logratio, fill = logratio < 0)) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          ylab("log odds ratio (2020/2016)") +
          scale_fill_discrete(name = "", labels = c("2020", "2016"))

ggsave(filename = here("Figures/wu.1.pdf"), plot = wu1, device = 'pdf')



