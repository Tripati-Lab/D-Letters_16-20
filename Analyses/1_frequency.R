############################
####### 1. Frequency ######
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

### Estimate frequency

text.16.20.c.b <- rbindlist(text.16.20.c, idcol = 'year')

text.16.20.c.f <- text.16.20.c.b %>% 
                  count(year, text, sort = TRUE) %>% 
                  left_join(text.16.20.c.b %>% 
                              count(year, name = "total")) %>%
                  mutate(freq = n/total)

## Top X% of words per year
threshold <- 0.1

sub.16 <- text.16.20.c.f[text.16.20.c.f$year == 'data16',]
sub.20 <- text.16.20.c.f[text.16.20.c.f$year == 'data20',]

q.16 <- quantile(unlist(sub.16$freq), 1 - (threshold/100))
q.20 <- quantile(unlist(sub.20$freq), 1 - (threshold/100))

sub.16 <- sub.16[sub.16$freq >= q.16]
sub.20 <- sub.20[sub.20$freq >= q.20]

top.x.year <- rbind.data.frame(sub.16, sub.20)

f.1 <- ggplot(data = top.x.year, aes(fill = year)) +
        geom_bar(aes(x = text, y = freq), stat = "identity") +
        facet_wrap(~year, ncol = 1, scales = 'free_y')

ggsave(filename = here("Figures/f.1"), plot = f.1)  

## Plot scatterplot frequencies

text.16.20.c.f.t <- text.16.20.c.f %>% 
  select(year, text, freq) %>% 
  pivot_wider(names_from = year, values_from = freq) %>%
  arrange(data16, data20)

f.2 <- ggplot(text.16.20.c.f.t, aes(data16, data20)) +
        geom_point(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
        geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        geom_abline(color = "red")

ggsave(filename = here("Figures/f.2"), plot = f.2)  


summary(lm(text.16.20.c.f.t$data20 ~ text.16.20.c.f.t$data16))




