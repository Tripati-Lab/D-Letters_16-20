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
sub.60 <- text.16.20.c.f[text.16.20.c.f$year == 'data60',] %>% top_n(10)
sub.16 <- text.16.20.c.f[text.16.20.c.f$year == 'data16',] %>% top_n(10)
sub.20 <- text.16.20.c.f[text.16.20.c.f$year == 'data20',] %>% top_n(10)

top.x.year <- rbind.data.frame(sub.60, sub.16, sub.20)

top.x.year$year <- factor(top.x.year$year, levels = c('data60', 'data16', 'data20'), labels = c("1968", "2016", "2020"))

f.1 <- ggplot(data = top.x.year, aes(fill = year)) +
        geom_bar(aes(x = text, y = freq), stat = "identity") +
        facet_wrap(~year, ncol = 1, scales = 'free_y') +
        theme(panel.grid.major = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.spacing = unit(.01, "lines"),
              panel.border = element_rect(color = "black", fill = NA, size = 0.5))

ggsave(filename = here("Figures/f.1.pdf"), plot = f.1, device = 'pdf')

## Plot scatterplot frequencies

text.16.20.c.f.t <- text.16.20.c.f %>%
  select(year, text, freq) %>%
  pivot_wider(names_from = year, values_from = freq)

colnames(text.16.20.c.f.t)[2:4] <- c("Y2020", "Y2016", "Y1968")

f.2 <- ggplot(text.16.20.c.f.t, aes(Y2016, Y2020)) +
        geom_point(alpha = 0.5, size = 2.5, color = 'black') +
        geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5, color = 'black') +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        geom_abline(color = "blue") +
        theme(panel.grid.major = element_blank(),
              axis.line = element_line(colour = "black"),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

ggsave(filename = here("Figures/f.2.pdf"), plot = f.2, device = 'pdf', height = 10, width = 10)

f.3 <- ggplot(text.16.20.c.f.t, aes(Y1968, Y2020)) +
  geom_point(alpha = 0.5, size = 2.5, color = 'black') +
  geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5, color = 'black') +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(filename = here("Figures/f.3.pdf"), plot = f.3, device = 'pdf', height = 10, width = 10)


f.4 <- ggplot(text.16.20.c.f.t, aes(Y1968, Y2016)) +
  geom_point(alpha = 0.5, size = 2.5, color = 'black') +
  geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5, color = 'black') +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "blue") +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(filename = here("Figures/f.4.pdf"), plot = f.4, device = 'pdf', height = 10, width = 10)


summary(lm(text.16.20.c.f.t$Y2020 ~ text.16.20.c.f.t$Y2016))
summary(lm(text.16.20.c.f.t$Y2020 ~ text.16.20.c.f.t$Y1968))
summary(lm(text.16.20.c.f.t$Y2016 ~ text.16.20.c.f.t$Y1968))


## Save relevant objects

rm(list = ls()[!ls() %in% c("text.16.20.c", "text.16.20.c.f", "text.16.20.c.f.t")])
save.image(here("Data/preprocessed/data_ws.RData"))



