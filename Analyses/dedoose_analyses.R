library(here)
library("readxl")
library(data.table)
library(splitstackshape)
library(ggplot2)
library(bbplot) #devtools::install_github('bbc/bbplot')

##Read files and pre-process

files.PAM <- list.files(here("Data/dedoose"), pattern = "xls", full.names = TRUE)
files.Des <- list.files(here("Data/dedoose"), pattern = "csv", full.names = TRUE)

ds <- read_excel(files.PAM)
des <- read.csv(files.Des)
colnames(ds)[1] <- "File"
colnames(des)[1] <- "File"

##Check that all the files are described
ds$File[!ds$File %in% des$Title]

## Fix the 1's

filesSaving <- ds$File
ds$File <- NULL
lev1 <- grep('^[A-Z]', colnames(ds)); lev1Names <- colnames(ds)[lev1]
lev2 <- which(! 1:ncol(ds) %in% c(lev3, lev1)); lev2Names <- colnames(ds)[lev2]
lev3 <- grep('^1_1_', colnames(ds)); lev3Names <- colnames(ds)[lev3]

for(i in seq_along(lev2)){
  Tnested <- lev3[which(lev3 > lev2[i] & lev3 < lev2[i+1])]
  if(length(Tnested)>0 ){
  newV <- ifelse(rowSums(ds[,Tnested])>0,1,0)
  newV <- ifelse(newV + ds[,lev2[i]]>0,1,0)
  ds[,lev2[i]] <- newV
  }
}

for(i in seq_along(lev1)){
  Tnested <- lev2[which(lev2 > lev1[i] & lev2 < lev1[i+1])]
  if(length(Tnested) > 0 ){
    newV <- ifelse(rowSums(ds[,Tnested])>0,1,0)
    newV <- ifelse(newV + ds[,lev1[i]]>0,1,0)
    ds[,lev1[i]] <- newV
  }
}


ds$File <- filesSaving

##Change the date for the target time-frames
des$Time.Frame <- ifelse(des$Time.Frame %in% c(2013, 2015, 2016), 2016, des$Time.Frame)

##Merge descriptions and presence absence

c.ds <- merge(ds, des, by = "File", all.x = TRUE)
c.ds <- c.ds[!duplicated(c.ds$File),]

table(c.ds$Time.Frame)
table(c.ds$`Admissions/Student Recruitment & Retention Efforts`)/nrow(c.ds)
table(c.ds$`Organizational Commitment to Change`)/nrow(c.ds)
table(c.ds$`Diversifying Faculty/Staff and Campus Leadership`)/nrow(c.ds)


## Complete dataset

ds_long <- melt(setDT(c.ds), id.vars = c(1, 94:106), variable.name = "Topic")
ds_long <- cSplit(ds_long, 'Topic', sep = "_", type.convert=FALSE)

repPrevious <- function(targetVector){
  for(x in seq_along(targetVector)){
    targetVector[x] <-
      if(!is.na(targetVector[x]) & targetVector[x] == 1){
        targetVector[x-1] } else{
          targetVector[x] }
  }
  return(targetVector)
}

ds_long$Topic_1 <- repPrevious(ds_long$Topic_1)
ds_long$Topic_2 <- repPrevious(ds_long$Topic_2)
ds_long$Topic_3 <- repPrevious(ds_long$Topic_3)
ds_long <- as.data.frame(ds_long)

## Get frequencies for major topics

library(dplyr)

freq.global <- data.frame(Freq =sapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long[ds_long$Topic_1 == x & is.na(ds_long$Topic_2),]
  100*length(which(tsds$value == 1))/nrow(c.ds)
}))

write.csv(freq.global, here("Data/dedoose/Results/1. freq.global.csv"))


freq.time <- lapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long[ds_long$Topic_1 == x & is.na(ds_long$Topic_2),]
  targettf <- unique(tsds$Time.Frame)
  tf <- sapply(targettf, function(y){
  tsds <- tsds[tsds$Time.Frame == y & is.na(tsds$Topic_2),]
  100*length(which(tsds$value == 1))/nrow(tsds)
})
  names(tf) <- targettf
  tf
  })

freq.time <- do.call(rbind, freq.time)
row.names(freq.time) <- row.names(freq.global)

write.csv(freq.time, here("Data/dedoose/Results/2. freq.time.csv"))

# (Some quick regression models...)

freq.time <- as.data.frame(freq.time)

summary(lm(freq.time$`1968` ~ freq.time$`2020`))
summary(lm(freq.time$`1968`~ freq.time$`2016`))
summary(lm(freq.time$`2020`~ freq.time$`2016`))

plot(freq.time$`1968` ~ freq.time$`2020`)
plot(freq.time$`1968`~ freq.time$`2016`)
plot(freq.time$`2020`~ freq.time$`2016`)


## Get the frequencies for nested topics

freq.global.topics <- lapply(unique(ds_long$Topic_1), function(x){
  #Get the ones that talk about the main topic (ONLY)
  tsdsRef <- ds_long[ds_long$Topic_1 == x & ds_long$value == 1 & is.na(ds_long$Topic_2),]
  #Get the ones that talk about the particular topics
  tsds <- ds_long[ds_long$Topic_1 == x & ds_long$value == 1,]
  tsds2 <- na.omit(unique(tsds$Topic_2))
  if(length(tsds2)>0){
 retnestedF <- unlist(lapply(tsds2, function(y){
    tsds3 <- tsds[tsds$Topic_2 == y & !is.na(tsds$Topic_2) & is.na(tsds$Topic_3), ]
    100 * length(which(tsds3$value == 1))/nrow(tsdsRef)
  }))
cbind(Topic = x, Subtopic = tsds2, data.frame(retnestedF))
}
})

freq.global.topics <- do.call(rbind,freq.global.topics)


write.csv(freq.global.topics, here("Data/dedoose/Results/3. freq.global.topics.csv"))


## Frequency plots
library('bbplot')
library("stringr")

bar_df <- freq.global %>%
  mutate("Topic" = rownames(freq.global)) %>%
  arrange(-Freq, .by_group = TRUE) %>%
  head(5)

bar_df$Topic <- factor(bar_df$Topic, levels = rev(bar_df$Topic))

gbars <- ggplot(bar_df, aes(y = Topic, x = Freq)) +
  geom_bar(stat="identity",
           position="identity",
           #fill="#1380A1",
           fill="tomato3") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Primary Anti-Racism Demands across\nHigh. Educ. institutions in the US",
       subtitle = "Letters refect major waves of activism between 1960's and 2020's") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20))+
  #scale_x_continuous(labels = scales::percent) +
  geom_label(aes(y = Topic, x = Freq+7, label = paste0(round(Freq, 0),"%")),
             hjust = 1,
             vjust = 0.5,
             colour = "black",
             fill = NA,
             label.size = NA,
             family="Helvetica",
             size = 6) +
  geom_vline(xintercept = 0, size = 1, colour="#333333") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pdf(here("Data/dedoose/Results/4. Barplot.pdf"), 12, 6)
print(gbars)
dev.off()


jpeg(here("Data/dedoose/Results/4. Barplot.jpeg"), 12, 6, units = "in", res = 300)
print(gbars)
dev.off()


## Frequency by timeframe (all three)

library(tidyr)
grouped.time.or <- freq.time %>%
  mutate("Topic" = rownames(freq.time)) %>%
  gather(key = Year, value=Freq, -Topic)

top3 <-  grouped.time.or  %>%
  group_by(Year)  %>%
  slice_max(order_by = Freq, n = 3)

grouped.time <- grouped.time.or[grouped.time.or$Topic %in% top3$Topic,]

grouped.time$alpha <- ifelse(paste0(grouped.time$Topic, grouped.time$Year) %in% paste0(top3$Topic, top3$Year),
       1, 0.5)

grouped.time$Year <- factor(grouped.time$Year)
grouped.time$Topic <- factor(grouped.time$Topic)

gbars.time <-  ggplot(grouped.time, aes(y = Topic, x = Freq, fill = Year)) +
  geom_bar(stat="identity",
           position="dodge",
           alpha = grouped.time$alpha) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Primary Anti-Racism Demands in the US over time",
       subtitle = "Sinthesys of major waves of activism in 1968, 2016, and 2020") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20))+
  #scale_x_continuous(labels = scales::percent) +
  geom_text(aes(y = (Topic), x = Freq+1, label = paste0(round(Freq, 0),"%")  ),
             hjust = 0,
             #vjust = 0.5,
             #colour = "black",
             #label.size = NA,
             #fill = NA,
             family="Helvetica",
             size = 6,
             position = position_dodge2(width=1)) +
  geom_vline(xintercept = 0, size = 1, colour="#333333") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


pdf(here("Data/dedoose/Results/5. Barplot.Years.pdf"), 13, 6)
print(gbars.time)
dev.off()


jpeg(here("Data/dedoose/Results/5. Barplot.Years.jpeg"), 13, 6, units = "in", res = 300)
print(gbars.time)
dev.off()


##Regional differences (full dataset)

library(usmap)
library(ggplot2)
library(usa)
library(sf)
library(maps)

sts <- as.data.frame(tigris::states() )
dfRegion <- sts[,c('REGION', 'DIVISION', 'STUSPS')] #Numeric
ts.areas <- usa::states[,c("abb", "division", "region")] #Letters

ds_long.regions <- base::merge(ds_long, ts.areas, by.x = "State_AB", by.y = "abb", all.x = TRUE)
ds_long.regions <- base::merge(ds_long.regions, dfRegion, by.x = "State_AB", by.y = "STUSPS", all.x = TRUE)

ds_long.regions$region <- as.character(ds_long.regions$region)
ds_long.regions$region <- ifelse(is.na(ds_long.regions$region) , "Canda", ds_long.regions$region)

Region.freqs <- lapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long.regions[ds_long.regions$Topic_1 == x & is.na(ds_long.regions$Topic_2),]
  targettf <- unique(tsds$region)
  tf <- sapply(targettf, function(y){
    tsds <- tsds[tsds$region == y & is.na(tsds$Topic_2),]
    100*length(which(tsds$value == 1))/nrow(tsds)
  })
  names(tf) <- targettf
  tf
})

Region.freqs <- do.call(rbind, Region.freqs)
row.names(Region.freqs) <- row.names(freq.global)

write.csv(Region.freqs, here("Data/dedoose/Results/6. Region.freqs.csv"))

save.image(here("Data/dedoose/WS_dedooseR.RData"))

