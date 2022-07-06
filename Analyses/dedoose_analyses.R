library(here)
library("readxl")
library(data.table)
library(splitstackshape)
library(ggplot2)

##Read files and pre-process

files <- list.files(here("Data/dedoose"), full.names = TRUE)
ds <- read_excel(files)
names(ds)[1] <- "Feature"

ds_long <- melt(setDT(ds), id.vars = c("Feature"), variable.name = "Code")
ds_long <- cSplit(ds_long, 'Code', sep="_", type.convert=FALSE)

repPrevious <- function(targetVector){
  for(x in seq_along(targetVector)){
    targetVector[x] <-
      if(!is.na(targetVector[x]) & targetVector[x] == 1){
        targetVector[x-1] } else{
          targetVector[x] }
  }
  return(targetVector)
}


ds_long$Code_1 <- repPrevious(ds_long$Code_1)
ds_long$Code_2 <- repPrevious(ds_long$Code_2)
ds_long$Code_3 <- repPrevious(ds_long$Code_3)
ds_long <- as.data.frame(ds_long)

# Level number


ds_long$level <- sapply(1:nrow(ds_long), function(x){
  ts <- unlist(ds_long[x,grep("Code", names(ds_long))])
  length(na.omit(ts))
})

##Combine 2013, 2015, 2016

ds_long <- cSplit(ds_long, 'Feature', sep=":", type.convert=FALSE)
ds_long$Feature_3 <- factor(ifelse(ds_long$Feature_2 %in% c(2013, 2015, 2016), 2016, ds_long$Feature_2))

library(dplyr)
ds_long <- ds_long[,-"Feature_2"] %>%
  group_by(Code_1, Code_2 , Code_3 , level ,
           Feature_1 ,  Feature_3) %>%
  summarise_all(sum)


##Plot those in Lev 1 with >70%

sum_lev1 <- ds_long %>%
  group_by(Code_1,
           Feature_1,  Feature_3) %>%
  summarise(targetV = sum(value))%>%
  mutate(countT= sum(targetV)) %>%
  mutate(per=(100*targetV/countT))


## Plot the top 3 per year

sum_lev1 <- ds_long %>%
  group_by(Code_1,
           Feature_1,  Feature_3) %>%
  summarise(targetV = sum(value))

tf <- sum_lev1 %>%
  group_by(Feature_3) %>%
  filter(Feature_1 == "Time Frame") %>%
  mutate(countT= sum(targetV)) %>%
  mutate(per=(100*targetV/countT)) %>%
  arrange_(~ desc(targetV)) %>%
  group_by(Feature_3) %>%
  do(head(., n = 3))

ggplot(tf, aes(y = reorder(Code_1, per), x = per, fill = Feature_3)) +
  geom_bar(stat="identity", width = 0.7) +
  facet_wrap(~ Feature_3)


## Categories vs year vs subcategories

sum_lev2 <- ds_long %>%
  group_by(Code_1, Code_2,
           Feature_1,  Feature_3) %>%
  summarise(targetV = sum(value))

tf2 <- sum_lev2 %>%
  group_by(Feature_3, Code_2) %>%
  filter(Feature_1 == "Time Frame")

tf3 <- tf2 %>%
  group_by(Feature_3, Code_1) %>%
  mutate(countT = sum(targetV)) %>%
  mutate(per=(100*targetV/countT)) %>%
  arrange_(~ desc(targetV)) %>%
  do(head(., n = 3)) %>%
  na.omit()

## Select those that are in the top per year

tf3Sel <- tf3[paste0(tf3$Code_1,"_", tf3$Feature_3) %in% paste0(tf$Code_1,"_", tf$Feature_3),]

completeRF <- rbind(tf, tf3Sel)


library(ggplot2)
library(forcats)

ggplot(mydat, aes(y = reorder(GO_term, as.numeric(Type)), x = Number, size = Number)) + geom_point(aes(color = Class), alpha = 1.0) +
  geom_tile(aes(width = Inf, fill = Type), alpha = 0.4) +
  scale_fill_manual(values = c("green", "red", "blue"))


##Venn diagram



tf.expanded <- tf[rep(row.names(tf), tf$targetV), ]

library(venneuler)
library(grid)
v <- venneuler(data.frame(tf.expanded[,c("Feature_3", "Code_1")]))
par(cex = 0.7)
plot(v, main = "Modes of Senior Transportation (Toronto 2017 Survey)", cex.main = 1.5)
grid.text(
  "@littlemissdata",
  x = 0.52,
  y = 0.15,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

