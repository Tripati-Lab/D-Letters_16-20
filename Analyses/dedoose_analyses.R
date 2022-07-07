library(here)
library("readxl")
library(data.table)
library(splitstackshape)
library(ggplot2)

##Read files and pre-process

files.PAM <- list.files(here("Data/dedoose"), pattern = "xls", full.names = TRUE)
files.Des <- list.files(here("Data/dedoose"), pattern = "csv", full.names = TRUE)

ds <- read_excel(files.PAM)
des <- read.csv(files.Des)
colnames(ds)[1] <- "File"
colnames(des)[1] <-"File"

##Check that all the files are described
ds$File[!ds$File %in% des$Title]

##Change the date for the target time-frames
des$Time.Frame <- ifelse(des$Time.Frame %in% c(2013, 2015, 2016), 2016, des$Time.Frame)

##Merge descriptions and presence absence

c.ds <- merge(ds, des, by = "File", all.x = TRUE)
c.ds <- c.ds[!duplicated(c.ds$File),]

table(c.ds$Time.Frame)

## Get frequencies for major topics

ds_long <- melt(setDT(c.ds), id.vars = c(1, 94:104), variable.name = "Topic")
ds_long <- cSplit(ds_long, 'Topic', sep="_", type.convert=FALSE)

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

freq.global <- data.frame(Freq =sapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long[ds_long$Topic_1 == x,]
  tsds <- as.data.frame(table(tsds$File, tsds$value))
  100 * length(which(tsds[tsds$Var2 == 1,"Freq"] > 0))/nrow(c.ds)
}))

freq.global[order(freq.global$Freq),]

freq.time <- lapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long[ds_long$Topic_1 == x,]
  
  targettf <- unique(tsds$Time.Frame)
  tf <- sapply(targettf, function(y){
  tsds <- tsds[tsds$Time.Frame == y,]
  tsds <- as.data.frame(table(tsds$File, tsds$value))
  100 * length(which(tsds[tsds$Var2 == 1,"Freq"] > 0))/nrow(c.ds[c.ds$Time.Frame == y,])
})
  names(tf) <- targettf
  tf
  })

freq.time <- do.call(rbind, freq.time)
row.names(freq.time) <- row.names(freq.global)
freq.time <- as.data.frame(freq.time)

# (Some quick regression models...)

summary(lm(freq.time$`1986` ~ freq.time$`2020`))
summary(lm(freq.time$`1986`~ freq.time$`2016`))
summary(lm(freq.time$`2020`~ freq.time$`2016`))

plot(freq.time$`1986` ~ freq.time$`2020`)
plot(freq.time$`1986`~ freq.time$`2016`)
plot(freq.time$`2020`~ freq.time$`2016`)


## Get the frequencies for nested topics

freq.global.topics <- lapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long[ds_long$Topic_1 == x,]
  tsds2 <- na.omit(unique(tsds$Topic_2))
  if(length(tsds2)>0){
 retnestedF <- unlist(lapply(tsds2, function(y){
    tsds3 <- tsds[tsds$Topic_2 == y, ] 
    tsds3 <- tsds3[!is.na(tsds3$value),]
    tsds <- as.data.frame(table(tsds3$File, tsds3$value))
    100 * length(which(tsds[tsds$Var2 == 1,"Freq"] > 0))/nrow(tsds3)
  }))
cbind(Topic = x, Subtopic = tsds2, data.frame(retnestedF))
}
})

freq.global.topics <- do.call(rbind,freq.global.topics)

