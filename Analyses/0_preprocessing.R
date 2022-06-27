############################
##### 0. Preprocessing #####
############################

library(here)
library(pdftools)
library(tidyverse)
library(tidytext)
library(pluralize)
library(stringi)
library(tm)
library(textstem)
library(words)
library(qdapDictionaries)
library(lexicon)

## List files and keep them in a list
data16.files <- list.files(here("Data/2016/OCR"), full.names = TRUE)
data20.files <- list.files(here("Data/2020/Analyze"), pattern = "OCR_", full.names = TRUE)

files.16.20 <- list('data16' = data16.files,
                    'data20' = data20.files )

## Read files
text.16.20 <- lapply(files.16.20, function(y){

  if(all(grepl("2016", y, fixed = T))){

  map_df(y, ~ data_frame(txt = pdf_text(.x))[-1,] %>% ##Remove header for 2016
           mutate(filename = .x) %>%
           unnest_tokens(text, txt))
  }else{
    map_df(y, ~ data_frame(txt = pdf_text(.x)) %>%
             mutate(filename = .x) %>%
             unnest_tokens(text, txt))
  }
})


##Singularize

text.16.20 <- lapply(text.16.20, function(x){
  x$text <- singularize(x$text)
  return(x)
})

## Check that all the letters were correctly read

all(unlist(files.16.20) %in% unlist(lapply(text.16.20, `[`, 1)))

## Remove additional words and non-characters
wte <- read.csv(here("data/words_to_exclude.csv"))

lexicons <- c(lexicon::common_names,
  lexicon::freq_first_names$Name,
  lexicon::freq_last_names$Surname,
  lexicon::pos_preposition,
  lexicon::pos_interjections,
  lexicon::pos_df_pronouns$pronoun)

remove_reg <- "&amp;|&lt;|&gt;"

ToremoveWords <- c("ci", 'TRUE', "e.g", "i.e", 'http', 'aaa', 'lalac', 'na',
                 'aaastudies.org', 'www.change.org', 'docsgooglecom', wte[,1], lexicons)


text.16.20.c <- lapply(text.16.20, function(y){
  y2 <-  y %>%
    mutate(text = str_remove_all(text, remove_reg)) %>%
    filter(!text %in% stop_words$word,
           !text %in% str_remove_all(stop_words$word, "'"),
           str_detect(text, "[a-z]"),
           !str_detect(text, "[0-9]")
    )

  #Remove non "latin1" or "ASCII" characters
  dat <- y2$text
  y2 <- y2[-grep("dat", iconv(dat, "latin1", "ASCII", sub="dat")),]

  ##Processing with tm
  y2$text <- removePunctuation(y2$text)
  y2$text <- stripWhitespace(y2$text)
  y2$text <- removeNumbers(y2$text)
  y2$text <- lemmatize_words(y2$text)

  ##Remove extra words
  y2 <- y2[! y2$text %in% ToremoveWords,]

  ##Remove non-words
  y2 <- y2[ y2$text %in% words::words$word,] ##Scrabble dictionary
  y2 <- y2[ y2$text %in% GradyAugmented,]

  ##Fix words...
  y2$text <- ifelse(y2$text == 'build', 'building', y2$text)

  return(y2)
})

names(text.16.20.c) <- names(files.16.20)

## Export pre-processed data

lapply(seq_along(text.16.20.c), function(x){
  write.csv(text.16.20.c[[x]],
            file = paste0(here("Data/preprocessed/"),
                          names(text.16.20.c)[x],
                          '.csv'))
})


## Save relevant objects

rm(list = ls()[!ls() %in% c("text.16.20.c")])
save.image(here("Data/preprocessed/data_ws.RData"))


