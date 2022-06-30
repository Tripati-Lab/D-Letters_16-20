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
data60.files <- list.files(here("Data/1968/txt"), full.names = TRUE)
data16.files <- list.files(here("Data/2016/2_OCR"), full.names = TRUE)
data20.files <- list.files(here("Data/2020/4_txt"), full.names = TRUE)

files.16.20 <- list('data60' = data60.files,
                    'data16' = data16.files,
                    'data20' = data20.files )

n = 2 #Number of words per ngram

## Read files
text.16.20 <- lapply(seq_along(files.16.20), function(y){
  
  if (y == 1) {
  
   tx <- map_df(files.16.20[[y]], ~ data_frame(txt = readLines(.x)) %>%
             mutate(filename = .x) %>%
             unnest_tokens(text, txt, token = "ngrams", n = n))
    
    
  }
  
  if (y == 2) {
    
    tx <- map_df(files.16.20[[y]], ~ data_frame(txt = pdf_text(.x))[-1,] %>% ##Remove header for 2016
             mutate(filename = .x) %>%
             unnest_tokens(text, txt, token = "ngrams", n = n))
    
  }
  
  if (y == 3) {
    
    tx <- map_df(files.16.20[[y]], ~ data_frame(txt = readLines(.x)) %>%
             mutate(filename = .x) %>%
             unnest_tokens(text, txt, token = "ngrams", n = n))
    
  }
  
  return(tx)
  
})

## Check that all the letters were correctly read

all(unlist(files.16.20) %in% unlist(lapply(text.16.20, `[`, 1)))

## Remove additional words and non-characters
remove_reg <- "&amp;|&lt;|&gt;"
wte <- read.csv(here("data/words_to_exclude.csv"))
lexicons <- c(lexicon::common_names,
  lexicon::freq_first_names$Name,
  lexicon::freq_last_names$Surname,
  lexicon::pos_preposition,
  lexicon::pos_interjections,
  lexicon::pos_df_pronouns$pronoun)
ToremoveWords <- c("ci", 'TRUE', "e.g", "i.e", 'http', 'aaa', 'lalac', 'na',
                   'aaastudies.org', 'www.change.org', 'docsgooglecom', wte[,1], lexicons)

text.16.20.c <- lapply(text.16.20, function(y){
  
  if(n == 2){
  
  y2 <- y %>%  
    separate(text, into = c("word1", "word2"), sep = " ") %>%
    na.omit()
  
  y2 <- y2 %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !str_detect(word1, "[0-9]"),
           !str_detect(word2, "[0-9]"),
           !word1 %in% ToremoveWords,
           !word2 %in% ToremoveWords) %>%
    unite(text, c(word1, word2), sep = " ")
  }
  
  if(n == 3){
    
    y2 <- y %>%  
      separate(text, into = c("word1", "word2", "word3"), sep = " ") %>%
      na.omit()
    
    y2 <- y2 %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word,
             !word3 %in% stop_words$word,
             !str_detect(word1, "[0-9]"),
             !str_detect(word2, "[0-9]"),
             !str_detect(word3, "[0-9]"),
             !word1 %in% ToremoveWords,
             !word2 %in% ToremoveWords,
             !word3 %in% ToremoveWords) %>%
      unite(text, c(word1, word2, word3), sep = " ")
    
  }
  
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


