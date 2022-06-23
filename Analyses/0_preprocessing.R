library(here)
library(pdftools)
library(tidyverse)
library(tidytext)
library(tidytext)

## List files and keep them in a list
data16.files <- list.files(here("Data/2016"), full.names = TRUE)
data20.files <- list.files(here("Data/2020"), full.names = TRUE)

files.16.20 <- list('data16' = data16.files,
                    'data20' = data20.files )

## Read files
text.16.20 <- lapply(files.16.20, function(y){
  map_df(y, ~ data_frame(txt = pdf_text(.x)) %>%
           mutate(filename = .x) %>%
           unnest_tokens(word, txt))
})


## Check that all the letters were correctly read

all(unlist(files.16.20) %in% unlist(lapply(text.16.20, `[`, 1)))

##




