############################
### 0. All Abbreviations ###
############################
library(rvest)
library(here)

files <-  list.files(here("data/abbreviations"), full.names = T)
Univ_Abb <- unique(do.call(c, lapply(files, function(x){
ws <- read_html(x)
Abb <- ws %>%  html_nodes("td") %>%  html_text()
Abb <- grep("^[A-Z ]+$", Abb, value = TRUE)
Abb <- tolower(Abb)
return(Abb)
})))
write.csv(Univ_Abb, here("data/abbreviations/abb.csv"))
