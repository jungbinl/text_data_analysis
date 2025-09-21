remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))

download.file(url = "https://github.com/youngwoos/Doit_R/raw/master/Data/scala-library-2.11.8.jar", destfile = paste0(.libPaths()[1], "KoNLP/Java/scala-library-2.11.8.jar"))

library(showtext)
library(KoNLP)
useNIADic()

library(stringr)
library(dplyr)
library(ggplot2)
library(ggwordcloud)
library(tidytext)

text <- readLines("korea_song.txt")
text <- str_replace_all(text, "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
text <- text %>% unnest_tokens(input = value, output = word, token = extractNoun)
text <- text  %>% filter(str_count(word) > 1) %>% count(word, sort = T) %>% filter(n > 1)

ggplot(data = text, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + geom_text(aes(label = n), hjust = -0.3) + labs(title = "korea song word count", x = NULL, y = NULL) +theme(text = element_text(family = "a"), plot.title = element_text(size = 15, hjust = 0.5))
