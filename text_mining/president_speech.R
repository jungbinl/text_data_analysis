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

text2 <- readLines("president_speech.txt")
text2 <- str_replace_all(text2, "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
text2 <- text2 %>% unnest_tokens(input = value, output = word, token = extractNoun)
text2 <- text2 %>% count(word, sort = T) %>% filter(n > 1)

ggplot(text2, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "a") +
  scale_radius(limits = c(3,NA), range = c(3, 15))
  scale_color_gradient(low = "red", high = "blue") +
  theme_minimal()
