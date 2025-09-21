

txt <- readLines("Orion.txt")

install.packages("stringr")
library(stringr)
library(udpipe)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

txt <- str_replace_all(txt, "\\W", " ")

x <- udpipe_annotate(ud_model, x = txt)
x <- as.data.frame(x)

nouns <- subset(x, upos == "NOUN")$token
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, 
                  word = Var1, 
                  freq = Freq)
pal <- brewer.pal(8,"Reds")
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 1,
          max.words = 200,
          random.order = F,
          rot.per = 0.5,
          scale = c(4,0.5),
          colors = pal)

