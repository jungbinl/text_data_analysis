library(dplyr)
library(udpipe)
library(rJava)
library(memoise)
library(wordcloud)
library(stringr)

text <- read.csv("alldata_1_for_kaggle2.csv")

model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

text <- str_replace_all(text, "\\W", " ")

x <- udpipe_annotate(ud_model, x = text)
x <- as.data.frame(x)

nous <- subset(x, upos == "NOUN")$token
wordcount <- table(unlist(nous))
df_word <- as.data.frame(wordcount, stringAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
top20 <- df_word %>% arrange(desc(freq)) %>% head(20)

library(ggplot2)

order <- arrange(top20, freq)$word

ggplot(data = top20, aes(x = word, y = freq)) + ylim(0,200) + geom_col() + coord_flip() + scale_x_discrete(limit = order) + geom_text(aes(label = freq), hjust = 0.3)

pal <- brewer.pal(10, "Dark2")
set.seed(1234)

df_word

wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = 0.5,
          scale = c(4,0.3),
          colors = pal)



