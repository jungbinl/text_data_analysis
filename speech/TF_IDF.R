install.packages("readr")

library(readr)
library(dplyr)
library(tidytext)

raw_speeches <- read_csv("speeches_presidents.csv")
raw_speeches

data1 <- readLines("president_speech.txt")
data2 <- readLines("president2_speech.txt")

data1 <- data1 %>% as_tibble() %>% mutate(president = "윤석열")
data2 <- data2 %>% as_tibble() %>% mutate(president = "이재명")

data1 <- data1 %>% group_by(president) %>% summarise(value = paste(value, collapse = " ")) %>% mutate(value = str_squish(value)) %>% as_tibble()

data2 <- data2 %>% group_by(president) %>% summarise(value = paste(value, collapse = " ")) %>% mutate(value = str_squish(value)) %>% as_tibble()

data <- bind_rows(raw_speeches, data1, data2)
data <- data %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))

data <- data %>% unnest_tokens(input = value, output = word, token = extractNoun)

frequency <- data %>%group_by(president) %>% count(word) %>% filter(str_count(word) > 1) 

frequency <- frequency %>% bind_tf_idf(term = word,document = president, n = n) %>% arrange(-tf_idf)
top10 <- frequency %>% group_by(president) %>% slice_max(tf_idf, n = 10, with_ties = F)
top10$president <- factor(top10$president, levels = c("윤석열", "이재명", "문재인", "박근혜", "이명박", "노무현"))

ggplot(top10, aes(x = reorder_within(word, tf_idf, president), y = tf_idf, fill = president)) + geom_col(show.legend = F) + coord_flip() + facet_wrap(~ president, scales = "free", ncol = 2) + scale_x_reordered() + labs(title = "TF_IDF", x = NULL, y = NULL) + theme(plot.title = element_text(hjust = 0.5))
