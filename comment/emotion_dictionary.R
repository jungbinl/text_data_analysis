install.packages("textclean")

library(dplyr)
library(readr)
library(textclean)
library(ggplot2)
library(tidyr)

data <- read.csv("knu_sentiment_lexicon.csv")

data %>% filter(polarity == 2) %>% arrange(word)

data %>% filter(polarity == -2) %>% arrange(word)

data %>% filter(!str_detect(word, "[가-힣]")) %>% arrange(word)

df <- tibble(sentence = c("오늘은 날씨도 좋고 기분이 상쾌하다. 하지만 일이 너무 많아서 스트레스를 받는다.","디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다"))

df <- df %>% unnest_tokens(input = sentence, output = word, token = "words", drop = F)
df <- df %>% left_join(data, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df <- df %>% group_by(sentence) %>% summarise(score = sum(polarity))

comment <- read.csv("news_comment_parasite.csv")
comment <- comment %>% mutate(id = row_number(), reply = str_squish(replace_html(reply)))
glimpse(comment)

word_comment <- comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F) %>% select(word, reply, id)
word_comment <- word_comment %>% left_join(data, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
table(word_comment$sentiment)
word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()

top10 <- word_comment %>% filter(sentiment != "neu") %>% count(sentiment, word) %>% group_by(sentiment) %>% slice_max(n, n = 10)
top10

ggplot(top10, aes(x = reorder(word, n), y = n, fill = sentiment)) + geom_col(show.legend = F) + coord_flip() + labs(title = "comment emotional score", x = NULL, y = NULL) + geom_text(aes(label = n), hjust = -0.3) + facet_wrap(~ sentiment, scales = "free") + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + scale_x_reordered() + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

score_comment <- word_comment %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select("reply", "score") %>% arrange(score)

score_comment <- score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
frequency_score <- score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n) * 100)

ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) + geom_col() + labs(title = "emotional ratio", x = NULL, y = NULL) + geom_text(aes(label = n), vjust = -0.3, ) + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = c("pos", "neu", "neg"))

ratio_score <- frequency_score %>% mutate(dummy = 0)

ggplot(ratio_score, aes(x = dummy, y = ratio, fill = sentiment)) + geom_col() + geom_text(aes(label = paste0(round(ratio, 1), "%")), position = position_stack(vjust = 0.5)) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

comment2 <- score_comment %>% unnest_tokens(input = reply, output = word, token = "words", drop = F) %>% filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)
comment2 <- comment2 %>% count(sentiment, word, sort = T)     

comment_wide <- comment2 %>% filter(sentiment != "neu") %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0))
comment_wide <- comment_wide %>% mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) / ((neg+1) / (sum(neg + 1)))))
top10 <- comment_wide %>% group_by(sentiment = ifelse(log_odds_ratio >0, "pos", "neg")) %>% slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
str(top10)

ggplot(top10, aes(x = reorder(word, log_odds_ratio), y = log_odds_ratio, fill = sentiment)) + geom_col() + coord_flip() + labs(title = "log odds ratio in comment", x = NULL, y = NULL)

new_data <- data %>% mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친") , 2, polarity))

new_data %>% filter(word %in% c("소름", "소름이", "미친"))
new_word_commnet <- word_comment %>% select(-polarity) %>% left_join(new_data, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
new_word_commnet <- new_word_commnet %>% group_by(id, reply) %>% summarise(score = sum(polarity)) %>% ungroup()
new_word_commnet <- new_word_commnet %>% select(reply, score) %>% arrange(-score)
new_word_commnet <- new_word_commnet %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))

new_word_comment <- new_word_commnet %>% unnest_tokens(input = reply, output = word, token = "words", drop = F) %>% filter(str_detect(word, "[가-힣]") & str_count(word) >= 2)
new_word_comment <- new_word_comment %>% count(sentiment , word, sort = T)
new_word_comment <- new_word_comment %>% filter(sentiment != "neu") %>% pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))
new_word_comment <- new_word_comment %>% mutate(log_odds_ratio = log(((pos +1) / (sum(pos+1))) / ((neg + 1)/(sum(neg + 1)))))
new_top10 <- new_word_comment %>% group_by(sentiment = ifelse(log_odds_ratio >0, "pos", "neg")) %>% slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

new_top10

ggplot(new_top10, aes(x = reorder(word, log_odds_ratio), y = log_odds_ratio, fill = sentiment)) + geom_col() + coord_flip() + labs(title = "log odds ratio with new data", x = NULL, y = NULL) + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))
new_top10

