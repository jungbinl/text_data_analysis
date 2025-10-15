library(ggraph)
library(tidygraph)
library(readr)
library(dplyr)
library(textclean)
library(stringr)
library(tidyr)
library(widyr)

raw_data <- read.csv("news_comment_parasite.csv")

data <- raw_data %>% select(reply) %>% mutate(reply = str_replace_all(reply, "[^가-힣]", " "), reply = str_squish(reply), id = row_number())
data <- data %>% unnest_tokens(input = reply, output = word, token = SimplePos22, drop = F)

comment_pos <- data %>% separate_rows(word, sep = "[+]")

noun <- comment_pos %>% filter(str_detect(word, "/n")) %>% mutate(word= str_remove(word, "/.*$"))
noun %>% count(word, sort = T)
pvpa <- comment_pos %>% filter(str_detect(word, "/pv|/pa")) %>% mutate(word = str_replace_all(word, "/.*$", "다"))
pvpa %>% count(word, sort = T)
comment <- bind_rows(noun, pvpa) %>% filter(str_count(word) >= 2) %>% arrange(id)
comment <- comment %>% mutate(word = ifelse(str_detect(word, "감독") & !str_detect(word, "감독상"), "봉준호", word), word = ifelse(word == "오르다", "올리다", word), word = ifelse(str_detect(word, "축하"), "축하", word))

comment %>% count(word) %>% arrange(-n)

pair <- comment %>% pairwise_count(item = word, feature = id, sort = T)
sub_pair <- pair %>% filter(n >= 25) %>% as_tbl_graph() 

ggraph(sub_pair) + geom_edge_link() + geom_node_point() + geom_node_text(aes(label = name))

ggraph(sub_pair, layout = "fr") + geom_edge_link(color = "gray", alpha = 0.5) + geom_node_point(color = "red", size = 5) + geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + labs(title = "korea movie comment relationship", x = NULL, y = NULL) + theme_void()

set.seed(1234)
graph_comment <- pair %>% filter(n >= 25) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

ggraph(graph_comment, layout = "fr") + geom_edge_link(color = "gray", alpha = 0.5) + geom_node_point(aes(size = centrality, color = group), show.legend = F) + scale_size(range = c(5, 15)) + geom_node_text(aes(label = name), repel = T, size = 4, family = "a") + theme_graph()

graph_comment %>% arrange(centrality)

word_cors <- comment %>% add_count(word) %>% filter(n >= 20) %>% pairwise_cor(item = word, feature = id, sort = T)

word_cors %>% arrange(-correlation)

target <- c("대한민국","역사","수상소감", "조국", "박근혜", "블랙리스트")

top_cors <- word_cors %>% filter(item1 %in% target) %>% group_by(item1) %>% slice_max(correlation, n = 8)

ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1), y = correlation, fill = item1)) + geom_col(show.legend = F) + coord_flip() + facet_wrap(~ item1, scales = "free") + scale_x_reordered() + labs(title = "top_cors", x = NULL, y = NULL) + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

graph_cors <- word_cors %>% filter(correlation > 0.15) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

ggraph(graph_cors, layout = "fr") + geom_edge_link(color = "gray", aes(edge_alpha = correlation, edge_width = correlation), show.legend = F) + scale_edge_width(range = c(1, 4)) + geom_node_point(aes(size = centrality, color = group), show.legend = F) + scale_size(range = c(5, 15)) + geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + theme_graph()

new_comment <- comment_pos %>% separate_rows(word, sep = "[+]") %>% filter(str_detect(word, "/n|/pv|/pa")) %>% mutate(word = ifelse(str_detect(word, "/pv|/pa"), str_replace(word, "/.*$", "다"), str_remove(word, "/.*$"))) %>% filter(str_count(word) >= 2) %>% arrange(id)

new_comment <- new_comment %>% mutate(word = ifelse(str_detect(word, "감독") & !str_detect(word, "감독상"), "봉준호", word), word = ifelse(word == "오르다", "올리다", word), word = ifelse(str_detect(word, "축하"), "축하", word))

new_comment <- new_comment %>% group_by(id) %>% summarise(sentence = paste(word, collapse = " "))
bigram <- new_comment %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
bigram <- bigram %>% separate(bigram, c("word1", "word2"), sep = " ")
pair_bigram <- bigram %>% count(word1, word2, sort = T) %>% na.omit()
pair_bigram <- pair_bigram %>% filter(n >= 8) %>% as_tbl_graph()
pair_bigram

set.seed(1234)
ggraph(pair_bigram, layout = "fr") + geom_edge_link(color = "gray", aes(edge_alpha = n, edge_width = n)) + geom_node_point(color = "red", size = 5) + geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + theme_graph()
