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
library(tidyr)


text1 <- readLines("president_speech.txt")
text2 <- readLines("president2_speech.txt")

text1 <- text1 %>% as_tibble() %>% mutate(president = "yoon")
text2 <- text2 %>% as_tibble() %>% mutate(president = "lee")

bind_speeches <- bind_rows(text1, text2) %>% select(president, value)

speeches <- bind_speeches %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value)) 
speeches <- speeches %>% unnest_tokens(input = value, output = word, token = extractNoun)
speeches <- speeches %>% count(president, word) %>% filter(str_count(word) > 1)
top10 <- speeches %>% group_by(president) %>% slice_max(n, n = 10, with_ties = F)

ggplot(top10, aes(x = reorder_within(word, n, president), y = n, fill = president)) + geom_col() + geom_text(aes(label = n), hjust = -0.3) + coord_flip() + labs(title = "speech compare", x = NULL, y = NULL) + theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5)) + facet_wrap( ~ president, scales = "free_y") + scale_x_reordered()


df_wide <- speeches %>% pivot_wider(names_from = president, values_from = n, values_fill = list(n = 0))
df_wide <- df_wide %>% mutate(ratio_yoon = ((yoon+1)/(sum(yoon+1))), ratio_lee = ((lee+1)/(sum(lee+1))), odds_ratio = (ratio_yoon/ratio_lee))

top10_2 <- df_wide %>% filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10) %>% arrange(-odds_ratio)
top10_2 <- bind_rows(df_wide %>% slice_max(order_by = odds_ratio, n = 10), df_wide %>% slice_min(order_by = odds_ratio, n = 10))
top10_2 <- top10_2 %>% mutate(president = ifelse(odds_ratio > 1, "yoon", "lee"), n = ifelse(odds_ratio > 1, yoon, lee))

top10_2 <- top10_2 %>%
  mutate(
    order_var = ifelse(president == "lee", -odds_ratio, odds_ratio)
  )

ggplot(
  top10_2,
  aes(x = reorder_within(word, order_var, president), y = odds_ratio, fill = president)
) +
  geom_col(show.legend = F) +
  coord_flip(clip = "off") +
  geom_text(aes(label = round(odds_ratio, 2)), hjust = -0.3) +
  labs(title = "Speech Odds Ratio", x = NULL, y = NULL) +
  theme(text = element_text(family = "a"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(5, 50, 5, 5),
        panel.spacing = unit(2, "lines")) +
  facet_wrap(~president, scales = "free") +
  scale_x_reordered()

speech_sentence <- bind_speeches %>% as_tibble() %>% unnest_tokens(input = value, output = sentence, token = "sentences") %>% filter(president == "yoon" & str_detect(sentence, "국민"))

df_wide <- df_wide %>% arrange(abs(1-odds_ratio))
df_wide_with_count <- df_wide %>% filter(yoon >= 5 & lee >= 5) %>% arrange(abs(1-odds_ratio)) %>% head(10)
df_wide_logratio <- df_wide %>% mutate(log_odds_ratio = log(odds_ratio), president = ifelse(odds_ratio > 1, "yoon", "lee")) %>% filter(yoon >= 3 & lee >= 3) %>% arrange(-log_odds_ratio) %>% select(word, log_odds_ratio, president)

tail(df_wide_logratio)

ggplot(df_wide_logratio, aes(x = reorder(word, log_odds_ratio), y = log_odds_ratio, fill = president)) + geom_col() + coord_flip() + labs(title = "log odds ratio text", x = NULL, y = NULL) + theme(text = element_text(family = "a"), plot.title = element_text(hjust =0.5))
