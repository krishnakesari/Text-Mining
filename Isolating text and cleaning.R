## Looking at text individually

plot_physics %>%
  group_by(author) %>%
  top_n(25, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()


## Isolating a text (eg: "eq" from Einstein text)
library(stringr)
physics %>%
  filter(str_detect(text, "eq\\.")) %>%
  select(text)

physics %>%
  filter(str_detect(text, "K1")) %>%
  select(text)

physics %>%
  filter(str_detect(text, "AK")) %>%
  select(text)

## cleaning
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak",
                                   "bn", "fig", "file", "cg", "cb",
                                   "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")

plot_physics <- physics_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()
