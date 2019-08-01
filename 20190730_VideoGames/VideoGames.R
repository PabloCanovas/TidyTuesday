# Video Games Dataset

## Submission -------------------

library(tidyverse)
library(lubridate)
library(ggthemes)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

df <- video_games %>% 
  mutate(year = mdy(release_date) %>% year(),
         price = price %>% as.numeric() %>% round()) %>% 
  drop_na(year, metascore, price, publisher) %>%
  mutate(owners = owners %>% str_split("[..]") %>%
           map_int(~.x %>% first() %>% str_trim("right") %>% str_replace_all("[,]", "") %>% as.integer())) %>% 
  mutate(publisher = case_when(publisher %>% str_detect("SEGA") ~ "SEGA",
                               publisher %>% tolower() %>% str_detect("square enix") ~ "Square Enix",
                               publisher %>% tolower() %>% str_detect("capcom") ~ "CAPCOM",
                               publisher %>% tolower() %>% str_detect("2k") ~ "2K",
                               TRUE ~ publisher)) %>% 
  mutate_at(vars(developer, publisher), ~ as.factor(.))

best_publishers <- df %>% 
  group_by(publisher) %>% 
  summarise(profit = sum(price*owners)/1e6) %>% 
  ungroup() %>% 
  top_n(12, profit)

df %>% 
  filter(publisher %in% best_publishers$publisher) %>%
  mutate(year = release_date %>% lubridate::year()) %>% 
  group_by(year, publisher) %>% 
  mutate(mean_score = mean(metascore),
         mean_price = mean(price),
         number_of_games = n()) %>% 
  ungroup() %>% 
  select(year, publisher, mean_score, mean_price, number_of_games) %>% 
  gather(Var, Value, -year, -publisher, -number_of_games) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = Value, col = Var)) +
  geom_point(aes(x = year, y = Value, size = number_of_games, col = Var)) +
  facet_wrap(~ publisher) + 
  theme(axis.title.y = element_text(angle = 0, vjust = .5), 
        plot.title = element_text(hjust = .5, size = 15), 
        plot.subtitle = element_text(hjust = .5, size = 12)) +
  labs(title = "Average Score and Price evolution by top Publishers", 
       subtitle = "Top publishers based on estimated profits",
       x = "Year", y = "Score\n & \nPrice") +
  scale_color_tableau()


ggsave("Score_Price_Evolution_Publishers.png", units = "in", width = 14, height = 8)


## Complete analysis -----------------------------------


# De cara a un posible RMarkdown?


library(tidyverse)
library(ggthemes)
library(ggridges)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

df <- video_games %>% 
  mutate(release_date = lubridate::mdy(release_date),
         price = price %>% as.numeric() %>% round()) %>% 
  drop_na(release_date, metascore, price, publisher) %>%
  mutate(owners = owners %>% str_split("[..]") %>%
           map_int(~.x %>% first() %>% str_trim("right") %>% str_replace_all("[,]", "") %>% as.integer())) %>% 
  mutate(publisher = case_when(publisher %>% str_detect("SEGA") ~ "SEGA",
                               publisher %>% tolower() %>% str_detect("square enix") ~ "Square Enix",
                               publisher %>% tolower() %>% str_detect("capcom") ~ "CAPCOM",
                               publisher %>% tolower() %>% str_detect("2k") ~ "2K",
                               TRUE ~ publisher)) %>% 
  mutate_at(vars(developer, publisher), ~ as.factor(.))


# Boxplot + points de los videojuegos mas vendidos de las 10 desarrolladoras con mas juegos

big_publishers <- df %>% 
  group_by(publisher) %>% 
  summarise(N_by_publisher = n()) %>% 
  ungroup() %>% 
  top_n(10, N_by_publisher)

df %>%
  filter(publisher %in% big_publishers$publisher) %>% 
  ggplot() + 
  geom_boxplot(aes(fct_reorder(publisher, metascore, median), metascore,
                   col = fct_reorder(publisher, metascore, median, .desc = TRUE)), alpha = .5) +
  geom_jitter(aes(publisher, metascore, col = publisher), alpha = .4) +
  coord_flip() + 
  labs(title = "Score distribution from the 10 publishers with more titles", 
       x = "publisher",
       col = "publisher")


# Boxplot + points de los videojuegos mas vendidos de las 10 desarrolladoras con mejores puntuaciones

top_publishers <- df %>% 
  group_by(publisher) %>% 
  mutate(N_by_publisher = n()) %>% 
  filter(N_by_publisher >= 5) %>% 
  summarise(median_score = median(metascore)) %>% 
  ungroup() %>% 
  top_n(10, median_score)

df %>%
  filter(publisher %in% top_publishers$publisher) %>% 
  ggplot() + 
  geom_boxplot(aes(fct_reorder(publisher, metascore, median), metascore,
                   col = fct_reorder(publisher, metascore, median, .desc = TRUE)), alpha = .5) +
  geom_jitter(aes(publisher, metascore, col = publisher), alpha = .4) +
  coord_flip() + 
  labs(title = "Score distribution from top 10 publishers", 
       x = "publisher",
       col = "publisher")


# ridge con los años de lanzamiento y las metascore

df %>% 
  mutate(year = release_date %>% lubridate::year() %>% as.factor()) %>% 
  group_by(year) %>% 
  mutate(NGames_year = n()) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = metascore, y = fct_rev(year), fill = NGames_year)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5)) + 
  labs(title = "Metascore evolution", y = "Year", x = "Metascore")


# ridge con los años de lanzamiento y los precios

df %>% 
  mutate(year = release_date %>% lubridate::year() %>% as.factor()) %>% 
  group_by(year) %>% 
  mutate(NGames_year = n()) %>% 
  ggplot() + 
  geom_density_ridges(aes(x = price, y = fct_rev(year), fill = NGames_year)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5)) + 
  labs(title = "Price evolution", y = "Year", x = "Price")



# Evolucion de los precios con el tiempo por desarrolladora (las top)

top_publishers <- df %>% 
  group_by(publisher) %>% 
  mutate(N_by_publisher = n()) %>% 
  filter(N_by_publisher >= 5) %>% 
  summarise(median_score = median(metascore)) %>% 
  ungroup() %>% 
  top_n(15, median_score)

df %>% 
  filter(publisher %in% top_publishers$publisher) %>% 
  mutate(year = release_date %>% lubridate::year() %>% as.factor()) %>% 
  group_by(year, publisher) %>% 
  mutate(mean_price = mean(price)) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = mean_price, col = publisher, group = publisher)) +
  geom_point(aes(x = year, y = mean_price, col = publisher)) + 
  theme(axis.title.y = element_text(angle = 0, vjust = .5)) + 
  labs(title = "Price evolution", x = "Year", y = "AvgPrice")


# Evolucion de los metascore con el tiempo por desarrolladora (las que mas juegos sacan)

# big_publishers <- df %>% 
#   group_by(publisher) %>% 
#   summarise(N_by_publisher = n()) %>% 
#   ungroup() %>% 
#   top_n(16, N_by_publisher) %>% 
#   arrange(-N_by_publisher) %>% 
#   head(16)
# 
# 
# top_publishers <- df %>% 
#   group_by(publisher) %>% 
#   mutate(N_by_publisher = n()) %>% 
#   filter(N_by_publisher >= 10) %>% 
#   summarise(median_score = median(metascore)) %>% 
#   ungroup() %>% 
#   top_n(16, median_score) %>% 
#   arrange(-median_score) %>% 
#   head(16)

best_publishers <- df %>% 
  group_by(publisher) %>% 
  mutate(N_by_publisher = n()) %>% 
  summarise(profit = sum(price*owners)/1e6) %>% 
  ungroup() %>% 
  top_n(12, profit)

df %>% 
  filter(publisher %in% best_publishers$publisher) %>%
  mutate(year = release_date %>% lubridate::year()) %>% 
  group_by(year, publisher) %>% 
  mutate(mean_score = mean(metascore),
         mean_price = mean(price),
         number_of_games = n()) %>% 
  ungroup() %>% 
  select(year, publisher, mean_score, mean_price, number_of_games) %>% 
  gather(Var, Value, -year, -publisher, -number_of_games) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = Value, col = Var)) +
  geom_point(aes(x = year, y = Value, size = number_of_games, col = Var)) +
  facet_wrap(~ publisher) + 
  theme(axis.title.y = element_text(angle = 0, vjust = .5), 
        plot.title = element_text(hjust = .5, size = 15), 
        plot.subtitle = element_text(hjust = .5, size = 12)) +
  labs(title = "Average Score and Price evolution by top publishers", 
       subtitle = "Top publishers based on estimated profits",
       x = "Year", y = "Score\n & \nPrice") +
  scale_color_tableau()



