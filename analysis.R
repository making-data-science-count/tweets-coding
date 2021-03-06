library(tidyverse)
library(here)
library(rtweet)
library(tidytags) # tidytags::process_tweets(searched_statuses)

d <- read_csv(here('data', 'sample-of-tweets.csv'),
              col_types = cols(
                id_str = col_character()
              ))

d_level3 <- select(d, status_id = id_str, level3, created_at)

# searched_statuses <- tidytags::pull_tweet_data(id_vector = d_level3$status_id)
#
# write_rds(searched_statuses, "searched-statuses.rds")

searched_statuses <- read_rds("searched-statuses.rds")

searched_statuses <- tidytags::process_tweets(searched_statuses)

users_data <- rtweet::users_data(searched_statuses) %>%
  distinct(user_id, .keep_all = T)

users_data %>%
  skimr::skim()

dd <- searched_statuses %>%
  select(favorite_count, retweet_count, status_id) %>%
  left_join(d_level3)

interactions_table <- dd %>%
  group_by(level3) %>%
  mutate(total_interactions = favorite_count + retweet_count) %>%
  summarize(mean_fav = mean(favorite_count),
            sd_fav = sd(favorite_count),
            mean_rt = mean(retweet_count),
            sd_rt = sd(retweet_count),
            mean_total = mean(total_interactions),
            sd_total = sd(total_interactions)) %>%
  arrange(desc(mean_total))

interactions_table

interactions_table %>%
  write_csv("interactions-table.csv")

ddd <- d_level3 %>%
  mutate(created_at = lubridate::ymd_hms(created_at)) %>%
  mutate(week = lubridate::round_date(created_at, unit = "day")) %>%
  count(week, level3) %>%
  spread(level3, n, fill = 0) %>%
  gather(key, val, -week) %>%
  mutate(week_int = lubridate::week(week))

m0 <- lme4::glmer(val ~ 1 + (1|key), data = ddd, family = "poisson")
performance::icc(m0)

m1 <- lme4::glmer(val ~ 1 + week_int + (1|key), data = ddd, family = "poisson")
sjPlot::tab_model(m1)

top_codes <- d %>%
  count(level3, sort = TRUE) %>%
  filter(n > 50) %>%
  rename(key = level3)

dddd <- ddd %>%
  semi_join(top_codes)

f <- function(data) {
  glm(val ~ week_int, data = data, family = "poisson")
}

my_order <- top_codes %>% arrange(key)

my_order <- my_order %>%
  mutate(order = 1:10)

dddd %>%
  arrange(key) %>%
  group_by(key) %>%
  group_split() %>%
  purrr::map(f) %>%
  map_df(broom::tidy) %>%
  mutate(order = rep(1:10, each = 2)) %>%
  left_join(my_order) %>%
  filter(term == "week_int") %>%
  mutate(p.value = round(p.value, 3)) %>%
  arrange(desc(abs(estimate)))

km2 <- brms::brm(val ~ 1 + week_int + (week_int|key), data = ddd, family = "poisson")
m2
sjPlot::tab_model(m2)

m3 <- brms::brm(val ~ 1 + week_int + I(week_int^2) + (week_int + I(week_int^2)|key), data = ddd, family = "poisson")
sjPlot::tab_model(m3)
m3
brms::ranef(m3)

# unrelated to COVID decreases
# student highlight increases
# requests increase

broom.mixed::tidy(m2)

