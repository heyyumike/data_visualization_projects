library(tidyverse)
library(nbastatR)
library(ggimage)

Sys.setenv(VROOM_CONNECTION_SIZE = 500000)

# list of players who won NBA MVP since 2000
nba_mvp <- bref_awards(awards = "Most Valuable Player") %>%
  tail(23) %>%
  mutate(namePlayer = recode(namePlayer, 'Nikola Jokić' = 'Nikola Jokic'))

# all nba games logs for both regular season and playoffs between 2000-2023
nba_game_logs <- game_logs(seasons = c(2000:2023), result_types = c("player"), season_types = c("Regular Season", "Playoffs"))

list_of_mvp_seasons <- nba_mvp %>% unite(col = "season_nba_player", c(slugSeason, namePlayer), sep = "-") %>%
  select(season_nba_player) %>% pull()

mvp_reg_vs_playoff_stats <- nba_game_logs %>% 
  unite(col = "season_nba_player", c(slugSeason, namePlayer), sep = "-", remove = FALSE) %>%
  filter(season_nba_player %in% list_of_mvp_seasons) %>% 
  group_by(slugSeason, namePlayer, typeSeason, urlPlayerHeadshot) %>%
  summarise(avg_points = mean(pts)) %>%
  spread(key = typeSeason, value = avg_points) %>%
  mutate(difference = Playoffs - `Regular Season`,
         performance = ifelse(difference > 0, "overperform", "underperform"))

# mvp regular season vs. playoff performance over the years
ggplot() +
  geom_segment(data = mvp_reg_vs_playoff_stats, 
               aes(x = slugSeason, xend = slugSeason, y = `Regular Season`, yend = Playoffs, color = performance),
               arrow = arrow(), size = 2) +
  geom_image(data = mvp_reg_vs_playoff_stats, aes(x = slugSeason, y = `Regular Season`, image = urlPlayerHeadshot)) +
  geom_image(data = mvp_reg_vs_playoff_stats, aes(x = slugSeason, y = ifelse(performance == "underperform", Playoffs - 0.5, Playoffs + 0.5), image = urlPlayerHeadshot)) +
  scale_y_continuous(name = "PPG", breaks = seq(from = 14, to = 38, by = 2), limits = c(14, 38)) +
  scale_color_manual(values = c("#4CBB17", "#FF2400")) +
  xlab("Season") +
  guides(color = "none") +
  ggtitle("MVPs Playoffs vs. Regular Season PPG Performance Since 2000") +
  theme_minimal()

# point differential by year
mvp_reg_vs_playoff_stats %>%
  ggplot(aes(x = reorder(slugSeason, desc(difference)), y = difference, fill = performance)) +
  geom_bar(stat = "identity") +
  geom_image(aes(image = urlPlayerHeadshot)) +
  scale_fill_manual(values = c("#4CBB17", "#FF2400")) +
  scale_y_continuous(name = "Difference Between Playoffs vs. Regular Season PPG", breaks = seq(from = -10, to = 10, by = 2), limits = c(-10, 10)) +
  xlab("Season") +
  guides(fill = "none") +
  ggtitle("MVPs Regular Season vs. Playoffs PPG Difference Performance Since 2000") +
  theme_minimal()
