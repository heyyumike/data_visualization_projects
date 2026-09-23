library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)
library(lubridate)
library(scales)
library(gridExtra)
library(ggimage)
library(png)
library(grid)
library(stringr)

### GRABBING LEAGUE OF LEGENDS MATCH DETAILS
# config
api_key      <- ""
game_name    <- "Tickleeeeeee"
tag_line     <- "420"
region       <- "americas"

# 2026 date range in epoch seconds (UTC)
start_time <- as.numeric(as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
end_time   <- as.numeric(as.POSIXct("2026-12-31 23:59:59", tz = "UTC"))

ARAM_QUEUE_ID <- 450  # used to filter match IDs server-side before fetching full match details

# =========================================================
# RIOT API HELPERS
# =========================================================

riot_get <- function(url) {
  res <- GET(url, add_headers("X-Riot-Token" = api_key))
  if (status_code(res) != 200) {
    stop(sprintf("Request failed [%s]: %s", status_code(res), content(res, "text")))
  }
  fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
}

get_my_stats <- function(match_id, puuid, region) {
  Sys.sleep(1.2)
  
  url <- sprintf("https://%s.api.riotgames.com/lol/match/v5/matches/%s", region, match_id)
  match <- riot_get(url)
  
  participants <- match$info$participants
  me <- participants %>% filter(puuid == !!puuid)
  
  my_team_id <- me$teamId
  team_totals <- participants %>%
    filter(teamId == my_team_id) %>%
    summarise(damage = sum(totalDamageDealtToChampions), kills = sum(kills))
  
  tibble(
    match_id          = match_id,
    game_mode         = match$info$gameMode,
    mutator           = if (length(match$info$gameModeMutators) == 0) {
      NA_character_
    } else {
      paste(match$info$gameModeMutators, collapse = ", ")
    },
    map_id            = match$info$mapId,
    date              = as.POSIXct(match$info$gameStartTimestamp / 1000, origin = "1970-01-01"),
    champion_raw      = me$championName,          # raw internal name from the match API (kept for reference/debugging)
    champion_id       = as.character(me$championId),  # numeric champion id -- join key into Data Dragon
    win               = case_when(
      me$gameEndedInEarlySurrender ~ "Remake",
      me$win ~ "Victory",
      TRUE ~ "Defeat"
    ),
    kills             = me$kills,
    deaths            = me$deaths,
    assists           = me$assists,
    team_kills        = team_totals$kills,
    double_kills      = me$doubleKills,
    triple_kills      = me$tripleKills,
    quadra_kills      = me$quadraKills,
    penta_kills       = me$pentaKills,
    kda               = round((me$kills + me$assists) / pmax(me$deaths, 1), 2),
    cs                = me$totalMinionsKilled + me$neutralMinionsKilled,
    gold              = me$goldEarned,
    damage_dealt      = me$totalDamageDealtToChampions,
    team_damage       = team_totals$damage,
    damage_share_pct  = round(me$totalDamageDealtToChampions / team_totals$damage * 100, 1),
    vision_score      = me$visionScore,
    damage_mitigated  = me$damageSelfMitigated,
    damage_taken      = me$totalDamageTaken,
    game_duration_min = round(match$info$gameDuration / 60, 1)
  )
}

# =========================================================
# PULL MATCH DATA
# =========================================================

account_url <- sprintf(
  "https://%s.api.riotgames.com/riot/account/v1/accounts/by-riot-id/%s/%s",
  region, URLencode(game_name), URLencode(tag_line)
)
puuid <- riot_get(account_url)$puuid

# page through all match IDs within date range but only fetch match details for ARAM games
get_all_match_ids <- function(puuid, region, start_time, end_time, queue = NULL) {
  all_ids <- character(0)
  start <- 0
  count <- 100
  
  repeat {
    url <- sprintf(
      paste0(
        "https://%s.api.riotgames.com/lol/match/v5/matches/by-puuid/%s/ids",
        "?startTime=%d&endTime=%d&start=%d&count=%d",
        if (!is.null(queue)) sprintf("&queue=%d", queue) else ""
      ),
      region, puuid, as.integer(start_time), as.integer(end_time), start, count
    )
    batch <- riot_get(url)
    Sys.sleep(1.2)  # throttle
    
    if (length(batch) == 0) break
    all_ids <- c(all_ids, batch)
    
    if (length(batch) < count) break  # last page
    start <- start + count
  }
  
  all_ids
}

match_ids <- get_all_match_ids(puuid, region, start_time, end_time, queue = ARAM_QUEUE_ID)
length(match_ids)  # sanity check: how many 2026 ARAM games you played

# match history
match_history <- map_dfr(match_ids, get_my_stats, puuid = puuid, region = region) %>%
  filter(win != "Remake", game_mode == "ARAM")

# =========================================================
# CHAMPION ROLES (Data Dragon)
# =========================================================

patch <- fromJSON("https://ddragon.leagueoflegends.com/api/versions.json")[1]
champ_data <- fromJSON(sprintf(
  "https://ddragon.leagueoflegends.com/cdn/%s/data/en_US/champion.json", patch
))

champ_roles <- champ_data$data %>%
  map_dfr(~ tibble(champion = .x$name, 
                   key = .x$key, 
                   primary_role = .x$tags[1],
                   icon_url = paste0("https://ddragon.leagueoflegends.com/cdn/", patch, "/img/champion/", .x$id, ".png")))

# data transformations
match_history <- match_history %>%
  mutate(
    day_of_week = factor(weekdays(date),
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    hour = factor(sprintf("%02d:00", hour(date)), levels = sprintf("%02d:00", 0:23))
  ) %>%
  left_join(champ_roles, by = c("champion_id" = "key"))

# =========================================================
# REUSABLE ANALYSIS FUNCTIONS
# =========================================================

# Column order (left -> right) + display labels, single source of truth
stat_order <- c(
  games_played              = "Games Played",
  win_rate                  = "Win Rate",
  kda                       = "KDA",
  damage_share_pct          = "Damage Share %",
  kill_participation_pct    = "Kill Participation %",
  avg_game_duration         = "Avg Game Duration\n(in minutes)",
  multikill_score_per_game  = "Multikill Score"
)

pct_stats <- c("win_rate", "damage_share_pct", "kill_participation_pct")

MIN_GAMES_FOR_HEATMAP <- 2  # drop one-off picks so a single fluke game doesn't dominate a row

# Summarize win rate, KDA, damage share, etc. grouped by any column (champion, primary_role, ...)
summarize_performance <- function(df, group_col, min_games = 1) {
  df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      games_played = n(),
      win_rate = sum(win == "Victory") / games_played,
      kda = (sum(kills) + sum(assists)) / pmax(sum(deaths), 1),
      damage_share_pct = sum(damage_dealt) / sum(team_damage),
      kill_participation_pct = (sum(kills) + sum(assists)) / sum(team_kills),
      multikill_score_per_game = sum(double_kills * 1 + triple_kills * 2 +
                                       quadra_kills * 3 + penta_kills * 4) / games_played,
      avg_game_duration = mean(game_duration_min),
      .groups = "drop"
    ) %>%
    filter(games_played >= min_games)
}

# Reshape a performance summary into long format, scaled per-stat for heatmap coloring
build_heatmap_data <- function(summary_df, group_col) {
  summary_df %>%
    select(all_of(group_col), all_of(names(stat_order))) %>%
    mutate(games_played_order = games_played) %>%
    pivot_longer(-c(all_of(group_col), games_played_order), names_to = "stat", values_to = "value") %>%
    group_by(stat) %>%
    mutate(
      value_scaled = (value - min(value)) / (max(value) - min(value)),
      label = if_else(
        stat %in% pct_stats,
        paste0(round(value * 100, 1), "%"),
        as.character(round(value, 2))
      )
    ) %>%
    ungroup() %>%
    mutate(
      across(all_of(group_col), ~ fct_reorder(.x, games_played_order, .desc = FALSE)),
      stat = factor(stat, levels = names(stat_order))
    )
}

# Render the heatmap for a given group column
plot_heatmap <- function(heatmap_data, group_col, title) {
  ggplot(heatmap_data, aes(x = stat, y = .data[[group_col]], fill = value_scaled)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label), color = "black", size = 4) +
    scale_fill_gradient(low = "#fee8c8", high = "#e34a33", name = "Relative\nPerformance") +
    scale_x_discrete(labels = stat_order) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
}

# =========================================================
# BUILD + PLOT: CHAMPION AND ROLE HEATMAPS
# =========================================================

champion_performance <- summarize_performance(match_history, "champion", min_games = MIN_GAMES_FOR_HEATMAP)
champion_heatmap_data <- build_heatmap_data(champion_performance, "champion")
champion_performance_plot <- plot_heatmap(champion_heatmap_data, "champion", "Champion Performance Heatmap")

role_performance <- summarize_performance(match_history, "primary_role")  
role_heatmap_data <- build_heatmap_data(role_performance, "primary_role")
role_performance_plot <- plot_heatmap(role_heatmap_data, "primary_role", "Role Performance Heatmap")

champion_performance_plot
role_performance_plot

# =========================================================
# ACTIVITY PATTERNS (day of week / hour of day)
# =========================================================

# Summarize games played, win rate, and W-L record for any time grouping
summarize_activity <- function(df, group_col) {
  df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      games_played = n(),
      wins = sum(win == "Victory"),
      losses = sum(win == "Defeat"),
      win_rate = wins / games_played,
      record = paste0(
        wins, if_else(wins == 1, " win, ", " wins, "),
        losses, if_else(losses == 1, " loss", " losses")
      ),
      .groups = "drop"
    )
}

# Summarize + fill in any factor level with zero games played (e.g. a day/hour never played)
# Requires group_col to already be a factor with the full set of levels defined upstream
build_activity_data <- function(df, group_col) {
  df %>%
    summarize_activity(group_col) %>%
    complete(.data[[group_col]],
             fill = list(games_played = 0, wins = 0, losses = 0, win_rate = 0, record = "0 wins, 0 losses")
    )
}

# Bar chart of activity, colored by win rate, with a games/record/win-rate label per bar
plot_activity <- function(summary_df, x_col, title, x_lab, flip = FALSE) {
  p <- ggplot(summary_df, aes(x = .data[[x_col]], y = games_played, fill = win_rate)) +
    geom_col(color = "black", linewidth = 0.3) +
    geom_text(
      aes(label = paste0("Games: ", games_played, "\n", record, "\nWin Rate: ", scales::percent(win_rate, accuracy = 1))),
      size = 3, lineheight = 0.9,
      hjust = if (flip) -0.05 else 0.5,
      vjust = if (flip) 0.5 else -0.2
    ) +
    scale_fill_gradient2(
      low = "#e34a33", mid = "grey85", high = "#2c7fb8", midpoint = 0.5,
      limits = c(0, 1), labels = scales::percent, name = "Win Rate"
    ) +
    scale_y_continuous(name = "Games Played", expand = expansion(mult = c(0, 0.3))) +
    labs(x = x_lab, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (flip) element_blank() else element_line(color = "grey92"),
      panel.grid.major.y = if (flip) element_line(color = "grey92") else element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  if (flip) p <- p + coord_flip()
  p
}

# =========================================================
# BUILD + PLOT: HOUR/DAY OF WEEK PLOTS
# =========================================================

# hourly/day of week data models/plots
day_activity <- build_activity_data(match_history, "day_of_week")
daily_activity_plot <- plot_activity(day_activity, "day_of_week", "Daily Activity Pattern", "Day of Week")

hour_activity <- summarize_activity(match_history, "hour")
hourly_activity_plot <- plot_activity(hour_activity, "hour", "Hourly Activity Pattern", "Hour of Day", flip = TRUE)

grid.arrange(daily_activity_plot, hourly_activity_plot, nrow = 2)

# =========================================================
# ARAM TANKINESS INDEX (damage taken vs. mitigated per game)
# =========================================================

build_tankiness_data <- function(df, min_games = MIN_GAMES_FOR_HEATMAP) {
  df %>%
    group_by(champion) %>%
    summarise(
      games_played = n(),
      avg_damage_taken = mean(damage_taken),
      avg_damage_mitigated = mean(damage_mitigated),
      icon_url = first(icon_url),
      .groups = "drop"
    ) %>%
    filter(games_played >= min_games)
}

plot_tankiness_index <- function(tankiness_data) {
  ggplot(tankiness_data, aes(x = avg_damage_taken, y = avg_damage_mitigated)) +
    geom_vline(xintercept = mean(tankiness_data$avg_damage_taken), linetype = "dashed", color = "black") +
    geom_hline(yintercept = mean(tankiness_data$avg_damage_mitigated), linetype = "dashed", color = "black") +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5,
             label = "Tanks / Frontliners\n(high taken, high mitigated)",
             size = 2.8, fontface = "italic", color = "red") +
    annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.8,
             label = "Exposed / Squishy\n(high taken, low mitigated)",
             size = 2.8, fontface = "italic", color = "red") +
    annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.5,
             label = "Passively Tanky\n(low taken, high mitigated)",
             size = 2.8, fontface = "italic", color = "red") +
    annotate("text", x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.8,
             label = "Backline / Low Exposure\n(low taken, low mitigated)",
             size = 2.8, fontface = "italic", color = "red") +
    geom_image(aes(image = icon_url)) +
    scale_size_continuous(range = c(0.05, 0.12), name = "Games Played") +
    scale_x_continuous(labels = comma, limits = c(0, NA), breaks = scales::breaks_pretty(n = 8)) +
    scale_y_continuous(labels = comma, limits = c(0, NA), breaks = scales::breaks_pretty(n = 8)) +
    labs(
      title = "ARAM Tankiness Index",
      subtitle = "Average damage taken vs. mitigated per game (dashed lines = averages)",
      x = "Avg Damage Taken per Game",
      y = "Avg Damage Mitigated per Game"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
}

tankiness_data <- build_tankiness_data(match_history)
plot_tankiness_index(tankiness_data)
