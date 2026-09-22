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
api_key      <- "SECRET"
game_name    <- "Tickleeeeeee"
tag_line     <- "420"
region       <- "americas"
n_matches    <- 100

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
    champion          = me$championName,
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

match_ids_url <- sprintf(
  "https://%s.api.riotgames.com/lol/match/v5/matches/by-puuid/%s/ids?start=0&count=%d",
  region, puuid, n_matches
)
match_ids <- riot_get(match_ids_url)

my_last_10 <- map_dfr(match_ids, get_my_stats, puuid = puuid, region = region)

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

# Riot's internal champion IDs don't always match Data Dragon's display names
champion_name_fixes <- c(
  "JarvanIV"    = "Jarvan IV",
  "DrMundo"     = "Dr. Mundo",
  "Khazix"      = "Kha'Zix",
  "KogMaw"      = "Kog'Maw",
  "MissFortune" = "Miss Fortune",
  "Chogath"     = "Cho'Gath"
)

# data transformations
my_last_10 <- my_last_10 %>%
  mutate(
    champion = recode(champion, !!!champion_name_fixes),
    day_of_week = factor(weekdays(date),
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    hour = factor(sprintf("%02d:00", hour(date)), levels = sprintf("%02d:00", 0:23))
  ) %>%
  left_join(champ_roles, by = "champion")

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

# Summarize win rate, KDA, damage share, etc. grouped by any column (champion, primary_role, ...)
summarize_performance <- function(df, group_col) {
  df %>%
    filter(win != "Remake") %>%
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
    )
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

champion_performance <- summarize_performance(my_last_10, "champion")
champion_heatmap_data <- build_heatmap_data(champion_performance, "champion")
champion_performance_plot <- plot_heatmap(champion_heatmap_data, "champion", "Champion Performance Heatmap")

role_performance <- summarize_performance(my_last_10, "primary_role")
role_heatmap_data <- build_heatmap_data(role_performance, "primary_role")
role_performance_plot <- plot_heatmap(role_heatmap_data, "primary_role", "Role Performance Heatmap")

grid.arrange(champion_performance_plot, role_performance_plot, nrow = 2, heights = c(2,1))

# =========================================================
# ACTIVITY PATTERNS (day of week / hour of day)
# =========================================================

# Summarize games played, win rate, and W-L record for any time grouping
summarize_activity <- function(df, group_col) {
  df %>%
    filter(win != "Remake") %>%
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
day_activity <- build_activity_data(my_last_10, "day_of_week")
daily_activity_plot <- plot_activity(day_activity, "day_of_week", "Daily Activity Pattern", "Day of Week")

hour_activity <- summarize_activity(my_last_10, "hour")
hourly_activity_plot <- plot_activity(hour_activity, "hour", "Hourly Activity Pattern", "Hour of Day", flip = TRUE)

grid.arrange(daily_activity_plot, hourly_activity_plot, nrow = 2)

# =========================================================
# ARAM TANKINESS INDEX (damage taken vs. mitigated per game)
# =========================================================

tankiness_data <- my_last_10 %>% 
  filter(win != "Remake") %>%
  group_by(champion) %>%
  summarise(
    games_played = n(),
    avg_damage_taken = mean(damage_taken),
    avg_damage_mitigated = mean(damage_mitigated),
    icon_url = first(icon_url),
    .groups = "drop"
  ) %>%
  filter(games_played >= 2)  # drop one-off picks so a single fluke game doesn't dominate

tankiness_data %>%
  ggplot(aes(x = avg_damage_taken, y = avg_damage_mitigated)) +
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
  scale_x_continuous(labels = comma, limits = c(10000, 70000), breaks = seq(10000, 70000, 10000)) +
  scale_y_continuous(labels = comma, limits = c(5000, 85000), breaks = seq(5000, 85000, 20000)) +
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

# =========================================================
# ARAM DEATH HEATMAP - MULTI-MAP (Howling Abyss / Koeshin's Crossing / Butcher's Bridge)
# =========================================================

# Pull your death locations from a match's timeline (separate endpoint, separate API call per match)
get_death_locations <- function(match_id, puuid, region) {
  Sys.sleep(1.2)
  
  url <- sprintf("https://%s.api.riotgames.com/lol/match/v5/matches/%s/timeline", region, match_id)
  timeline <- riot_get(url)
  
  # map puuid -> participantId for this specific match
  my_participant_id <- timeline$info$participants %>%
    filter(puuid == !!puuid) %>%
    pull(participantId)
  
  if (length(my_participant_id) == 0) return(tibble())
  
  all_events <- map_dfr(timeline$info$frames$events, bind_rows)
  
  all_events %>%
    filter(type == "CHAMPION_KILL", victimId == my_participant_id) %>%
    transmute(
      match_id,
      timestamp_min = round(timestamp / 60000, 1),
      x = position.x,
      y = position.y
    )
}

# Only pull timelines for ARAM matches (skip the API cost for Summoner's Rift, etc.)
aram_match_ids <- my_last_10 %>%
  filter(game_mode == "ARAM", win != "Remake") %>%
  pull(match_id)

death_data <- map_dfr(aram_match_ids, get_death_locations, puuid = puuid, region = region)

# gameModeMutators tells us which of the 3 ARAM map skins was used.
# NA (no mutator) = classic Howling Abyss, per the pattern observed so far.
determine_map_name <- function(mutator) {
  case_when(
    is.na(mutator) ~ "Howling Abyss",
    str_detect(mutator, regex("bloom|koeshin", ignore_case = TRUE)) ~ "Koeshin's Crossing",
    str_detect(mutator, regex("bilgewater|butcher", ignore_case = TRUE)) ~ "Butcher's Bridge",
    TRUE ~ "Unknown"  # catch-all in case the mutator string doesn't match a known pattern
  )
}

my_last_10 <- my_last_10 %>%
  mutate(map_name = determine_map_name(mutator))

# All 3 skins share the same underlying map12 folder/coordinate system -
# only the background image differs
map_config <- list(
  "Howling Abyss" = list(
    image_url = "https://raw.communitydragon.org/pbe/game/assets/maps/info/map12/2dlevelminimap.png"
  ),
  "Koeshin's Crossing" = list(
    image_url = "https://raw.communitydragon.org/pbe/game/assets/maps/info/map12/2dlevelminimap_bloom.png"
  ),
  "Butcher's Bridge" = list(
    image_url = "https://raw.communitydragon.org/pbe/game/assets/maps/info/map12/2dlevelminimap_bilgewater2.png"
  )
)
map_xmin <- -28; map_xmax <- 12849
map_ymin <- -19; map_ymax <- 12858

# Join map_name (derived from match-level data) into your per-death rows
match_map_lookup <- my_last_10 %>% select(match_id, map_name)

death_data <- death_data %>%
  left_join(match_map_lookup, by = "match_id")

# Render one heatmap per map skin you've actually played on
plot_death_heatmap_by_skin <- function(death_data, target_map_name) {
  cfg <- map_config[[target_map_name]]
  if (is.null(cfg)) return(invisible(NULL))  # skip "Unknown" or unmapped skins
  
  map_deaths <- death_data %>% filter(map_name == target_map_name)
  if (nrow(map_deaths) == 0) return(invisible(NULL))
  
  tmp_file <- tempfile(fileext = ".png")
  download.file(cfg$image_url, destfile = tmp_file, mode = "wb")
  map_img <- readPNG(tmp_file)
  
  ggplot(map_deaths, aes(x = x, y = y)) +
    annotation_custom(rasterGrob(map_img, width = unit(1, "npc"), height = unit(1, "npc")),
                      xmin = map_xmin, xmax = map_xmax, ymin = map_ymin, ymax = map_ymax) +
    geom_point(color = "red", size = 1.5, alpha = 0.6) +
    coord_fixed(xlim = c(map_xmin, map_xmax), ylim = c(map_ymin, map_ymax)) +
    labs(title = paste("Death Heatmap -", target_map_name), x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# Loop over whichever skins actually show up in your data
maps_played <- unique(death_data$map_name)
death_heatmaps <- map(maps_played, ~ plot_death_heatmap_by_skin(death_data, .x)) %>%
  set_names(maps_played) %>%
  compact()  # drop any NULLs (e.g. "Unknown" or zero-death maps)

death_heatmaps  # print/inspect each; or grid.arrange(grobs = death_heatmaps, ncol = 1) to stack them

grid.arrange(
  grobs = death_heatmaps,             # list of ggplot objects -- grid.arrange accepts this directly
  ncol = length(death_heatmaps),      # side-by-side, one column per map you've played
  top = "ARAM Death Heatmaps by Map"  # shared title across the whole arrangement
)
