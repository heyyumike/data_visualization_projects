library(tidyverse)
library(ggtext)
library(rvest)

# FUNCTIONS
## function to scrap movie script from website
scrape_webpages <- function(urls) {
  scripts <- list()
  
  for (url in urls) {
    page <- read_html(url)
    script <- page %>% 
      html_nodes(".scrolling-script-container") %>%
      html_text() %>%
      gsub("\r\n\r\n\r\n                    \t\t\t", "", .)
    scripts[[url]] <- script
  }
  
  return(scripts)
}

## function to count words in list 
count_words <- function(scripts) {
  word_counts <- list()
  
  for (i in 1:length(scripts)) {
    word_count <- strsplit(scripts[[i]], "\\s+")[[1]] %>% length()
    word_counts[[i]] <- word_count
    names(word_counts)[i] <- names(scripts)[i]
  }
  
  return(word_counts)
}

## function that creates a data frame by scraping and counting words off tv scripts from springfieldspringfield.co.uk and joining it with runtime data from IMDB 
create_data <- function(script_urls, tv_series_name, episodes_data) {
  tv_series_scripts <- scrape_webpages(script_urls)
  
  tv_script_wordcounts <- count_words(tv_series_scripts) %>%
    unlist() %>% data.frame() %>%
    rownames_to_column("season_episode") %>%
    rename(word_count = ".") %>%
    mutate(season_episode = str_sub(season_episode, -6, -1),
           tv_series = tv_series_name)
  
  tv_series_full_data <- tv_script_wordcounts %>%
    left_join(episodes_data, by = c("tv_series", "season_episode")) %>%
    mutate(words_per_minute = word_count / runtime) %>%
    select(tv_series, season_episode, word_count, runtime, words_per_minute)
  
  return(tv_series_full_data)
}

## function for generating the URLs of webpages with TV episode scripts that we will be scraping from
generate_episode_urls <- function(seasons, episodes, tv_show) {
  show_urls <- c()
  
  for (season in seasons) {
    for (episode in 1:episodes[season]) {
      url <- paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=", tv_show, "&episode=s", str_pad(season, pad = 0, width = 2, "left"), "e", str_pad(episode, pad = 0, width = 2, "left"))
      show_urls <- c(show_urls, url)
    }
  }
  
  return(show_urls)
}

# runtime data for all TV Series and their respective seasons/episodes from IMDB
## table with TV shows information (specifically we are interested in tconst since we need that for each episodes runtime and their parentTconst)
tv_series_data <- read_tsv("title.basics.tsv") %>%
  filter(tconst %in% c("tt0141842", "tt7660850", "tt0804503", "tt0903747", "tt0306414",
                       "tt1856010", "tt5071412", "tt3032476", "tt1632701", "tt4574334")) # filtering for TV show title is not robust since there may be multiple results for the same title (e.g. Stranger Things); instead we'll filter with tconst which you can find off the TV show's IMDB page

tv_series_tconst <- tv_series_data %>% select(tconst) %>% pull()

# this table gives us each TV series we are interested in as well as their respective seasons/episodes
tv_episodes_data <- read_tsv("title.episode.tsv") %>% 
  filter(parentTconst %in% tv_series_tconst)

tv_episodes_t_const <- tv_episodes_data %>% 
  select(tconst) %>% pull()

episodes_data <- read_tsv("title.basics.tsv") %>%
  filter(tconst %in% tv_episodes_t_const)

# putting all episodes/tv series data together
tv_series_episodes_data <- episodes_data %>% 
  left_join(tv_episodes_data, by = "tconst") %>%
  left_join(tv_series_data %>% select(tconst, primaryTitle), by = c("parentTconst" = "tconst")) %>% 
  rename(tv_series = primaryTitle.y, runtime = runtimeMinutes, episode_name = originalTitle) %>%
  mutate(season_episode = paste0("s0", seasonNumber, "e", str_pad(episodeNumber, pad = 0, width = 2, "left")),
         runtime = as.integer(runtime)) %>%
  filter(!is.na(runtime)) %>% # these are the upcoming season 5 episodes for Stranger Things that is scheduled to come out in 2024; obviously we don't have the script for these episodes yet
  select(tv_series, season_episode, episode_name, runtime)

# TV SHOWS
## BREAKING BAD
breaking_bad_urls <- generate_episode_urls(seasons = seq(1,5,1), episodes = c(7,13,13,13,16), tv_show = "breaking-bad")

breaking_bad_full_data <- create_data(breaking_bad_urls, "Breaking Bad", tv_series_episodes_data)

## THE SOPRANOS
the_sopranos_urls <- generate_episode_urls(seasons = seq(1,6,1), episodes = c(13,13,13,13,13,21), tv_show = "the-sopranos")

the_sopranos_full_data <- create_data(the_sopranos_urls, "The Sopranos", tv_series_episodes_data)

## MAD MEN
mad_men_urls <- generate_episode_urls(seasons = seq(1,7,1), episodes = c(13,13,13,13,13,13,14), tv_show = "mad-men")

mad_men_full_data <- create_data(mad_men_urls, "Mad Men", tv_series_episodes_data)

## THE WIRE
the_wire_urls <- generate_episode_urls(seasons = seq(1,5,1), episodes = c(13,12,12,13,10), tv_show = "the-wire")

the_wire_full_data <- create_data(the_wire_urls, "The Wire", tv_series_episodes_data)

## SUCCESSION
succession_urls <- generate_episode_urls(seasons = seq(1,4,1), episodes = c(10,10,9,10), tv_show = "succession-2018")

succession_full_data <- create_data(succession_urls, "Succession", tv_series_episodes_data)

## HOUSE OF CARDS
house_of_cards_urls <- generate_episode_urls(seasons = seq(1,6,1), episodes = c(13,13,13,13,13,8), tv_show = "house-of-cards-2013")

house_of_cards_full_data <- create_data(house_of_cards_urls, "House of Cards", tv_series_episodes_data)

## OZARK
ozark_urls <- generate_episode_urls(seasons = seq(1,4,1), episodes = c(10,10,10,14), tv_show = "ozark-2017")

ozark_full_data <- create_data(ozark_urls, "Ozark", tv_series_episodes_data)

## BETTER CAUL SAUL
better_call_saul_urls <- generate_episode_urls(seasons = seq(1,6,1), episodes = c(10,10,10,10,10,13), tv_show = "better-call-saul-2015")

better_call_saul_full_data <- create_data(better_call_saul_urls, "Better Call Saul", tv_series_episodes_data)

## SUITS
suits_urls <- generate_episode_urls(seasons = seq(1,9,1), episodes = c(12,16,16,16,16,16,16,16,10), tv_show = "suits")

suits_full_data <- create_data(suits_urls, "Suits", tv_series_episodes_data)

## STRANGER THINGS
stranger_things_urls <- generate_episode_urls(seasons = seq(1,4,1), episodes = c(8,9,8,9), tv_show = "stranger-things-2016")

stranger_things_full_data <- create_data(stranger_things_urls, "Stranger Things", tv_series_episodes_data)

# GRAPHING IT ALL OUT
# Owen Phillips graph 
breaking_bad_full_data %>% 
  bind_rows(the_sopranos_full_data, mad_men_full_data, the_wire_full_data, succession_full_data) %>%
  ggplot(aes(x = words_per_minute, y = fct_reorder(tv_series, words_per_minute, median), fill = tv_series)) +
  geom_boxplot(outlier.shape = NA, width = 0.25, alpha = 0.5) +
  geom_jitter(aes(color = tv_series), size = 5, alpha = 0.5, shape = 19, height = 0.2) +
  scale_x_continuous(limits = c(0,175), breaks = seq(0,175,25), name = "Words Per Minute") +
  ylab("") +
  theme_minimal() +
  guides(fill = "none", color = "none") + 
  labs(
    title = "<strong style='font-size:36pt'>The Wordiness of Prestige TV Dramas</strong><br><br>
    <span style='font-size:14pt'>Each dot represents the words per minute of an episode of the corresponding show</span>",
    x = "", y = ""
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee", size = 1.25),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.title.x = element_text(size = 24),
    plot.title = element_markdown(lineheight = 0.25)
  ) +
  labs(caption = "Source: springfieldspringfield.co.uk, IMDB", size = 24)

# similar graph with personal TV shows favorites
house_of_cards_full_data %>% 
  bind_rows(ozark_full_data, breaking_bad_full_data, better_call_saul_full_data, suits_full_data, stranger_things_full_data) %>%
  ggplot(aes(x = words_per_minute, y = fct_reorder(tv_series, words_per_minute, median), fill = tv_series)) +
  geom_boxplot(outlier.shape = NA, width = 0.25, alpha = 0.5) +
  geom_jitter(aes(color = tv_series), size = 5, alpha = 0.5, shape = 19, height = 0.2) +
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,25), name = "Words Per Minute") +
  ylab("") +
  theme_minimal() +
  guides(fill = "none", color = "none") + 
  labs(
    title = "<strong style='font-size:36pt'>The Wordiness of TV Dramas</strong><br><br>
    <span style='font-size:14pt'>Each dot represents the words per minute of an episode of the corresponding show</span>",
    x = "", y = ""
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee", size = 1.25),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.title.x = element_text(size = 24),
    plot.title = element_markdown(lineheight = 0.25)
  ) +
  labs(caption = "Source: springfieldspringfield.co.uk, IMDB")

# MISCELLANEOUS CODE
# episode runtimes are available in a table from another webpage; we need to scrap that data
bb_bcs_episode_runtime_page <- read_html("https://breakingbad.fandom.com/wiki/List_of_episodes_by_runtime")
bb_bcs_episode_runtime_table <- html_node(bb_bcs_episode_runtime_page, ".wikitable") %>% 
  html_table()

breaking_bad_episode_runtime <- bb_bcs_episode_runtime_table %>%
  filter(Series == "Breaking Bad") %>%
  select(Runtime, `S/Ep`, Series) %>%
  rename(runtime = Runtime, tv_series = Series, season_episode = `S/Ep`) %>%
  mutate(runtime = as.integer(gsub(" mn", "", runtime)), season_episode = tolower(season_episode),
         season_episode = paste0(substr(season_episode, 1, 1), "0", substr(season_episode, 2, nchar(season_episode))))

# episode runtimes are available in a table from another webpage; however, each season is split into a different webpage
the_sopranos_runtime_webpages <- c(paste0("https://thetvdb.com/series/the-sopranos/seasons/official/", seq(from = 1,to = 6,by = 1)))

the_sopranos_episode_runtime <- list()

for (webpage in the_sopranos_runtime_webpages) {
  runtime_table <- read_html(webpage) %>% 
    html_node("table") %>%
    html_table()
  
  the_sopranos_episode_runtime[[webpage]] <- runtime_table
}

the_sopranos_episode_runtime <- the_sopranos_episode_runtime %>% 
  bind_rows() %>%
  rename(season_episode = `...1`, runtime = Runtime) %>%
  mutate(tv_series = "The Sopranos", season_episode = tolower(season_episode)) %>%
  select(runtime, season_episode, tv_series)
