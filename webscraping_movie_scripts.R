library(tidyverse)
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

# BREAKING BAD
## Breaking Bad has 62 episodes total, each season has different number of episdoes; produce webpage that stores script for each episode in TV series
breaking_bad_urls <- c(paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s01e", str_pad(1:7, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s02e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s03e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s04e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s05e", str_pad(1:16, pad = 0,width = 2 , "left")))

breaking_bad_scripts <- scrape_webpages(breaking_bad_urls)

breaking_bad_script_wordcounts <- count_words(breaking_bad_scripts) %>%
  unlist() %>% data.frame() %>%
  rownames_to_column("season_episode") %>%
  rename(word_count = ".") %>%
  mutate(season_episode = str_sub(season_episode, -6, -1))


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

# THE SOPRANOS
the_sopranos_urls <- c(paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s01e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s02e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s03e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s04e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s05e", str_pad(1:13, pad = 0,width = 2 , "left")),
                       paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=the-sopranos&episode=s06e", str_pad(1:21, pad = 0,width = 2 , "left")))

the_sopranos_scripts <- scrape_webpages(the_sopranos_urls)

the_sopranos_script_wordcounts <- count_words(the_sopranos_scripts) %>%
  unlist() %>% data.frame() %>%
  rownames_to_column("season_episode") %>%
  rename(word_count = ".") %>%
  mutate(season_episode = str_sub(season_episode, -6, -1))

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
