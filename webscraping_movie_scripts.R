library(tidyverse)
library(rvest)

# function to scrap movie script from website
scrape_webpages <- function(urls) {
  scripts <- list()
  
  for (url in urls) {
    page <- read_html(url)
    script <- page %>% 
      html_nodes(".scrolling-script-container") %>%
      html_text()
    scripts[[url]] <- script
  }
  
  return(scripts)
}

# Breaking Bad has 62 episodes total, each season has different number of episdoes; produce webpage that stores script for each episode in TV series
urls <- c(paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s01e", str_pad(1:7, pad = 0,width = 2 , "left")),
          paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s02e", str_pad(1:13, pad = 0,width = 2 , "left")),
          paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s03e", str_pad(1:13, pad = 0,width = 2 , "left")),
          paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s04e", str_pad(1:13, pad = 0,width = 2 , "left")),
          paste0("https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=breaking-bad&episode=s05e", str_pad(1:16, pad = 0,width = 2 , "left")))

scraped_scripts <- scrape_webpages(urls)

# function to count words in list 

test 