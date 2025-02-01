library(tidyverse)
library(rvest)
library(httr)
library(ggtext)

url <- "https://worldathletics.org/records/all-time-toplists/hurdles/400-metres-hurdles/all/women/senior?regionType=world&timing=electronic&page=1&bestResultsOnly=false&firstDay=1899-12-29&lastDay=2024-08-08&maxResultsByCountry=all&eventId=10229523&ageCategory=senior"

webpage <- read_html(url)

hurdles_data <- webpage %>%
  html_node("table") %>%
  html_table()

hurdles_data %>%
  select(Competitor, Mark, Rank, Date) %>%
  mutate(
    Competitor = ifelse(Competitor == "Sydney MCLAUGHLIN", "Sydney MCLAUGHLIN-LEVRONE", Competitor),
    transformed_time = Mark ^ 5,
    is_sydney_femke = case_when(
      Competitor == "Sydney MCLAUGHLIN-LEVRONE" ~ "Sydney McLaughlin-Levrone",
      Competitor == "Femke BOL" ~ "Femke Bol",
      TRUE ~ "Other"
    ),
    competitor_date_of_event = paste0(Competitor, " - ", Date)
  ) %>%
  filter(Rank <= 50) %>%
  ggplot(aes(x = reorder(competitor_date_of_event, -transformed_time), y = 1 / transformed_time, fill = is_sydney_femke)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Mark), hjust = -0.25) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("#F36C21", "white", "#0092C2")) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        panel.background = element_rect(fill = "floralwhite", color = NA),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(face = "bold", color = "black"),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(
    fill = "",
    title = "<strong style='font-size:20pt'>Two Women Hold 30 of the Top 51 Fastest Women's 400m Hurdle Times in History*</span></strong><br>
    <span style='font-size:14pt'>17 by <strong style='color:#F36C21;'>Femke Bol</strong><span>
    <span style='font-size:14pt'>and 13 by <strong style='color:#0092C2;'>Sydney McLaughlin-Levrone </strong><span>",
    x = "", y = "",
    caption = "Source: worldathletics.org\n*Data as of August 8th, 2024 - Paris 2024 Olympics 400 Meters Hurdles Women"
  ) + 
  guides(fill = "none") +
  coord_flip()
