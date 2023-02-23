library(tidyverse)
library(pdftools)
library(stringr)
library(ggtext)

# read in data from pdf 
crime_statistics_pdf <- pdf_text("http://geoenlace.net/seguridadjusticiaypaz/archivo/5c1a88_130f090d9a.pdf")

# function that will read in table from pdf 
scrape_pdf <- function(list_of_tables,
                       table_number,
                       first_row,
                       last_row,
                       number_of_columns,
                       column_names) {
  
  data <- list_of_tables[table_number] %>%
    strsplit("\n") %>%
    unlist() %>%
    trimws()
  
  data <- data[first_row:last_row] %>% 
    str_split_fixed(" {2,}", number_of_columns) %>%
    data.frame()
  
  names(data) <- column_names
  
  return(data)
}

# table is split by in 2 different pages due to length; we will need to read them in separately
first_table <- scrape_pdf(list_of_tables = crime_statistics_pdf,
                          table_number = 3,
                          first_row = 9,
                          last_row = 47,
                          number_of_columns = 6,
                          column_names = c("position", "city", "country", "homicides", "population", "homicide_rate"))

second_table <- scrape_pdf(list_of_tables = crime_statistics_pdf,
                           table_number = 4,
                           first_row = 6,
                           last_row = 17,
                           number_of_columns = 6,
                           column_names = c("position", "city", "country", "homicides", "population", "homicide_rate"))

# first table has a few errors that we will have to manually account for (e.g. bold/highlighted font messed up reading in; and random 3 in row)
first_table[1,] <- data.frame(position = "1",
                              city = "Colima (AM)",
                              country = "México",
                              homicides = "601",
                              population = "330,329",
                              homicide_rate = "181.94")

# second table also has an error that we have to account for
second_table[5,] <- data.frame(position = "43",
                               city = "Buenaventura",
                               country = "Colombia",
                               homicides = "111",
                               population = "315,743",
                               homicide_rate = "35.16")

# combining the two tables together and excluding blank row
crime_statistics_data <- first_table %>% 
  rbind(second_table) %>%
  filter(position != "") %>%
  mutate(homicides = gsub(pattern = ",", replacement = "", x = homicides),
         population = gsub(pattern = ",", replacement = "", x = population),
         is_mexico = ifelse(country == "México", "yes", "no"),
         city = trimws(gsub(pattern = "\\(AM\\)", replacement = "", x = city), which = "both")) %>%
  mutate_at(.vars = c("homicides", "population", "homicide_rate"),
            .funs = as.numeric)

# plotting data
crime_statistics_data %>%
  ggplot(aes(x = reorder(city, homicide_rate), y = homicide_rate, fill = is_mexico)) +
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = homicide_rate), hjust = -0.1, fontface = "bold") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 200)) +
  guides(fill = "none") +
  labs(
    title = "<strong style='font-size:16pt'>Seventeen of the 50 most violent cities in the
    <span style='color:#F8766D;'>world </span><span>are located in </span><span style='color:#00BFC4;'>Mexico </span><br>
    <span style='font-size:12pt'>Ranking by murder rate per 100,000 inhabitants</span>
    </strong>",
    x = "", y = ""
  ) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold", color = "black"),
        plot.title = element_markdown(lineheight = 1.1),
        legend.text = element_markdown(size = 11)) +
  coord_flip()
