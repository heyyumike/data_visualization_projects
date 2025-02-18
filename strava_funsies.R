library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(grid)
library(ggtext)

# initiate strava API instance based on user-based credentials
strava_key <- 134962
strava_secret <- 'ebb7e8db872392f704df01576cb08b64b50c4dfb'

app <- oauth_app("strava", strava_key, strava_secret)

endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

token <- oauth2.0_token(endpoint, app, as_header = FALSE,
                        scope = "activity:read_all")

activities_req <- GET(url = 'https://www.strava.com/api/v3/athlete/activities',
                      config = token,
                      query = list(per_page = 100))


# getting all activities data based on athlete activities endpoint
all_activities_data <- fromJSON(content(activities_req, as = 'text'), flatten = TRUE) %>%
  arrange(start_date_local) %>%
  mutate(start_date_local_date_only = as.Date(start_date_local),
         start_date_local = ymd_hms(start_date_local),
         day_of_week = wday(start_date_local, label = TRUE, abbr = FALSE, week_start = 7),
         run_type = case_when(
           grepl("Recovery Run", name) ~ "Recovery Run",
           grepl("Long Run", name) ~ "Long Run",
           grepl("Speed Run", name) ~ "Speed Run"
         ),
         is_race = case_when(
           grepl("Monterey Bay Half Marathon", name) ~ TRUE,
           grepl("Silicon Valley Turkey Trot", name) ~ TRUE,
           grepl("Kaiser Half Marathon", name) ~ TRUE,
           .default = FALSE
         ),
         start_date_local_time_numeric = as.numeric(hms(format(start_date_local, "%H:%M:%S"))) / 3600,
         distance_in_miles = distance / 1609,
         cumulative_distance_in_meters = cumsum(distance),
         cumulative_distance_in_miles = cumsum(distance_in_miles),
         pace_per_minute = (moving_time / distance_in_miles) / 60,
         ppm_minute = floor(pace_per_minute),
         ppm_second = sprintf("%02d", round((pace_per_minute - floor(pace_per_minute)) * 60)),
         converted_pace_per_miles = ms(paste0(ppm_minute, ":", ppm_second)),
         distance_category = factor(case_when(
           distance_in_miles >= 0 & distance_in_miles < 1 ~ "0-0.99 miles",
           distance_in_miles >= 1 & distance_in_miles < 2 ~ "1-1.99 miles",
           distance_in_miles >= 2 & distance_in_miles < 3 ~ "2-2.99 miles",
           distance_in_miles >= 3 & distance_in_miles < 4 ~ "3-3.99 miles",
           distance_in_miles >= 4 & distance_in_miles < 5 ~ "4-4.99 miles",
           distance_in_miles >= 5 & distance_in_miles < 6 ~ "5-5.99 miles",
           distance_in_miles >= 6 & distance_in_miles < 7 ~ "6-6.99 miles",
           distance_in_miles >= 7 & distance_in_miles < 8 ~ "7-7.99 miles",
           distance_in_miles >= 8 & distance_in_miles < 9 ~ "8-8.99 miles",
           distance_in_miles >= 9 & distance_in_miles < 10 ~ "9-9.99 miles",
           distance_in_miles >= 10 & distance_in_miles < 11 ~ "10-10.99 miles",
           distance_in_miles >= 11 & distance_in_miles < 12 ~ "11-11.99 miles",
           distance_in_miles >= 12 ~ "12+ miles"
         ), levels = c("0-0.99 miles", "1-1.99 miles", "2-2.99 miles", "3-3.99 miles", "4-4.99 miles", "5-5.99 miles",
                       "6-6.99 miles", "7-7.99 miles", "8-8.99 miles", "9-9.99 miles", "10-10.99 miles", "11-11.99 miles", "12+ miles")))

# cumulative distance 
all_activities_data %>%
  ggplot(aes(x = start_date_local, y = cumulative_distance_in_miles)) +
  geom_area(alpha = 0.5) +
  scale_y_continuous(name = "Cumulative Distance (in miles)", limits = c(0, 300)) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  annotate("segment", x = as.POSIXct("2024-07-20"), xend = as.POSIXct("2024-08-01"), y = 80, yend = 40, color = "#6A5ACD", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-07-20"), y = 89, label = "Hip Flexor Strain\n Out for ~ 3 weeks", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2024-08-18"), xend = as.POSIXct("2024-08-26"), y = 100, yend = 55, color = "#6A5ACD", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-08-15"), y = 108, label = "Shin Splints Pain\n Out for ~ 10 days", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2024-09-22"), xend = as.POSIXct("2024-09-26"), y = 125, yend = 85, color = "#6A5ACD", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-09-15"), y = 132, label = "More Shin Splints Pain\n Out for ~ 1 week", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2024-10-13"), xend = as.POSIXct("2024-10-26"), y = 150, yend = 132, color = "#808000", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-10-06"), y = 157, label = "Vacation\n Out for ~ 1.5 weeks", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2024-11-05"), xend = as.POSIXct("2024-11-08"), y = 180, yend = 158, color = "#B7410E", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-11-05"), y = 187, label = "Monterey Bay\nHalf Marathon", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2024-11-23"), xend = as.POSIXct("2024-11-27"), y = 205, yend = 186, color = "#B7410E", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2024-11-23"), y = 212, label = "Silicon Valley\nTurkey Trot", size = 3.5, fontface = "bold") +
  annotate("segment", x = as.POSIXct("2025-01-21"), xend = as.POSIXct("2025-01-30"), y = 291, yend = 287, color = "#B7410E", arrow = arrow(), size = 1) +
  annotate("text", x = as.POSIXct("2025-01-16"), y = 289, label = "Kaiser\nHalf Marathon", size = 3.5, fontface = "bold") +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        panel.background = element_rect(fill = "floralwhite", color = NA),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold", color = "black"),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2)) +
  labs(
    fill = "",
    title = "<strong style='font-size:20pt'>Cumulative Distance</strong><br>
           <span style='font-size:16pt'>Cumulative distance with arrows indicating key races/breaks in running (color-coded as <strong style='color:#6A5ACD;'>injuries</strong>, 
           <strong style='color:#808000;'>PTO/vacation</strong>, 
           and <strong style='color:#B7410E;'>races</strong>)</span>",
    x = "",
    caption = "Source: Strava API"
  )

# activity start time by day of week over time
all_activities_data %>%
  ggplot(aes(x = start_date_local, y = start_date_local_time_numeric, size = distance_in_miles)) +
  geom_point(alpha = 0.75, fill = "black", color = "white", shape = 21) +
  scale_size(range = c(2, 10)) + 
  scale_y_continuous(name = "Hour of Day", limits = c(0, 24), breaks = seq(0, 24, 2)) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 month") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
    legend.background = element_rect(fill = "floralwhite", color = "floralwhite"),
    text = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(face = "bold", color = "black"),
    plot.title = element_text(size = 20, face = "bold")
  ) +
  facet_wrap(~day_of_week, ncol = 7) +
  xlab("") +
  ggtitle(label = "Activity Start Time by Day of Week Over Time",
          subtitle = "Each point represents the hour of day for when a run was started, with distance mapped to the size aesthetic (larger points = longer runs)\nHot summer days = later starting run times (to avoid running in the heat) --> Shorter fall/winter days = earlier starting run times (run while the sun is still up)") +
  labs(size = "Distance",
       caption = "Source: Strava API")

# helper function to help format pace/mile axis labels (pace will automatically be converted to seconds otherwise)
seconds_to_mins_secs <- function(seconds) {
  mins <- floor(seconds / 60)
  secs <- seconds %% 60
  paste0(mins, "M ", sprintf("%02d", secs), "S")
}

# base plot with avg. pace by run type over time (necessary in order to annotate single facet)
avg_pace_by_run_type_plot <- all_activities_data %>%
  ggplot(aes(x = start_date_local, y = converted_pace_per_miles, fill = is_race)) +
  geom_jitter(alpha = 0.75, size = 4, shape = 21, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  scale_y_continuous(name = "Avg. Pace Per Mile", labels = seconds_to_mins_secs, limits = c(450, 660), breaks = seq(450, 660, 30)) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 month") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(face = "bold", color = "black"),
    plot.title = element_markdown(lineheight = 1.1),
    plot.margin = margin(10, 50, 10, 10)
  ) +
  facet_wrap(~run_type, ncol = 3) +
  labs(
    fill = "",
    title = "<strong style='font-size:20pt'>Avg. Pace Per Mile by Run Type Over Time</strong><br>
           <span style='font-size:16pt'>Each point represents the average pace per mile of a running activity. Points highlighted in <strong style='color:#FF0000;'>red</strong> indicate races and their respective average pace per mile</span>",
    x = "",
    caption = "Source: Strava API"
  )

# data frame with only data for races
only_races_data <- all_activities_data %>% filter(is_race == TRUE)
avg_pace_plot_with_race_annotations <- avg_pace_by_run_type_plot +
  geom_text(data = only_races_data, aes(y = c(587, 546, 543), label = c("Monterey Bay Half Marathon", "Silicon Valley Turkey Trot", "Kaiser\nHalf\nMarathon")), color = "red", size = 3.5)

# calculate avg. paces for each run type for annotation purposes
avg_pace_by_run_type <- all_activities_data %>%
  group_by(run_type) %>%
  summarise(avg_pace_per_minute = sum(sum(moving_time) / sum(distance_in_miles)) / 60) %>%
  mutate(avg_ppm_minute = floor(avg_pace_per_minute),
         avg_ppm_second = sprintf("%02d", round((avg_pace_per_minute - floor(avg_pace_per_minute)) * 60)),
         converted_avg_pace_per_miles = ms(paste0(avg_ppm_minute, ":", avg_ppm_second)))

# adding in annotations with avg. pace per run type
avg_pace_plot_with_race_annotations +
  geom_hline(data = avg_pace_by_run_type, aes(yintercept = converted_avg_pace_per_miles), color = "maroon", linetype = "dashed", size = 2) +
  geom_text(data = avg_pace_by_run_type, aes(x= c(as.POSIXct("2024-07-20"), as.POSIXct("2024-12-25"), as.POSIXct("2024-12-25")),
                                             y = converted_avg_pace_per_miles + 3,
                                             label = paste0("Avg. Pace: ", converted_avg_pace_per_miles)), inherit.aes = FALSE, size = 4, color = "maroon")

# create a calendar with all dates in between first and last day of running journey
all_dates <- tibble(date = seq(from = min(as.Date(all_activities_data$start_date_local)), to = max(as.Date(all_activities_data$start_date_local)), by = "days"))

# left joining all_dates with all_activities_data; any NAs in date (aka non-running date) will be marked with 0 miles for distance_in_miles
all_dates_data <- all_dates %>%
  left_join(all_activities_data, by = c("date" = "start_date_local_date_only")) %>%
  mutate(distance_in_miles = replace_na(distance_in_miles, 0),
         weekday = factor(wday(date, label = TRUE, week_start = 7), levels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))),
         week_start = floor_date(date, unit = "week"))

# creating Github-inspired chart tracking distance ran by day over time
all_dates_data %>%
  ggplot(aes(x = week_start, y = weekday, fill = distance_in_miles)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "white", high = "red", name = "Distance") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0,0)) +
  geom_text(aes(label = case_when(
    distance_in_miles == 0 ~ "",
    .default = as.character(round(distance_in_miles, digits = 2))
  )), fontface = "bold", size = 4) +
  geom_text(aes(label = case_when(is.na(run_type) ~ "", .default = run_type)), size = 2, vjust = -2) +
  geom_text(aes(label = date), size = 2, vjust = -4) +
  coord_equal(ratio = 8) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        panel.background = element_rect(fill = "floralwhite", color = NA),
        legend.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        text = element_text(size = 16),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(face = "bold", color = "black")) +
  ylab("Day of Week") +
  ggtitle(label = "Running Activity Frequency Heatmap",
          subtitle = "Each tile represents daily running distance (in miles). Blank/white tiles indicate no running activity/rest day") +
  labs(caption = "Source: Strava API")

# cumulative distance over time by month
all_dates_data %>%
  mutate(date_month = factor(format(floor_date(date, unit = "month"), "%B %Y"), 
                             levels = c("July 2024", "August 2024", "September 2024", "October 2024", "November 2024", "December 2024", "January 2025", "February 2025")),
         date_day = day(date)) %>%
  group_by(date_month) %>%
  mutate(cumulative_distance_in_miles = cumsum(distance_in_miles)) %>%
  select(date, date_month, date_day, cumulative_distance_in_miles) %>% 
  ggplot(aes(x = date_day, y = cumulative_distance_in_miles, color = date_month)) +
  geom_line(size = 3) +
  scale_color_manual(values = c("January 2025" = "#ADD8E6", "February 2025" = "#FFC0CB", "July 2024" = "#FF0000", "August 2024" = "#FFD700", "September 2024" = "#8B4513", 
                                "October 2024" = "#FF7518", "November 2024" = "#D2691E", "December 2024" = "#C0C0C0")) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        panel.background = element_rect(fill = "floralwhite", color = NA),
        legend.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(face = "bold", color = "black")) +
  ggtitle(label = "Cumulative Distance Over Time by Month",
          subtitle = "Each line represents the progression of running miles accumulated over the course of a month") +
  labs(x = "Day",
       y = "Cumulative Distance (in miles)",
       color = "Month/Year",
       caption = "Source: Strava API")

# total number of runs/distance in miles based on run distance
all_activities_data %>%
  group_by(distance_category, run_type) %>%
  summarise(count = n(), total_distance = sum(distance_in_miles)) %>%
  mutate(percentage = total_distance / sum(total_distance)) %>%
  ggplot(aes(x = distance_category, y = total_distance)) +
  geom_bar(stat = "identity", color = "black", fill = "#ffa07a") +
  geom_text(aes(label = paste0(run_type, "\n", count, " ", case_when(
    count == 1 ~ "run",
    .default = "runs"
  ), " (", round(total_distance, digits = 2), " miles)")), position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  theme(plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
        panel.background = element_rect(fill = "floralwhite", color = NA),
        text = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(face = "bold", color = "black")) +
  ggtitle(label = "Total Number of Runs/Distance Based on Run Distance",
          subtitle = "Overall bar length represents the total number of miles ran for all runs that fall within each distance category. Each bar is further broken out by run type") +
  labs(x = "Distance Category",
       y = "Total Distance (in miles)",
       caption = "Source: Strava API")

# the goal here is to grab splits data that available from the activities endpoint, provided you enter the corresponding activity_id; we capture all IDs in a vector to iterate through
activity_ids <- all_activities_data %>% select(id) %>% pull()

# create a function get_splits that takes an activity_id and access token and fetches the corresponding splits data and saves it to a data frame
get_splits <- function(activity_id, access_token) {
  url <- paste0("https://www.strava.com/api/v3/activities/", activity_id)
  
  response <- GET(url = url,
      config = access_token,
      query = list(per_page = 100))
  
  activity_data <- fromJSON(content(response, as = "text"))
  
  splits_standard <- activity_data$splits_standard %>%
    mutate(activity_id = activity_id)
  
  return(splits_standard)
}

# iterate through activity_ids vector and calling get_splits function; saving results in list which is then converted to data frame 
splits_data <- lapply(activity_ids, function(id) get_splits(id, token)) %>%
  bind_rows() %>%
  mutate(pace_per_minute = (moving_time / (distance / 1609)) / 60,
         ppm_minute = floor(pace_per_minute),
         ppm_second = sprintf("%02d", round((pace_per_minute - floor(pace_per_minute)) * 60)),
         converted_pace_per_miles = ms(paste0(ppm_minute, ":", ppm_second))) %>%
  left_join(all_activities_data %>% select(id, run_type), by = c("activity_id" = "id")) %>%
  filter(distance >= 300)

# creating data frame that captures avg. difference in pace between splits (calculated as difference in pace per mile between consecutive splits in activity (in seconds))
avg_difference_in_pace_between_splits_data <- splits_data %>%
  group_by(activity_id) %>%
  mutate(last_converted_pace_per_mile = lag(converted_pace_per_miles),
         difference_in_pace = as.numeric(converted_pace_per_miles - last_converted_pace_per_mile)) %>% 
  group_by(run_type, split) %>%
  summarise(avg_difference_in_pace_between_splits = mean(difference_in_pace, na.rm = TRUE),
            avg_difference_classification = ifelse(avg_difference_in_pace_between_splits < 0, "Negative", "Positive")) %>%
  filter(!is.na(avg_difference_in_pace_between_splits))

# graphing above data frame
avg_difference_in_pace_between_splits_data %>% 
  ggplot(aes(x = split, y = avg_difference_in_pace_between_splits)) +
  geom_bar(aes(fill = avg_difference_classification), stat = "identity", color = "black") +
  geom_text(aes(label = round(avg_difference_in_pace_between_splits, digits = 2)), vjust = ifelse(avg_difference_in_pace_between_splits_data$avg_difference_classification == "Positive", -0.5, 1.25)) +
  scale_y_continuous(name = "Avg. Difference in Time Between Splits (in seconds)", limits = c(-40, 40), breaks = seq(-40, 40, 10)) +
  facet_wrap(~run_type, ncol = 3, scales = "free_x") +
  theme(
    plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    text = element_text(size = 16),
    strip.background = element_rect(fill = "lightblue", color = NA),
    strip.text = element_text(face = "bold", color = "black"),
    plot.title = element_markdown(lineheight = 1.1),
    axis.text = element_text(face = "bold", color = "black"),
    legend.position = "none"
  ) +
  labs(
    fill = "",
    title = "<strong style='font-size:20pt'>How Much Faster/Slower Am I Running in Between (Mile) Splits?</strong><br>
           <span style='font-size:16pt'>Each bar represents the average difference in pace per mile between splits (<strong style='color:#00BFC4;'>blue</strong> bars = increase (slower pace) vs.<strong style='color:#F8766D;'>red</strong> bars = decrease (faster pace) in time between splits)  
    Further broken out by run type since not all runs are the same</span>",
    x = "Splits",
    caption = "Note: Splits with < 300 meters/0.186 miles were filtered out\nSource: Strava API"
  )
