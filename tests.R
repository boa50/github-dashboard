library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)

my_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

theme_boa <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colours$axis),
          axis.ticks = element_line(colour = my_colours$axis),
          axis.text = element_text(colour = my_colours$axis),
          axis.title = element_text(colour = my_colours$axis),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = NA)
    )
}

### Reference
# http://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html

names(df_commits) <- c("message", "executed_date", "repository")
df_commits$executed_date <- as.POSIXct(stringr::str_replace_all(df_commits$executed_date, "Z|T", " "))

df_commits <- df_commits %>%
  select(executed_date) %>% 
  mutate(commit_date = as.Date(executed_date)) %>% 
  count(commit_date)

min_date <- as.Date(min(df_commits$commit_date))
commit_date <- seq(min_date, by = "day", length = Sys.Date() - min_date + 1)

df <- as.data.frame(commit_date) %>% 
  left_join(df_commits, by = "commit_date") %>% 
  mutate(
    # commits = ifelse(is.na(n), 0, n),
    commits = n,
    weekday_number_rev = plyr::mapvalues(wday(commit_date),
                                         from = c(1:7),
                                         to = c(7:1)),
    weekday = wday(commit_date, label = TRUE, locale = "en_AU.UTF-8"),
    year = year(commit_date),
    month = month(commit_date, label = TRUE, locale = "en_AU.UTF-8")
  ) %>% 
  group_by(year) %>%
  mutate(
    week_test = any(epiweek(commit_date) > 50 & month == "Jan"),
    ### Fixing January dates that would show on the last column
    week = ifelse(week_test,
                  ifelse(epiweek(commit_date) > 50 & month == "Jan",
                         1,
                         epiweek(commit_date) + 1),
                  epiweek(commit_date)),
    ### Fixing December dates that would show on the first column
    week = ifelse(week == 1 & month == "Dec", max(week) + 1, week)
  ) %>% 
  ungroup()
  
# Variables to show only the last years
n_years <- 3
current_year <- as.integer(format(Sys.Date(), "%Y"))
first_year <- current_year - n_years + 1

# p <- df %>%
df %>%
  filter(year >= first_year) %>% 
  ggplot(aes(week, weekday, fill = commits, text = commit_date)) +
  geom_rect(aes(xmin = week - .35, xmax = week + .35,
                ymin = weekday_number_rev - .35, ymax = weekday_number_rev + .35))+
  facet_wrap(~year, ncol = 1) +
  scale_fill_gradient(low = "#cde3f9", high = "#1976d2", na.value = "#eeeeee") +  
  scale_y_discrete(expand = c(0, 0), 
                   limits = rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 54),
                     breaks = c(2.5, 6.8, 11.2, 15.5, 19.8, 24.1, 28.4, 32.7, 37, 41.3, 45.6, 49.9),
                     labels = month.abb) +
  labs(title = "Commits History") +
  theme_boa() +
  coord_fixed() +
  # guides(fill = guide_colourbar(title = NULL,
  #                               label = TRUE,
  #                               ticks = FALSE,
  #                               barwidth = 0.5,
  #                               barheight = 25)) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(colour = "black", 
                                margin = margin(t = 12, b = 12)),
    strip.background.x = element_rect(linetype = 1,
                                      linewidth = 7,
                                      colour = "white", 
                                      fill = "#e0e0e0")
  )

# ggplotly(p)
