library(plotly)
library(ggplot2)
library(dplyr)

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
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)
    )
}

### Reference
# http://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html

names(df_commits) <- c("message", "executed_date", "repository")
df_commits$executed_date <- as.POSIXct(stringr::str_replace_all(df_commits$executed_date, "Z|T", " "))

df <- df_commits %>%
  select(executed_date) %>% 
  mutate(executed_date = as.Date(executed_date)) %>% 
  count(executed_date) %>% 
  mutate(weekday_number = wday(executed_date),
         weekday = wday(executed_date, label = TRUE, locale = "en_AU.UTF-8"),
         year = year(executed_date),
         month = month(executed_date, label = TRUE, locale = "en_AU.UTF-8"),
         week = week(executed_date)
        )

# p <- df %>%
df %>%
  ggplot(aes(week, weekday, fill = n, text = executed_date)) + 
  geom_tile(height=0.7, width=0.7,) +
  facet_wrap(~year, ncol = 1, scales = "fixed") +
  # facet_wrap(~year(date), ncol = 1) + 
  scale_fill_gradient(low="#daeafa", high="#1976d2") +  
  scale_y_discrete(expand = c(0, 0), limits = rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 53)) +
  labs(
    title = "Time-Series Calendar Heatmap: AMZN Stock Prices",
    x = "Week",
    y = "",
    fill = "Number of commits"
  ) +
  theme_boa()

# ggplotly(p)
