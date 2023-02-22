library(tidyquant)
library(plotly)
library(ggplot2)
library(dplyr)
library(plyr)

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

### References
# http://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html

### Loading the data
amznStock <- as.data.frame(tidyquant::tq_get(c("AMZN"),get="stock.prices")) # get data using tidyquant
amznStock <- amznStock[year(amznStock$date) > 2020, ]

### Creating the heatmap
amznStock$weekday = as.POSIXlt(amznStock$date)$wday #finding the day no. of the week
amznStock$weekdayf<-factor(amznStock$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE) # converting the day no. to factor

amznStock$monthf<-factor(month(amznStock$date),levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) # finding the month

amznStock$yearmonth<- factor(as.yearmon(amznStock$date)) # finding the year and the month from the date. Eg: Nov 2018

amznStock$week <- as.numeric(format(amznStock$date,"%W")) # finding the week of the year for each date

amznStock<-ddply(amznStock,.(yearmonth),transform,monthweek=1+week-min(week)) # normalizing the week to start at 1 for every month

p <- amznStock %>%
# amznStock %>%
  ggplot(aes(week, weekdayf, fill = adjusted, text = date)) + 
  geom_tile(height=0.7, width=0.7,) +
  facet_wrap(~year(date), ncol = 1, scales = "fixed") +
  # facet_wrap(~year(date), ncol = 1) + 
  scale_fill_gradient(low="red", high="green") +  
  scale_y_discrete(expand = c(0, 0), limits = rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Time-Series Calendar Heatmap: AMZN Stock Prices",
    x = "Week",
    y = "",
    fill = "Price"
  ) +
  theme_boa()

ggplotly(p)
