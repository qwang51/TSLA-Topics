setwd('~/Downloads/topics')

library(ggplot2)
library(highcharter)
library(plotly)

# read table
topics <- read.table('topics.txt', sep = '\t', stringsAsFactors = FALSE, header=TRUE)
dist <- read.table('document_topic_distributions.txt', sep = '\t', stringsAsFactors = FALSE,
                    fill=TRUE, header = TRUE)

# check and remove na in [dist]
# There are 303 rows where all topics has value of NA, remove them all
dist <- dist[complete.cases(dist),]

# change datetime to data type Date in [dist]
dist$datetime <- as.Date(format(as.POSIXct(dist$datetime,format="%m/%d/%Y %H:%M"), '%Y-%m-%d'))

# new data frame dist_by_date
require(reshape2)
dist_melt <- melt(dist[,-(1:2)], id='datetime')
dist_by_date <- dcast(dist_melt, datetime ~ variable, mean)

paste(names(dist_by_date[-1]), topics$words)

dist_by_date_long <- melt(dist_by_date, id='datetime')
dist_by_date_long$variable <- as.character(dist_by_date_long$variable)

topics$topic <- paste('t', as.character(topics$topic), sep='')


new_name <- dist_by_date
n <- paste(names(dist_by_date[-1]), topics$words)
names(new_name) <- c('datetime', n)

names_long <- melt(new_name, id='datetime')
names_long$variable <- as.character(names_long$variable)

saveRDS(names_long, file='new_name.rds')


saveRDS(dist_by_date, file='data.rds')
saveRDS(dist_by_date_long, file='data_long.rds')
saveRDS(topics, file='topics.rds')



# Create word counts table
words <- unlist(strsplit(unlist(topics[1,2]), "[0-9,|]+"))
count <- as.numeric(unlist(strsplit(unlist(topics[1,2]), "[^0-9]+")))
count <- count[!is.na(count)]

words1 <- unlist(strsplit(unlist(topics[2,2]), "[0-9,|]+"))
count1 <- as.numeric(unlist(strsplit(unlist(topics[2,2]), "[^0-9]+")))
count1 <- count[!is.na(count1)]

t0 <- as.data.frame(count, colnames <- words)
t1 <- as.data.frame(count1, colnames <- words1)

saveRDS(t0, file='t0.rds')

# graphs
g <- ggplot(dist_by_date_long, aes(x=datetime, y=value, colour=variable))
gg <- g + geom_line()
#ggplotly(gg)


# Working highstock chart

highchart(type = 'stock') %>%
  hc_add_series_times_values(dist_by_date$datetime,
                             dist_by_date$t0,
                             name = "t0") %>% 
  hc_add_series_times_values(dist_by_date$datetime,
                             dist_by_date$t1,
                             name = "t1") %>% 
  hc_add_theme(hc_theme_google())


# dygraph

z <- xts(dist_by_date[,2:10], order.by = dist_by_date$datetime)

dygraph(z[,3:5]) %>% 
  dyRangeSelector() %>% 
  dyCrosshair(direction = "vertical") %>% 
  dyOptions(colors = RColorBrewer::brewer.pal(length(colnames(z)), "Paired"),
            #axisLineWidth = 2,
            gridLineWidth = 1.2,
            gridLineColor = '#f2f2f2',
            axisLineWidth = 1.2,
            axisLineColor = '#f2f2f2',
            strokeWidth = 2) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.7,
              highlightSeriesOpts = list(strokeWidth = 2.5),
              hideOnMouseOut = TRUE) %>%
  dyLegend(show='onmouseover',
           width=90,
           labelsSeparateLines=TRUE, 
           hideOnMouseOut = FALSE) 


# Working
hc_opts <- list()
hc_opts$title <- list(text = "This is a title", x = -20)
hc_opts$xAxis <- list(dist_by_date$datetime)

hc_opts$series <- list(list(name='t0',
                            data=dist_by_date$t0))

hc_opts$series <- append(hc_opts$series,
                         list(list(
                           name='t1',
                           data=dist_by_date$t1)))

highchart(type = 'stock', hc_opts)



# plotly

plot_ly(dist_by_date_long[1:600,], x=datetime, y=value, group=variable) %>%
  layout(title = 'Plotly',
         xaxis = list(
           rangeselector = list(
             buttons = list(
               list(
                 count = 3,
                 label = "3 mo",
                 step = "month",
                 stepmode = "backward"),
               list(
                 count = 6,
                 label = "6 mo",
                 step = "month",
                 stepmode = "backward"),
               list(
                 count = 1,
                 label = "1 yr",
                 step = "year",
                 stepmode = "backward"),
               list(
                 count = 1,
                 label = "YTD",
                 step = "year",
                 stepmode = "todate"),
               list(step = "all"))),
           
           rangeslider = list(type = "date")))





# feature selection R - entrophy - Caret packege

