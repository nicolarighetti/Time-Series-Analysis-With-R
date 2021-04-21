########################
# Plot time series ####
########################

##############
# PLOT.TS ####
##############

data("AirPassengers")

plot.ts(AirPassengers)

plot.ts(AirPassengers, 
        main = "PASSENGERS",
        xlab = "1949-1960 (monthly data)",
        ylab = "Passengers (1000's)",
        col = "violetred3", 
        lwd=5)

colors()

# create "toy" time series with the same lenght of the AirPassenger one
AirPassengers_2 <- AirPassengers + 100
AirPassengers_3 <- AirPassengers + 200

AirPassengers_multi <- ts.union(AirPassengers, AirPassengers_2, AirPassengers_3)

# Plot two time series together
plot.ts(AirPassengers_multi, 
        main = "Three time series",
        xlab = "TIME", ylab = "VALUES",
        col = c("blue", "red", "black"), 
        lwd=c(1, 1, 1), lty=c(1, 4, 5),
        plot.type = "single")

# Plot time series in different panels
plot.ts(AirPassengers_multi, 
        main = "Three time series",
        xlab = "TIME", ylab = "VALUES",
        col = "blue", 
        lwd=4,
        plot.type = "multiple")

# Display the series on three columns 
plot.ts(AirPassengers_multi, 
        main = "Two time series",
        xlab = "TIME", ylab = "VALUES",
        col = "blue", 
        lwd=4,
        plot.type = "multiple",
        nc=3)

# Focus on a specific time period
window(AirPassengers, start=c(1950, 01), end=c(1950, 12))

plot.ts(window(AirPassengers, start=c(1950, 01), end=c(1959,12)), 
        main = "PASSENGERS ('50s)",
        xlab = "1950-1959 (monthly data)",
        ylab = "Passengers (1000's)",
        col = "violetred3", 
        lwd=5)

plot.ts(window(AirPassengers, start=c(1950, 01), end=c(1953,12)), 
        main = "PASSENGERS (1950-1953)",
        xlab = "1950-1953 (monthly data)",
        ylab = "Passengers (1000's)",
        col = "blue", 
        lwd=5)

# Resample the data using a different frequency
plot.ts(window(AirPassengers, start=c(1950, 01), end=c(1954,12), frequency = 4), 
        main = "PASSENGERS (1950-1955) - QUARTERLY DATA",
        xlab = "1950-1955 (Quarterly data)",
        ylab = "Passengers (1000's)",
        col = "violetred3", 
        lwd=5)

# Apply the window function to more than one time series
plot.ts(window(AirPassengers_multi, start=c(1950, 01), end=c(1954,12), frequency = 4), 
        main = "PASSENGERS (1950-1955) - QUARTERLY DATA",
        xlab = "1950-1955 (Quarterly data)",
        ylab = "Passengers (1000's)",
        col = "violetred3", 
        lwd=5)

# Function help
?plot.ts

###############
# PLOT.XTS ####
###############
library(xts)

xts_format <- xts(x = as.vector(AirPassengers), 
                  order.by = zoo::as.yearmon(time(AirPassengers)), 
                  frequency = 12)

# as.xts
AirPassengers_xts <- as.xts(AirPassengers)

plot.xts(AirPassengers_xts,
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         col = "steelblue2", 
         lwd=5)

# Plot more than one xts time series
AirPassengers_multi_xts <- as.xts(AirPassengers_multi)

plot.xts(AirPassengers_multi_xts,
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = c("blue", "orange", "black"),
         multi.panel = T)

plot.xts(AirPassengers_multi_xts,
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = c("blue", "orange", "black"),
         multi.panel = F)

# Subset the data
plot.xts(AirPassengers_multi_xts["1950-01/1954-12"], 
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = c("blue", "orange", "black"),
         multi.panel = F)

plot.xts(AirPassengers_xts["1950-01/1951-01"], 
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = c("blue", "orange", "black"),
         multi.panel = F)

# Change the frequency of the data
periodicity(AirPassengers_xts)

yearly_xts <- to.period(AirPassengers_xts, period="years")
periodicity(yearly_xts)

plot.xts(to.period(AirPassengers_xts, period="years"),
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = c("blue", "orange", "black", "red"),
         multi.panel = F)

plot.xts(to.period(AirPassengers_xts, period="years")[,2],
         main = "PASSENGERS",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = "blue",
         multi.panel = F)

# Re-sample and calculate the average
index_years <- endpoints(AirPassengers_xts, on = "year")
AirPassengers_xts_year_avg <- period.apply(AirPassengers_xts, INDEX=index_years, FUN=mean)

plot.xts(AirPassengers_xts_year_avg,
         main = "PASSENGERS (Year Average)",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = "blue",
         multi.panel = F)

# Resample and calculate the median
AirPassengers_xts_year_median <- period.apply(AirPassengers_xts, INDEX=index_years, FUN=median)

plot.xts(AirPassengers_xts_year_median,
         main = "PASSENGERS (Year Average)",
         ylab = "Passengers (1000's)", 
         lwd=5, lty=1,
         col = "blue",
         multi.panel = F)


#############
# GGPLOT ####
#############
library(tidyverse)

elections_news <- read_csv("data/elections-stories-over-time-20210111144254.csv")
elections_news$date <- as.Date(elections_news$date)

# simple plot
ggplot(elections_news) +
        geom_line(aes(x = date, y = count), color = "snow4", size = 0.25) +
        ylab("News Articles") +
        xlab("Date") +
        labs(title = "Time Series of News Articles on Elections",
             subtitle = "Data from MediaCloud",
             caption = "Data Analysis II") +
        theme_minimal(base_size = 20)

# Plot two (or more) time series 
#install.packages("gridExtra")
library(gridExtra)

p1 <- ggplot(elections_news) +
        geom_line(aes(x = date, y = ratio), col = "black", size = 0.5) +
        ylab("News Articles (ratio)") +
        xlab("Date") +
        ggtitle("MediaCloud Data on Elections (Daily)") 

p2 <- ggplot(elections_news) +
        geom_line(aes(x = date, y = count), col="red", size=0.5) +
        ylab("News Articles (count)") +
        xlab("Date") +
        ggtitle("MediaCloud Data on Elections (Daily)") 

grid.arrange(p1, p2)

# Plot two or more time series in the same plot
elections_news <- elections_news %>% # create fake data with "mutate"
        mutate(time_series_data_2 = count*2,
               time_series_data_3 = count*4)

ggplot(elections_news) +
        geom_line(aes(x = date, y = count), col = "black", size = 0.5) +
        geom_line(aes(x = date, y = time_series_data_2), col = "blue", size = 0.5) +
        geom_line(aes(x = date, y = time_series_data_3), col="red", size=0.5) +
        ylab("") +
        xlab("Date") +
        ggtitle("MediaCloud Data on Elections (Daily)") 

# Focus on a shorter time window
elections_news %>%
        filter(date >= "2016-01-01" & date < "2019-01-01") %>%
        ggplot() +
        geom_line(aes(x = date, y = count), col = "black", size = 0.5) +
        scale_x_date(breaks="year", date_labels ="%Y") +
        theme(axis.text.x = element_text(angle = 90, hjust=1)) +
        ylab("News Articles (Ratio)") +
        xlab("Day") +
        labs(title="MediaCloud Data on Elections (Monthly) - 2016") +
        theme_bw()

# Add annotations
ggplot(elections_news) +
        geom_line(aes(x = date, y = count), col = "grey50", size = 0.25) +
        ylim(c(0, 15000)) +
        # 1 EVENT
        annotate("label", x = as.Date("2018-11-01"), y = 14500, 
                 label = "Midterm Elections\nNovember 2018", color = "white", fill="orange", fontface="bold", size=3) +
        # add a line. You can also use an arrow by adding in geom_segment: 
        # arrow = line(length = unit(0.2, "cm"), ends = "last") 
        geom_segment(aes(x = as.Date("2018-11-01"), xend = as.Date("2018-11-01"), y = 0, yend = 14500), 
                     color = "orange", size = 0.2, linetype = 1) +
        # 2 EVENT
        annotate("label", x = as.Date("2019-05-01"), y = 12000, 
                 label = "Pennsylvania Elections\nMay 2019", color = "white", fill="orange", fontface="bold", size=3) +
        geom_segment(aes(x = as.Date("2019-05-01"), xend = as.Date("2019-05-01"), y = 0, yend = 12000), 
                     color = "orange", size = 0.2, linetype = 1) +
        # 3 EVENT
        annotate("label", x = as.Date("2016-11-01"), y = 12000, 
                 label = "Presidential Elections\nNovember 2016", color = "white", fill="orange", fontface="bold", size=3) +
        geom_segment(aes(x = as.Date("2016-11-01"), xend = as.Date("2016-11-01"), y = 0, yend = 12000), 
                     color = "orange", size = 0.2, linetype = 1) +
        # 4 EVENT
        annotate("label", x = as.Date("2020-11-01"), y = 12000, 
                 label = "Presidential\nElections\nNovemeber\n2020", color = "white", fill="orange", fontface="bold", size=3) +
        geom_segment(aes(x = as.Date("2020-11-01"), xend = as.Date("2020-11-01"), y = 0, yend = 12000), 
                     color = "orange", size = 0.2, linetype = 1) +
        ylab("News Articles") +
        xlab("Date") +
        labs(title = "MediaCloud Data on Elections (Monthly)",
             subtitle = "Peaks annotated with relevant political events",
             caption = "Advanced Data Analysis
                  University of Vienna") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(face = "italic")) +
        theme_gray()
