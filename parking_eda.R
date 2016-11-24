# Exploratory Data Analysis of the Bath Historic Car Park Occupancy data
#

# Assume the data have been downloaded to the working directory
# Loading the data takes a while
df <- read.csv("BANES_Historic_Car_Park_Occupancy.csv")

# Drop columns that we aren't interested in at this time
df <- df[ , !(names(df) %in% c("Easting", "Northing", "Location", "Description", "ID"))]

# Library to help with dates
library(lubridate)

df$LastUpdate <- dmy_hms(df$LastUpdate)
df$DateUploaded <- dmy_hms(df$DateUploaded)

# Select a limited date range to work with

# Oct 2016
oct2016df <- df [df$LastUpdate >= ymd("20161001") & df$LastUpdate < ymd("20161031"), ]

# show summary listing counts for all car parks
print(summary(oct2016df, maxsum=length(levels(df$Name))))

#######

# All of 2015
df2015 <- df [df$LastUpdate >= ymd("20150101") & df$LastUpdate < ymd("20160101"), ]

# reorder by datetime
df2015 <- df2015[order(df2015$LastUpdate),]

# create columns for date and month and make them factors
df2015$LastUpdate_date <- date(df2015$LastUpdate)
df2015$LastUpdate_month <- month(df2015$LastUpdate)
df2015 <- transform(df2015, LastUpdate_month = factor(LastUpdate_month))
df2015 <- transform(df2015, LastUpdate_date = factor(LastUpdate_date))

# create a decimal hours column
df2015$LastUpdate_dechr <- hour(df2015$LastUpdate) + minute(df2015$LastUpdate)/60 + second(df2015$LastUpdate)/3600

# Draw graphs

library(lattice)

plot.percentage.occupancy <- function(df, carpark_name, filename) {
  for (month in levels(df$LastUpdate_month)) {
    monthdf <- df[df$LastUpdate_month == month, ]
    carpark_df <- monthdf[monthdf$Name == carpark_name,]
    graph_title <- sprintf("%s in %s %s", carpark_name, month(monthdf$LastUpdate[1], TRUE, FALSE), year(monthdf$LastUpdate[1]))
    
    png(sprintf("%s_percentage_%s_month_%s.png", 
                filename, year(monthdf$LastUpdate), month(monthdf$LastUpdate[1], FALSE, FALSE)), 
        width = 1000, height = 1000)
    print(xyplot(Percentage ~ LastUpdate_dechr | LastUpdate_date, data = carpark_df, layout = c(7, 5), 
                 main=graph_title, xlab="Update time", ylab="Percentage of capacity"))
    dev.off()
  }

}

plot.percentage.occupancy(df2015, "SouthGate General CP", "graphs/SouthGate_General_CP")

## Now clean up the data and plot again

# delete all values < -5%
df2015_cleaned <- df2015[df2015$Percentage >= -5, ]
plot.percentage.occupancy(df2015_cleaned, "SouthGate General CP", "graphs/SouthGate_General_CP_cleaned")


# function for plotting percentage occupancy on a single day
plot.percentage.occupancy.day <- function(specified_day, df, carpark_name, filename) {
  df <- df[df$LastUpdate >= specified_day & df$LastUpdate < specified_day + ddays(1) & df$Name == carpark_name, ]
  png(sprintf("%s_percentage_on_%s.png", filename, specified_day, width = 1000, height = 1000))
  plot(df$LastUpdate, df$Percentage, type="l", xlab="Update time", ylab="Percentage of capacity")
  title(sprintf("%s on %s", carpark_name, specified_day))
  dev.off()
}

# Plot some examples
plot.percentage.occupancy.day(ymd("20150614"), df2015_cleaned, "SouthGate General CP", "graphs/SouthGate_General_CP_cleaned")
plot.percentage.occupancy.day(ymd("20151107"), df2015_cleaned, "SouthGate General CP", "graphs/SouthGate_General_CP_cleaned")



