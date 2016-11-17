#df <- read.csv("BANES_Historic_Car_Park_Occupancy.csv")

# Drop columns that we aren't interested in
#df <- df[ , !(names(df) %in% c("Easting", "Northing", "Location", "Description", "ID"))]


library(lubridate)

#df$LastUpdate <- dmy_hms(df$LastUpdate)
#df$DateUploaded <- dmy_hms(df$DateUploaded)

# Select a limited date range to work with

# All of 2015
# df2015 <- df [df$LastUpdate >= ymd("20150101") & df$LastUpdate < ymd("20160101"), ]
# 
# df2015 <- df2015[order(df2015$LastUpdate),]
# df2015$LastUpdate_date <- date(df2015$LastUpdate) 
# df2015$LastUpdate_dechr <- hour(df2015$LastUpdate) + minute(df2015$LastUpdate)/60 + second(df2015$LastUpdate)/3600
# df2015$LastUpdate_month <- month(df2015$LastUpdate) 
# df2015 <- transform(df2015, LastUpdate_month = factor(LastUpdate_month))
# df2015 <- transform(df2015, LastUpdate_date = factor(LastUpdate_date))


library(lattice)

plot.percentage.occupancy <- function(df, carpark_name, filename) {
  for (month in levels(df$LastUpdate_month)) {
    monthdf <- df[df$LastUpdate_month == month, ]
    southgate_df <- monthdf[monthdf$Name == carpark_name,]
    graph_title <- sprintf("%s in %s 2015", carpark_name, month(monthdf$LastUpdate[1], TRUE, FALSE))
    # png(sprintf("SouthGate_General_CP_2015_occupancy_month_%s.png", month(monthdf$LastUpdate[1], FALSE, FALSE)), width = 1000, height = 1000)
    # print(xyplot(Occupancy ~ LastUpdate_dechr | LastUpdate_date, data = southgate_df, layout = c(7, 5), 
    #              main=graph_title, xlab="Update time", ylab="Occupancy"))
    # dev.off()
    png(sprintf("%s_2015_percentage_month_%s.png", filename, month(monthdf$LastUpdate[1], FALSE, FALSE)), width = 1000, height = 1000)
    print(xyplot(Percentage ~ LastUpdate_dechr | LastUpdate_date, data = southgate_df, layout = c(7, 5), 
                 main=graph_title, xlab="Update time", ylab="Percentage of capacity"))
    dev.off()
  }

}

#plot.percentage.occupancy(df2015, "SouthGate General CP", "SouthGate_General_CP")

## Now clean up the data and plot again

# delete all values < -5%
df2015_cleaned <- df2015[df2015$Percentage >= -5, ]
#plot.percentage.occupancy(df2015_cleaned, "SouthGate General CP", "SouthGate_General_CP_cleaned")


plot.percentage.occupancy.day <- function(specified_day, df, carpark_name) {
  df <- df[df$LastUpdate >= specified_day & df$LastUpdate < specified_day + ddays(1) & df$Name == carpark_name, ]
  plot(df$LastUpdate, df$Percentage, type="l", xlab="Update time", ylab="Percentage of capacity")
  title(sprintf("%s on %s", carpark_name, specified_day))
}

plot.percentage.occupancy.day(ymd("20150614"), df2015, "SouthGate General CP")

# 1 Jan 2016
firstjan2016df <- df [df$LastUpdate >= ymd("20160101") & df$LastUpdate < ymd("20160102"), ]

summary(firstjan2016df, maxsum=length(levels(df$Name)))

southgate_firstjan2016 <- firstjan2016df[firstjan2016df$Name == "SouthGate General CP",]
southgate_firstjan2016 <- southgate_firstjan2016[order(southgate_firstjan2016$LastUpdate),]
plot(southgate_firstjan2016$LastUpdate, southgate_firstjan2016$Percentage, type="l", xlab="Update time", ylab="Percentage of capacity")
title("SouthGate General CP on 1st Jan 2016")

# # Nov 2015
# 
# nov2015df <- df [df$LastUpdate >= ymd("20151101") & df$LastUpdate < ymd("20151201"), ]
# nov2015df <- nov2015df[order(nov2015df$LastUpdate),]
# nov2015df$LastUpdate_date <- date(nov2015df$LastUpdate) 
# nov2015df$LastUpdate_dechr <- hour(nov2015df$LastUpdate) + minute(nov2015df$LastUpdate)/60 + second(nov2015df$LastUpdate)/3600
# 
# southgate_nov2015 <- nov2015df[nov2015df$Name == "SouthGate General CP", ]
# southgate_jan2106 <- transform(southgate_nov2015, LastUpdate_date = factor(LastUpdate_date))
# 
# print(xyplot(Percentage ~ LastUpdate_dechr | LastUpdate_date, data = southgate_nov2015, layout = c(7, 5), 
#              main="SouthGate General CP in Nov 2015", xlab="Update time", ylab="Percentage of capacity"))

# Dec 2015

# dec2015df <- df [df$LastUpdate >= ymd("20151201") & df$LastUpdate < ymd("20160101"), ]
# dec2015df <- dec2015df[order(dec2015df$LastUpdate),]
# dec2015df$LastUpdate_date <- date(dec2015df$LastUpdate) 
# dec2015df$LastUpdate_dechr <- hour(dec2015df$LastUpdate) + minute(dec2015df$LastUpdate)/60 + second(dec2015df$LastUpdate)/3600
# 
# southgate_dec2015 <- dec2015df[dec2015df$Name == "SouthGate General CP", ]
# southgate_jan2106 <- transform(southgate_dec2015, LastUpdate_date = factor(LastUpdate_date))
# 
# print(xyplot(Percentage ~ LastUpdate_dechr | LastUpdate_date, data = southgate_dec2015, layout = c(7, 5), 
#        main="SouthGate General CP in Dec 2015", xlab="Update time", ylab="Percentage of capacity"))

# All of Jan 2016
# jan2016df <- df [df$LastUpdate >= ymd("20160101") & df$LastUpdate < ymd("20160201"), ]
# jan2016df <- jan2016df[order(jan2016df$LastUpdate),]
# jan2016df$LastUpdate_date <- date(jan2016df$LastUpdate) 
# jan2016df$LastUpdate_dechr <- hour(jan2016df$LastUpdate) + minute(jan2016df$LastUpdate)/60 + second(jan2016df$LastUpdate)/3600
# 
# southgate_jan2016 <- jan2016df[jan2016df$Name == "SouthGate General CP", ]
# plot(southgate_jan2016$LastUpdate, southgate_jan2016$Percentage, type="l", xlab="Update time", ylab="Percentage of capacity")
# title("SouthGate General CP in Jan 2016")
# 
# southgate_jan2106 <- transform(southgate_jan2016, LastUpdate_date = factor(LastUpdate_date))
# 
# print(xyplot(Percentage ~ LastUpdate_dechr | LastUpdate_date, data = southgate_jan2016, layout = c(7, 5), 
#        main="SouthGate General CP in Jan 2016", xlab="Update time", ylab="Percentage of capacity"))


# plot(southgate_jan2016$LastUpdate_dechr, southgate_jan2016$Percentage, type="l", xlab="Update time", ylab="Percentage of capacity")
# title("SouthGate General CP in Jan 2016")

# Oct 2016
oct2016df <- df [df$LastUpdate >= ymd("20161001") & df$LastUpdate < ymd("20161031"), ]

# show summary listing counts for all car parks
summary(oct2016df, maxsum=length(levels(df$Name)))

# TODO
# Find car parks and days with percentage > 100 or < 0
# These should be cleaned up - ignored or replaced with average values for weekday/weekend

