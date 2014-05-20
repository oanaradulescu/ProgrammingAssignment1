getwd()
setwd("/Users/oana/Documents/Education/MOOC/Pop Datography | Telling Stories with Data/Data Science @ coursera/4 exploratory data analysis/exdata-data-NEI_data")

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

install.packages("dplyr")
library(dplyr)

# The bottleneck in most data analyses is the time it takes 
# for you to figure out what to do with your data, 
# and dplyr makes this easier by having individual functions that correspond 
# to the most common operations (group_by, summarise, mutate, filter, select and arrange). 
# Each function does one only thing, but does it well.

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

years <- group_by(NEI, year)
emissionsperyr <- summarise(years, totalemissions = sum(Emissions))

png(file = "totalemissionsfrompm2.5intheunitedstates.png")
with(emissionsperyr, plot(year, totalemissions))
title(main = "Total emissions from PM2.5 in the United States")
dev.off()

# TODO
# format y-axis ticks
# format y-axis label
# better align the x-axis values (1999, 2002, 2005, 2008) with the x-axis ticks
# prettify: make the point outline bolder, improve typography
# experiment with line plot vs. scatterplot

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

emissionsperyr.bc <- filter(NEI, fips == "24510") %.%
    group_by(year) %.%
    summarise(totalemissions = sum(Emissions))

png(file = "totalemissionsfrompm2.5inbaltimore.png")
with(emissionsperyr.bc, plot(year, totalemissions))
title(main = "Total emissions from PM2.5 in Baltimore City, Maryland")
dev.off()

library(ggplot2)

ggplot(emissionsperyr.bc, aes(x=year, y=totalemissions)) + geom_point()
qplot(year, totalemissions, data=emissionsperyr.bc)

qplot(year, totalemissions, data=emissionsperyr.bc, geom="line")
ggplot(emissionsperyr.bc, aes(x=year, y=totalemissions)) + geom_line()

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

emissionsperyrntype.bc <- filter(NEI, fips == "24510") %.%
    group_by(year, type) %.%
    summarise(totalemissions = sum(Emissions))


ggplot(emissionsperyrntype.bc, aes(x=year, y=totalemissions, color=type)) + geom_line() + geom_point()

# TODO
# add title
# format axis labels
# format axis ticks
# improve legend: order, typography
# add a horizontal line to help answer emissions decrease/increase for point sources

# 4. Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?

str(NEI)
str(SCC)

SCC %.%
    group_by(Data.Category) %.%
    summarise(unique(Data.Category))

unique(SCC$Data.Category)
scc.names <- unique(SCC$Short.Name)


rows <- grep("[Cc][Oo][Aa][Ll]", SCC$Short.Name)
coal.scc <- SCC[rows,"SCC"]

emissionsperyr.coal <- NEI %.%
    filter(SCC %in% coal.scc) %.%
    group_by(year) %.%
    summarise(coalemissions = sum(Emissions))

png(file = "emissionsfrompm2.5-coal.png")
ggplot(emissionsperyr.coal, aes(year, coalemissions)) + geom_point() + geom_line()
dev.off()


# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

emissionsperyr.bc.motorvehicles <- NEI %.%
    filter(type == "ON-ROAD", fips == "24510") %.%
    group_by(year) %.%
    summarise(onroademissions = sum(Emissions))

png(file = "emissionsfrompm2.5-bc-motorvehicles.png")
ggplot(emissionsperyr.bc.motorvehicles, aes(year, onroademissions)) + geom_point() + geom_line()
dev.off()

# 6. Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

