### VISUALIZATIONS of MATERNITY and PATERNITY leave lengths ####

setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

library(ggplot2)

install.packages("dplyr")
library(dplyr)

library(wesanderson)

# 1.1 THIS IS DATA READING & CLEANING 

# Open and clean paternity data

# Data on leave only available for fathers both as months and converted to fully-paid months
dadsonlymonthsdata = read.csv("OECD_Patleave .csv", stringsAsFactors = FALSE) #OECD data on length of paid parental leave only available for fathers in months
names(dadsonlymonthsdata)[2] = "dadsonlymonths"
dadsonlymonthsdata = select(dadsonlymonthsdata, "Country", "dadsonlymonths")

dadsonlyasfullpaidmonthsdata = read.csv("OECD_Patleave .csv", stringsAsFactors = FALSE) # OECD data on length of parental leave only available for fathers converted to fully paid months 
names(dadsonlyasfullpaidmonthsdata)[3] = "dadsonlyfullypaidm"
dadsonlyasfullpaidmonthsdata = select(dadsonlyasfullpaidmonthsdata, "Country", "dadsonlyfullypaidm")
dadsonlyasfullpaidmonthsdata = na.omit(dadsonlyasfullpaidmonthsdata)

# 2. THIS IS VISUALIZATION

#Give a common name to column about leave
names (dadsonlyasfullpaidmonthsdata)[2] = "leave"
names(dadsonlymonthsdata)[2]= "leave"

# create an grp column for each of the above data.frames (Dads, Moms)
dadsonlyasfullpaidmonthsdata$LeaveType = ("Fully_paid_months")
dadsonlymonthsdata$LeaveType = "Length"

#2.1.4. Make a bar chart about paternity with both months and fully paid months (dodged)

# merge data together
dadleaves = rbind(dadsonlyasfullpaidmonthsdata, dadsonlymonthsdata)

# set levels for grp column - which one should be displayed first within the group
# here, mothers followed by fathers

dadleaves$LeaveType = factor(dadleaves$LeaveType, levels=c("Fully_paid_months","Length" ))

# make sure country is a factor (reorder levels if you have to)

dadleaves$Country = factor(dadleaves$Country)

# plot using ggplot

ggplot(dadleaves, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", width= 1, aes(fill=LeaveType), position = position_dodge(width=0.5)) + coord_flip() +
  theme_minimal() + labs(title = "Paid leave only available to fathers", x="Countries", y = "Months")
