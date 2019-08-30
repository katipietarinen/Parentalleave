#### VISUALIZATIONS of MATERNITY and PATERNITY leave lengths ####

setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

library(ggplot2)

install.packages("dplyr")
library(dplyr)

library(wesanderson)

# 1.1 THIS IS DATA READING & CLEANING 

# Open and clean maternity data
leavedata = read.csv("matleavedata.csv", stringsAsFactors=FALSE)  # read csv file

#Then change characters to numerics in Total.leave.as.full.rate.months (because it's stupidly as characters)
leavedata$Total.leave.as.full.rate.months = as.numeric(leavedata$Total.leave.as.full.rate.months)

#Change headers to shorter names
names(leavedata)[4] = "totalpaidmotherleave"
names(leavedata)[5] = "totalmotherleaveasfullpaidm"
allmotherleavemonths = select(leavedata,"Country","totalpaidmotherleave")
allmotherleavemonths = na.omit(allmotherleave)
motherleavefullpaidmonths = select(leavedata,"Country","totalmotherleaveasfullpaidm")
motherleavefullpaidmonths = na.omit(motherleavefullpaidmonths)

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

#2.1.1. Make a bar chart about maternity (fully paid months)
ggplot(motherleavefullpaidmonths, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", fill="darkgreen", position = "dodge")+ coord_flip() + geom_text(aes(label=leave), vjust=-7, size=2)+
  theme_minimal() + labs(title="Leave for mothers as fully paid months", x="Countries", y = "Months")

#2.1.2. Make a bar chart combining maternity and paternity (stacked)

#Give a common name to column about leave
names(motherleavefullpaidmonths)[2] = "leave"
names (dadsonlyasfullpaidmonthsdata)[2] = "leave"

# create an grp column for each of the above data.frames (Dads, Moms)
dadsonlyasfullpaidmonthsdata$LeaveType = ("Fathers_only")
motherleavefullpaidmonths$LeaveType = ("Mothers")

# merge data together
matpatfullypaid = rbind(dadsonlyasfullpaidmonthsdata, motherleavefullpaidmonths)

# set levels for grp column - which one should be displayed first within the group
# here, mothers followed by fathers

matpatfullypaid$LeaveType = factor(matpatfullypaid$LeaveType, levels=c("Fathers_only" , "Mothers"))

# make sure country is a factor (reorder levels if you have to)

matpatfullypaid$Country = factor(matpatfullypaid$Country)

# plot using ggplot

ggplot(matpatfullypaid, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", aes(fill=LeaveType), position = "stack") + coord_flip() +
  theme_minimal() + labs(title="Leave as fully paid months", x="Countries", y = "Months") + scale_fill_manual(values=wes_palette("Cavalcanti1"))


#2.1.2. Make a bar chart about maternity with both months and fully paid months (dodged)

# change column name to leave in allmotherleavemonths 
names(allmotherleavemonths)[2]= "leave"
allmotherleavemonths$LeaveType = "LeaveAvailableToMothers"
allmotherleavemonths = allmotherleavemonths [-(36),] 
allmotherleavemonths = allmotherleavemonths [-(44),]
allmotherleavemonths = allmotherleavemonths [-(43),]

# merge data together
motherleaves = rbind(allmotherleavemonths, motherleavefullpaidmonths)

# set levels for grp column - which one should be displayed first within the group
# here, mothers followed by fathers

motherleaves$LeaveType = factor(motherleaves$LeaveType, levels=c("LeaveAvailableToMothersAsFullyPaidMonths", "LeaveAvailableToMothers" ))

# make sure country is a factor (reorder levels if you have to)

motherleaves$Country = factor(motherleaves$Country)

# plot using ggplot

ggplot(motherleaves, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", width= 1, aes(fill=LeaveType), position = position_dodge(width=0.5)) + coord_flip() +
  theme_minimal() + labs(title = "Leave available to mothers", x="Countries", y = "Months")


#2.1.3. Make a bar chart about paternity (fully paid months)
ggplot(dadsonlyasfullpaidmonthsdata, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", fill ="yellow", position = "dodge")+ coord_flip() + theme_minimal() + labs(title="Leave only available to fathers as fully paid months", x="Countries", y = "Months")

#2.1.4. Make a bar chart about paternity with both months and fully paid months (dodged)

# change column name to leave in dadsonlymonthsdata 
names(dadsonlymonthsdata)[2]= "leave"
dadsonlymonthsdata$LeaveType = "DadsOnlyLeave"

# merge data together
dadleaves = rbind(dadsonlyasfullpaidmonthsdata, dadsonlymonthsdata)

# set levels for grp column - which one should be displayed first within the group
# here, mothers followed by fathers

dadleaves$LeaveType = factor(dadleaves$LeaveType, levels=c("DadsOnlyLeaveAsFullyPaidMonths","DadsOnlyLeave" ))

# make sure country is a factor (reorder levels if you have to)

dadleaves$Country = factor(dadleaves$Country)

# plot using ggplot

ggplot(dadleaves, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", width= 1, aes(fill=LeaveType), position = position_dodge(width=0.5)) + coord_flip() +
  theme_minimal() + labs(title = "Leave only available for fathers", x="Countries", y = "Months")




