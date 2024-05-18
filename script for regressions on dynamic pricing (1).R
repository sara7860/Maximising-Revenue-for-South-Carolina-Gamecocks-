## script for looking at the regressions dealing with original sale date and 
### resale date 
library(ggplot2)

fixed.clean.data.04.02.2024 <- 
  read.csv("~/MSBA/Team Garnet /fixed clean data 04-02-2024.csv")

# remove SaleDate, EventName, EventDate, ResoldDate
my_data <- subset(fixed.clean.data.04.02.2024, 
                  select = -c(SaleDate, EventName,EventDate,ResoldDate))

# test for the first observations
# find the amount of days before an event a ticket was sold 
first_sale <- my_data$SaleDate.Fixed[1]
first_event <- my_data$EventDateFixed[1]
# these are character values, so we have to change them to dates
d1<- as.Date(first_sale, format = "%m/%d/%Y")
d2<- as.Date(first_event, format = "%m/%d/%Y")
d2 - d1
as.numeric(difftime(d2,d1))

# make sure all saledates are <= all event dates
sum(as.Date(my_data$SaleDate.Fixed, format = "%m/%d/%Y") > as.Date(my_data$EventDateFixed, format = "%m/%d/%Y")) 
# we are all good 


# create transformed version of dataset 
date_data <- my_data
date_data$SALEDATE <- as.Date(my_data$SaleDate.Fixed, format = "%m/%d/%Y")
date_data$EVENTDATE <- as.Date(my_data$EventDateFixed, format = "%m/%d/%Y")
date_data$RESOLDDATE <- as.Date(my_data$resoldDate.Fixed, format = "%m/%d/%Y")
date_data$sold_days_before <- as.numeric(difftime(date_data$EVENTDATE, date_data$SALEDATE, units = "days"))
date_data$resold_days_before <- as.numeric(difftime(date_data$EVENTDATE, date_data$RESOLDDATE, units = "days"))

# add in price difference 
date_data$price_difference <- date_data$ResoldTotalAmount - date_data$Total.Sum


# scatterplot of sold days till game and total sum 
ggplot(data = date_data, aes(sold_days_before, Total.Sum)) +
  geom_point()
# not much here 
# only look at where total sum is below 1000
ggplot(data = date_data[date_data$Total.Sum < 1000,], aes(sold_days_before, Total.Sum)) +
  geom_point()

noplancode <- date_data[date_data$PlanCode == "",]
#### only look at records without a plan code 
ggplot(data = noplancode[noplancode$Total.Sum<1000,], aes(sold_days_before, Total.Sum)) +
  geom_point()
# slight negative trend 


# scatterplot of sold days till game and price difference 
# hopefully the price difference shows some sort of pattern 
ggplot(data = noplancode[noplancode$Total.Sum < 1000,], aes(sold_days_before, price_difference)) +
  geom_point()


## scatterplot of total resold amount and days till event

ggplot(data = date_data, aes(resold_days_before, ResoldTotalAmount)) +
  geom_point()


## scatterplot of resold days before v resoldtotal amount but only where the difference is positive
ggplot(data = date_data[date_data$price_difference>=0,], aes(resold_days_before, ResoldTotalAmount)) +
  geom_point()

# finding the max and min of each section's resoldtotal amount 
minmax <- date_data %>% group_by(SectionName) %>% summarise(min = min(ResoldTotalAmount), max = max(ResoldTotalAmount))



#### LINEAR REGRESSION predicting resold total amount by days till game, section, 
# and sec/nonsec 
# create a column of SEC NON SEC 
date_data$CONFERENCE <- ifelse(date_data$EventName.Fixed %in%
                                 c("VS. COMMODORES","VS. WILDCATS","VS. AGGIES",
                                   "VS. MISSOURI TIGERS","VS. MS STATE BULLDOGS",
                                   "VS. GATORS","VS. AUBURN TIGERS","VS. VOLUNTEERS",
                                   "VS. GEORGIA BULLDOGS"), "SEC", "NOTSEC")
date_data$ClemsonGame <- ifelse(date_data$EventName.Fixed == "VS. CLEMSON TIGERS", "clemson", "not clemson")

mod <- lm(ResoldTotalAmount ~ resold_days_before + SectionName + SeasonHeaderName + CONFERENCE, data = date_data)
summary(mod)

mod <- lm(ResoldTotalAmount ~ resold_days_before + SectionName + SeasonHeaderName + CONFERENCE + ClemsonGame, data = date_data)
summary(mod)
plot(mod)

## predict 



#### do a TTEST to see if there is a significant difference between the positive price difference of home v away sections 

