# code for plotting the value differences by seat 
# note: this is for all seats, and is just going to present an average; the 
# visual will only show seats that have x and y coordinate values present

ticket_data <- 
  read.csv("~/MSBA/Team Garnet /trimmed clean dataset 03-12-2024.csv", 
           stringsAsFactors=TRUE)

new_dataset <- ticket_data 

# create a new column that shows differences (add it to new_dataset)
# calculation: ResoldTotalAmount - TotalSum
# so, if the difference > 0, it means that the resold value > current value 
# thus, difference > 0 means that seat is undervalued, and 
# difference < 0 means that seat is overvalued 
new_dataset$price_difference <- 
  new_dataset$ResoldTotalAmount - new_dataset$Total.Sum

# reduce this dataset. we only need columns showing section, row, seat, x and y 
# coords, resoldtotalamount, totalsum, and pricedifference 
seats <- new_dataset[c("SectionName", "RowName", "Seat", "X", "Y",
                       "ResoldTotalAmount", "Total.Sum", "price_difference")]
seats$Sect_Row_Seat <- paste(seats$SectionName, seats$RowName, seats$Seat, 
                             sep = "_")

nrow(seats)
# 84075 entries 

### ASSUMING: that each seat is indeed matched to only one set of X,Y coords 
### need to check this

# group by individual seat. shows the x and y coords as well as the average 
# price difference for each seat. 
library(dplyr)
sect <- seats %>% group_by(SectionName, RowName, Seat) %>% summarise(count = n())
price_difference_by_Seat <- seats %>% group_by(Sect_Row_Seat, X, Y) %>% 
  summarise(average_price_difference = mean(price_difference)) 
price_difference_by_Seat
plotting_values <- price_difference_by_Seat[!(is.na(price_difference_by_Seat$X)),]

# basic scatterplot of the seats 
plot(plotting_values$X, plotting_values$Y)
# the right side looks weird because that is the side of the stadium that the 
# screen is on (so there are no seats)
# but still lower sections aren't showing
# maybe because they are student seats?

# so look to see if the opposite side might lead to insight 
plot(plotting_values$X, plotting_values$Y)
abline(v = c(900, 1400), h = c(2300, 1200))
# look for the sections of seats where 900<= X <= 1400 and 1200<= Y <= 2300
opposite_side <- seats[seats$X >= 900 & seats$X<= 1400 & 
                          seats$Y >= 1400 & seats$Y<=2300,]
plot(opposite_side$X, opposite_side$Y)


# most basic color scheme: positive v negative differences: red means < 0, 
# green means >0; negative = group 1; positive = group 2 
plotting_values$color_id <- 
  ifelse(plotting_values$average_price_difference < 0, 1, 2)

library(ggplot2)
ggplot(data = plotting_values, aes(X, Y, color = factor(color_id))) + 
  geom_point() + 
  scale_color_manual(values = c("red", "green"), breaks = c(1, 2))


                     