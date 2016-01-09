
# Directory
root <- getwd()

# Helpers
source('code/helpers.R')

options(scipen=999)

library(readxl)
library(dplyr)
library(waffle)
library(RColorBrewer)
library(reshape2)
library(Hmisc)
df2 <- read_excel('data/thegardenproducereport14-15.xlsx',
                 skip = 1)

df <- read_excel('data/Useage615.xlsx', skip = 1)

# Add the January 2016 dump to the previous stuff (df)
setwd('data/january_2016/')
files <- dir()
results_list <- list()
for (i in 1:length(files)){
  temp <- read_excel(files[i],
                     skip = 1)
  results_list[[i]] <- temp
}
new_data <- do.call('rbind', results_list)
# Change names
names(new_data) <- 
  c('item', 'number', 'month', 'uom', 'qty', 'state')

# Add a blank column for price (since we don't have it)
new_data$price <- rep(NA, nrow(new_data))
# Order appropriately
new_data <- 
  new_data[,c('item', 'number', 'month', 'uom', 'qty', 'price', 'state')]


# BACK TO OLD DATA
setwd(root)

# Change names of old data
names(df) <- names(df2) <- 
  c('item', 'number', 'month', 'uom', 'qty', 'price', 'state')

# Combine
df <- rbind(df, df2, new_data)

# Get rid of grand total
df <- df[which(is.na(df$item) | df$item != 'Grand Total'),]

# Get right names in item / number / state (de-aggregate)
for (i in 1:nrow(df)){
  while(is.na(df$item[i])){
    new_row <- i-1
    df$item[i] <- df$item[new_row]
  }
  while(is.na(df$number[i])){
    new_row <- i-1
    df$number[i] <- df$number[new_row]
  }
  #print(i)
}

# Need to fix dates!!!

# Fix the funky date stuff (get rid of month headers and take previous rows)
df$is_date <- grepl("^[[:digit:]]",df$month)
df$date <- ifelse(df$is_date,
                  format(as.Date(df$month, format = '%d-%B-%y'), '%Y-%m-%d'), 
                  NA)
df$date <- as.Date(df$date)

# repair dates and states
for (i in 2:nrow(df)){{
  while(is.na(df$date[i])){
    new_row <- i-1
    df$date[i] <- df$date[new_row]
  }
  while(is.na(df$state[i])){
    new_row <- i-1
    df$state[i] <- df$state[new_row]
  }
  #print(i)
}}

# Get rid of the aggregated month rows
df <- df[which(!is.na(df$qty)),
         c('date', 'item', 'number', 'uom', 'qty', 'price', 'state')]

# Clean up state names
df$state <- toupper(df$state)
df$state[which(df$state == 'N/A')] <- NA
df$state[which(df$state == 'FL (ORANGE)')] <- 'FLORIDA'
df$state[which(df$state == 'COSATA RICA')] <- 'COSTA RICA'
df$state[which(df$state == 'AZ')] <- 'ARIZONA'
df$state[which(df$state == 'FL')] <- 'FLORIDA'
df$state[which(df$state == 'MI')] <- 'MICHIGAN'
df$state[which(df$state == 'ID')] <- 'IDAHO'
df$state[which(df$state == 'GA')] <- 'GEORGIA'
df$state[which(df$state == 'MX')] <- 'MEXICO'
df$state[which(df$state == 'NC')] <- 'NORTH CAROLINA'
df$state[which(df$state == 'NJ')] <- 'NEW JERSEY'
df$state[which(df$state == 'PA')] <- 'PENNSYLVANIA'
df$state[which(df$state %in% c('SC', 'S. CAROLINA'))] <- 'SOUTH CAROLINA'
df$state[which(df$state == 'TN')] <- 'TENNESSEE'
df$state[which(df$state == 'TX')] <- 'TEXAS'
df$state[which(df$state == 'WA')] <- 'WASHINGTON'
df$state[which(df$state == 'CA')] <- 'CALIFORNIA'
df$state[which(df$state == 'CO')] <- 'COLORADO'



# Give florida / non-florida column
df$florida <- ifelse(df$state == 'FLORIDA', 'Florida', 'Non-Florida')

# Get rid of the 30 dollars with no state associated
# df <- df[which(!is.na(df$state)),]


#####
# WHAT PERCENTAGE OF OVERALL PRODUCE IS FLORIDA PRODUCE?
#####
simple <- df %>%
  group_by(florida) %>%
  summarise(dollars = sum(price, na.rm = TRUE))
simple$color <- adjustcolor(ifelse(simple$florida == 'Florida', 'blue', 'black'), alpha.f = 0.6)
complicated <- df %>%
  group_by(state) %>%
  summarise(dollars = sum(price, na.rm = TRUE))
complicated <- complicated[order(complicated$dollars),]
complicated$color <- adjustcolor(ifelse(complicated$state == 'FLORIDA', 'blue', 'black'), alpha.f = 0.6)

#####
# VISUALIZE
#####
pdf('visuals.pdf', height = 11, width = 8.5)
par(mar = c(7,6,5,4))
par(mfrow = c(2,1))

bp <- barplot(simple$dollars,
              names.arg = simple$florida,
              las = 1,
              ylab = NA,
              xlab = 'Place of origin of product',
              col = simple$color,
              border = NA,
              ylim = c(0, max(simple$dollars, na.rm = TRUE) * 1.2))
mtext(side = 2, line = 4, 'Dollars spent')
text(x = bp[,1],
     y = simple$dollars,
     labels = paste0('$', round(simple$dollars / 1000, digits = 1), 'K'),
     pos = 3)
text(x = bp[,1],
     y = simple$dollars,
     labels = paste0(round(simple$dollars / sum(simple$dollars) * 100, digits = 1), '%'),
     pos = 1)
title(main = 'Where do our food dollars go?')


#####
par(mar = c(9,6,5,4))
bp <- barplot(complicated$dollars,
              #names.arg = complicated$state,
              las = 1,
              ylab = NA,
              xlab = NA,
              col = complicated$color,
              border = NA,
              ylim = c(0, max(complicated$dollars, na.rm = TRUE) * 1.2))
mtext(side = 2, line = 4, 'Dollars spent')
mtext(side = 1, line = 6, 'Place of origin of product')
axis(side = 1,
     at = bp[,1],
     labels = complicated$state,
     las = 3,
     cex.axis = 0.7,
     tick = FALSE)

text(x = bp[,1],
     y = complicated$dollars,
     labels = paste0('$', round(complicated$dollars / 1000, digits = 1), 'K'),
     pos = 3,
     cex = 0.4)
text(x = bp[,1],
     y = complicated$dollars,
     labels = paste0(round(complicated$dollars / sum(complicated$dollars) * 100, digits = 1), '%'),
     pos = 1,
     cex = 0.4)
title(main = 'Where do our food dollars go?\n(detailed)')


##
vec <- round(simple$dollars/1000)
names(vec) <- simple$florida
waffle(vec, colors = c('blue', 'grey'),
       xlab = 'Each square = $1,000',
       size = 0.3,
       title = 'Where do our food dollars go?',
       rows = 15)

vec <- round(complicated$dollars/1000)
names(vec) <- complicated$state
vec <- rev(vec)
cols <- colorRampPalette(brewer.pal(8, 'Set2'))(length(vec))
cols[which(names(vec) == 'FLORIDA')] <- 'blue'
waffle(vec, xlab = 'Each square = $1,000',
       colors = cols,
       size = 0.3,
       title = 'Where do our food dollars go?',
       rows = 10)

#####
# WRITE CLEANED SPREADSHEET
#####
write.csv(df, 'data/cleaned_data.csv')


#####
# TIME SERIES
#####
library(maps)
usa <- map('state', plot = FALSE)
states <- toupper(unique(usa$names))
states <- sub(':MAIN', '', states)
df$place <- ifelse(df$state == 'FLORIDA',
                   'Florida',
                   ifelse(df$state %in% states,
                          'Other states',
                          'Foreign'))

# Get time series by place and month
df$year <- as.numeric(format(df$date, '%Y'))
df$month <- as.numeric(format(df$date, '%m'))
temp <- df %>%
  group_by(place, year, month) %>%
  summarise(dollars = sum(price, na.rm = TRUE))

# fake month
temp$year_month <- paste0(temp$year, '-', temp$month)
temp$val <- as.numeric(factor(temp$year_month))

# Plot
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
places <- unique(temp$place)
cols <- adjustcolor(c('blue', 'orange', 'red'), alpha.f = 0.6)
for (i in 1:length(places)){
  sub_temp <-temp[which(temp$place == places[i]),]
  if(i == 1){
    plot(sub_temp$val,
         sub_temp$dollars,
         type = 'n',
         ylim = c(0, 160000),
         xaxt = 'n',
         las = 1,
         xlab = 'Month',
         ylab = NA)
    mtext(side = 2, line = 4, 'Dollars')
    axis(side = 1,
         at = sub_temp$val,
         labels = months[sub_temp$month])
  }
  lines(sub_temp$val,
        sub_temp$dollars,
        col = cols[i],
        lwd = 7)
}

legend('topleft',
       lty = 1,
       lwd = 5,
       col = cols,
       legend = places)
title(main = 'Product place of origin by month')


#####
# FLORIDA VS OTHER
#####
df$place <- ifelse(df$state == 'FLORIDA',
                   'Florida',
                   'Non-Florida')

# Get time series by place and month
df$year <- as.numeric(format(df$date, '%Y'))
df$month <- as.numeric(format(df$date, '%m'))
temp <- df %>%
  group_by(place, year, month) %>%
  summarise(dollars = sum(price, na.rm = TRUE))

# fake month
temp$year_month <- paste0(temp$year, '-', temp$month)
temp$val <- as.numeric(factor(temp$year_month))

# Plot
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
places <- unique(temp$place)
cols <- adjustcolor(c('blue', 'red'), alpha.f = 0.6)
for (i in 1:length(places)){
  sub_temp <-temp[which(temp$place == places[i]),]
  if(i == 1){
    plot(sub_temp$val,
         sub_temp$dollars,
         type = 'n',
         ylim = c(-5000, 200000),
         xaxt = 'n',
         las = 1,
         xlab = 'Month',
         ylab = NA)
    mtext(side = 2, line = 4, 'Dollars')
    axis(side = 1,
         at = sub_temp$val,
         labels = months[sub_temp$month])
  }
  lines(sub_temp$val,
        sub_temp$dollars,
        col = cols[i],
        lwd = 7)
  
  text(x = sub_temp$val,
       y = sub_temp$dollars,
       col = cols[i],
       cex = 0.6,
       pos = ifelse(i == 2, 3, i),
       labels = paste0('$',round(sub_temp$dollars/1000, digits = 1), 'K')
  )
}

legend('topleft',
       lty = 1,
       lwd = 5,
       col = cols,
       legend = places)


title(main = 'Product place of origin by month')

# Barplot of the same
bp_data <- dcast(temp, year + month ~ place, value.var = 'dollars')

obj <- t(as.matrix(bp_data[,3:4]))
bp <- barplot(obj, beside = TRUE,
              col = cols,
              border = NA,
              names.arg = unique(paste0(months[temp$month], '\n', temp$year)),
              las = 1,
              yaxt = 'n',
              ylim = c(0, max(obj) * 1.15))
axis(side = 2,
     at = seq(0, 500000, 50000),
     labels = paste0(seq(0, 500000, 50000)/1000, 'K'),
     las = 1)
mtext(side = 2, 'Dollars', line = 4)
legend('topleft',
       fill = cols,
       legend = places,
       border = NA)
title(main = 'Product place of origin by month')

text(x = bp,
     y = obj,
     labels = paste0('$', round(obj/1000, digits = 1), 'K'),
     pos = 3,
     col = cols,
     cex = 0.4)


#####
# BY WEEK
#####
par(mfrow = c(2,1))
df$week <- as.numeric(format(df$date, '%U'))
df$year_week <- paste0(df$year,'-',  df$week)

temp <- df %>%
  group_by(year, week) %>%
  summarise(florida = sum(price[place == 'Florida'], na.rm = TRUE),
            other = sum(price[place == 'Non-Florida'], na.rm = TRUE))

library(tidyr)
temp_gathered <- gather(temp, key, dollars, florida:other)

# Subset to only recent months
temp_gathered <- temp_gathered[which(temp_gathered$year == 2015 &
                                       temp_gathered$week >= 15),]

# Label
temp_gathered$label <- paste0(temp_gathered$year, '-', temp_gathered$week)

# Relevel 
temp_gathered$label <- factor(as.character(temp_gathered$label), 
                                  levels = unique(sort(as.character(temp_gathered$label))))

# date
temp_gathered$first_day <- as.Date('2015-01-01') + (7 * temp_gathered$week)

g <- ggplot(data = temp_gathered, aes(x = first_day, y = dollars, group = key, color = key))
g1 <-g +   geom_line(size = 2) +
  geom_point(size = 4) +
  xlab('Date') +
  ylab('Dollars') +
  theme_bw() +
  ggtitle('Weekly spending by source location')

g <- ggplot(data = temp_gathered, aes(x = first_day, y = dollars, group = key, color = key)) 
g2 <- g + geom_area(aes(fill = key), color = NA, position = 'stack') + 
  xlab('Date') +
  ylab('Dollars') +
  theme_bw() +
  ggtitle('Weekly spending by source location')
#### PERCENTS
temp_p <- temp
temp_p$total <- temp_p$florida + temp_p$other
temp_p$florida <- temp_p$florida / temp_p$total * 100
temp_p$other <- temp_p$other / temp_p$total * 100

temp_p_gathered <- gather(temp_p, key, dollars, florida:other)


# Subset to only recent months
temp_p_gathered <- temp_p_gathered[which(temp_p_gathered$year == 2015 &
                                       temp_p_gathered$week >= 15),]

# Label
temp_p_gathered$label <- paste0(temp_p_gathered$year, '-', temp_p_gathered$week)

# Relevel 
temp_p_gathered$label <- factor(as.character(temp_p_gathered$label), 
                              levels = unique(sort(as.character(temp_p_gathered$label))))

# date
temp_p_gathered$first_day <- as.Date('2015-01-01') + (7 * temp_p_gathered$week)

g <- ggplot(data = temp_p_gathered, aes(x = first_day, y = dollars, group = key, color = key))
g3 <- g +   geom_line(size = 2) +
  geom_point(size = 4) +
  xlab('Date') +
  ylab('Percent of spending') +
  theme_bw() +
  ggtitle('Weekly spending by source location')


g <- ggplot(data = temp_p_gathered, aes(x = first_day, y = dollars, group = key, color = key)) 
g4 <- g + geom_area(aes(fill = key), color = NA, position = 'stack') + 
  xlab('Date') +
  ylab('Percent of spending') +
  theme_bw() +
  ggtitle('Weekly spending by source location')

###########
multiplot(g1, g2, g3, g4, cols = 2)

par(mfrow = c(1,1))
dev.off()

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

#####
# READ IN THE GOOGLE SPREADSHEET
#####
setwd(root)
setwd('data')
direct <- read.csv('Farm Produce Receiving - Sheet1.csv')

# Format dates, etc.
direct$Date <- as.Date(direct$Date, format = '%m/%d/%Y')

#####
# GET SMALL DATAFRAMES FOR BOTH DIRECT AND DISTRIBUTOR PURCHASE
#####
distributor_small <- df[,c('date', 'item', 'price', 'florida')]
distributor_small$provider <- 'distributor'

direct_small <- direct[,c('Date', 'General.Product', 'Total.Cost')]
direct_small$florida <- 'Florida'
direct_small$provider <- 'direct'
names(direct_small) <- names(distributor_small)

# Clean up item names in distributor
temp <- strsplit(distributor_small$item, ' ')
distributor_small$item <- sapply(temp, '[', 1)

# Capitalize item names in direct
direct_small$item <- toupper(direct_small$item)

#####
# JOIN BOTH TOGETHER
#####
both <- rbind(direct_small, distributor_small)

# Make year, month, week columns
both$year <- as.numeric(format(both$date, '%Y'))
both$month <- as.numeric(format(both$date, '%m'))
both$week  <- as.numeric(format(both$date, '%U'))
both$year_month <- paste0(both$year, '-', both$month)
both$year_week <- paste0(both$year, '-', both$week)
both <- arrange(both, date)
# write.csv(both, 'combined_for_kelli.csv', row.names = FALSE)


