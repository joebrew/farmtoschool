library(ggplot2)

#####
# LINEAR
#####
x <- seq(0.50, 5.00, by = 0.05)
y <- jitter(seq(500, 300, length = length(x)), factor = 50)

ggplot() +
  geom_point(aes(x = x, y = y)) + 
  geom_smooth(aes(x = x, y = y))+
  xlab('Average cost per a la carte item (dollars)') +
  ylab('Purchases of a la carte items') + 
  theme_bw() +
  ggtitle('Theoretical relationship between unhealthy food cost and consumption')


#####
# EXPONENTIAL
#####
x <- seq(0.50, 5.00, by = 0.05)
y <- jitter(seq(500, 300, length = length(x)), factor = 50) ^ (seq(1.2, 0.8, length = length(x)))

ggplot() +
  geom_point(aes(x = x, y = y)) + 
  geom_smooth(aes(x = x, y = y)) +
  xlab('Dollars per item') +
  ylab('Purchases of item') + 
  theme_bw()


#####
# LESS THAN EXPECTED
#####
x <- seq(0.50, 5.00, by = 0.05)
y <- jitter(seq(500, 300, length = length(x)), factor = 50) ^ (0.8)

ggplot() +
  geom_point(aes(x = x, y = y)) + 
  geom_smooth(aes(x = x, y = y))+
  xlab('Dollars per item') +
  ylab('Purchases of item') + 
  theme_bw()



Rationale:
  The over-consumption of unhealthy foods, and the corresponding under-consumption of healthy foods, form a major causal component of the current American youth obesity epidemic.  Poor food choices are the result of culture, convenience and cost.  

Proposal:
  
  