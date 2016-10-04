df <- 
  data.frame(price = c(1.2, 1.78),
             type = c('whole', 'destemmed'))
df$quantity <-  ifelse(df$type == 'whole', 0.73, 1)
df$labor_per_pound <- c(.2777, 0)
df$total <- (df$price / df$quantity) + df$labor_per_pound
