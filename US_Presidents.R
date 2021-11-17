# install and load required libraries
install.packages(c('eeptools','reactable','reactablefmtr','modeest'))
x<-c("dplyr", "eeptools", "reactable","reactablefmtr",'modeest')
lapply(x, require, c = T)

#read csv file
pres_data <- read.csv("U.S. Presidents Birth and Death Information - Sheet1.csv", header = TRUE)

# data processing & manipulation
pres_data <- pres_data[!(is.na(pres_data$BIRTH.DATE) | pres_data$BIRTH.DATE==""), ]
pres_data$year_of_birth <- substr(pres_data$BIRTH.DATE,(nchar(pres_data$BIRTH.DATE)+1)-4,nchar(pres_data$BIRTH.DATE))
pres_data$dob <- as.Date(pres_data$BIRTH.DATE,"%b %d, %Y")    # birth date
pres_data$dod <- as.Date(pres_data$DEATH.DATE,"%b %d, %Y")    # death date

pres_data$is_alive[is.na(pres_data$dod)] <- "Yes"             # is_alive variable

pres_data$dod[is.na(pres_data$dod)] <- Sys.Date()             # replace empty death date cells with current date
pres_data$is_alive[is.na(pres_data$is_alive)] <- "No"

pres_data$lived_years <- round(age_calc(pres_data$dob,pres_data$dod,"years"),2)       # calculating lived_years
pres_data$lived_months <- round(age_calc(pres_data$dob,pres_data$dod,"months"),2)     # calculating lived_months
pres_data$lived_days <- as.numeric(age_calc(pres_data$dob,pres_data$dod,"days"))      # calculating lived_days

# top 10 longest lived presidents ranked
longest_lived <- pres_data[order(-pres_data$lived_days),][1:10,]
longest_lived$lived_years <-  with(longest_lived, ifelse(longest_lived$is_alive == "Yes", paste0(longest_lived$lived_years,"*"), longest_lived$lived_years))
longest_lived$lived_months <-  with(longest_lived, ifelse(longest_lived$is_alive == "Yes", paste0(longest_lived$lived_months,"*"), longest_lived$lived_months))
longest_lived$lived_days <-  with(longest_lived, ifelse(longest_lived$is_alive == "Yes", paste0(longest_lived$lived_days,"*"), longest_lived$lived_days))
longest_lived <- longest_lived[ -c(7,8,9,13,14)]          # removing unwated columns

# top 10 shortest lived presidents ranked
shortest_lived <- pres_data[order(pres_data$lived_days),][1:10,]
shortest_lived$lived_years <-  with(shortest_lived, ifelse(shortest_lived$is_alive == "Yes", paste0(shortest_lived$lived_years,"*"), shortest_lived$lived_years))
shortest_lived$lived_months <-  with(shortest_lived, ifelse(shortest_lived$is_alive == "Yes", paste0(shortest_lived$lived_months,"*"), shortest_lived$lived_months))
shortest_lived$lived_days <-  with(shortest_lived, ifelse(shortest_lived$is_alive == "Yes", paste0(shortest_lived$lived_days,"*"), shortest_lived$lived_days))
shortest_lived <- shortest_lived[ -c(7,8,9,13,14)]

# generate table 'top 10 longest lived presidents ranked' & scatter plot
reactable(longest_lived,defaultColDef = colDef(header = function(value) gsub(".", " ", value, fixed = TRUE),cell = function(value) format(value, nsmall = 1),minWidth = 70,headerStyle = list(background = "#f7f7f8")),
          columns = list(year_of_birth = colDef(align = "center"), lived_days =  colDef(align = "center"), lived_months =  colDef(align = "center"), lived_years =  colDef(align = "center")),
          bordered = TRUE, striped = TRUE, highlight = TRUE, sortable = FALSE ) %>% add_title("Top 10 Presidents from longest lived to shortest lived", font_family = "Helvetica", font_size = 24) %>% add_source("*Still alive")

plot(longest_lived$year_of_birth,longest_lived$lived_days,xlab = "year_of_birth", ylab = "lived_days", col = "red")

# generate table 'top 10 shortest lived presidents ranked' & scatter plot
reactable(shortest_lived,defaultColDef = colDef(header = function(value) gsub(".", " ", value, fixed = TRUE),cell = function(value) format(value, nsmall = 1),minWidth = 70,headerStyle = list(background = "#f7f7f8")),
          columns = list(year_of_birth = colDef(align = "center"), lived_days =  colDef(align = "center"), lived_months =  colDef(align = "center"), lived_years =  colDef(align = "center")),
          bordered = TRUE, striped = TRUE, highlight = TRUE, sortable = FALSE ) %>% add_title("Top 10 Presidents from shortest lived to longest lived", font_family = "Helvetica", font_size = 24) %>% add_source("*Still alive")

plot(shortest_lived$year_of_birth,shortest_lived$lived_days,xlab = "year_of_birth", ylab = "lived_days", col = "red")

# weighted mean, list assumptions: if alive: 0.083335, else 0.01282, more details listed in report
pres_data$weights <- ifelse(pres_data$is_alive == "Yes",0.083335,0.01282)

#measures of central tendency
mean <- round(mean(pres_data$lived_days),2)
std_dev <- round(sd(pres_data$lived_days),2)
weighted_mean <- round(weighted.mean(pres_data$lived_days, pres_data$weights),2)
median <- round(median(pres_data$lived_days),2)
mode <-  mlv(pres_data$lived_days, method = "mfv")
min <- min(pres_data$lived_days)
max <- max(pres_data$lived_days)

Statistic <- c("Mean","Weighted Mean","Median","Mode","Max", "Min", "Standard deviation")
Values <- rbind(mean,weighted_mean,median,list(mode),max,min,std_dev)

df <- as.data.frame(Values,Statistic)
colnames(df) <- c('lived_days')

# generate table 'Basic statistical measures
reactable(df,bordered = TRUE, striped = TRUE, highlight = TRUE, sortable = FALSE) %>% add_title("Basic statistics- mean, median, mode, max, min and standard deviation", font_family = "Helvetica", font_size = 24)

# plot normal distribution
set.seed(8015)
y <- rnorm(45,mean,std_dev)
hist(y, main = "Normal Distribution", col = "green", border = "black", ylim = c(0,25), xlab = "lived_days")

abline(v = mean, col = 'blue', lwd = 3)
abline(v = median, col = 'red', lwd = 3)
abline(v = max, col = 'maroon', lwd = 3)
abline(v = min, col = 'violet', lwd = 3)
abline(v = weighted_mean, col = 'orange', lwd = 3)

text(x = 38000 ,                   
     y = 25,
     paste("Mean =", mean),
     col = "blue",
     cex = 1)
text(x = 38000 ,                   
     y = 22,
     paste("Median =", median),
     col = "red",
     cex = 1)
text(x = 38000 ,                   
     y = 19,
     paste("Weighted Mean =", weighted_mean),
     col = "orange",
     cex = 1)
text(x = 38000 ,                   
     y = 16,
     paste("max =", max),
     col = "maroon",
     cex = 1)
text(x = 38000 ,                   
     y = 13,
     paste("min =", min),
     col = "violet",
     cex = 1)


