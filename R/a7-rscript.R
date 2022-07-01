#This is Varun's Rscript
library(data.table)
library(R.utils)
library(openintro)
library(dplyr)
library(ggplot2)

correlation_function <- function(year_num) {
  read <- data.frame(fread("data/presidential_2008-2016.csv.bz2"))
  
  county_select <- county %>%
    rename(county = name) %>%
    mutate(state = state2abbr(state)) %>%
    select(county, poverty, med_income, state) 
  
  joint_data <- merge(read, county_select, by = c("county", "state"))
  
  joint_data <- joint_data %>%
    arrange(med_income)
  
  cols_wanted <- c(year_num, "med_income")
  
  joint_data <- joint_data[grepl(paste(cols_wanted, collapse = "|"),  names(joint_data))]
  
  names(joint_data)[3] = "gop"
  
  joint_data <-  joint_data %>%
    select(gop, med_income)
  
  ggplot(joint_data) + ggtitle("Republican votes based on median income in " ,year_num) + geom_point(aes(x = med_income, y = gop), color = "black", size = 1) + geom_smooth(aes(x = joint_data$med_income, y = joint_data$gop), method = "lm", color = "purple")+labs(x = "Median Income", y = "Votes")
}
correlation_function(2008)