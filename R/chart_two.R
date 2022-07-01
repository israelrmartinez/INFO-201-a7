library("dplyr")
library("tidyr")
library("data.table")
library("ggplot2")
library("openintro")
library("tm")

majority_func <- function(year) {
  ## Decrypting the data file.
  election_data <- data.frame(fread("data/presidential_2008-2016.csv.bz2")) 
  
  ## Vector of names of the columns to be selected for dataframe. 
  cols <- c("county", year, "state")
   
  ## Gathering of year specific data.
  election_data <- election_data[grepl(paste(cols, collapse="|"), 
                                               names(election_data))]
  
  ## Renaming columns in order to be used for any year specified.
  names(election_data)[2:5] <- c("total", "democrat", "republican", "other")
  
  ## Calculates difference in votes between democrat and republican.
  difference <- election_data$democrat - election_data$republican
  
  ## Adds a new column determining what party got majority votes in that county.
  election_data$majority <- ifelse(difference > 0, "democrat", "republican")
  
  ## Adds a column with the difference between democratic votes and republican votes per county.
  # election_data <- election_data %>%
  #   mutate(difference = abs(difference))
  
  election_data$county <- tolower(election_data$county)
    
  removeWords <- c("county", "parish", "city")
  
  election_data$county <- removeWords(election_data$county, removeWords)
  
  election_data$county <- gsub("saint", "st", election_data$county)
  
  election_data$county <- gsub(" ", "", election_data$county)
  
  election_data$county <- gsub("'", "", election_data$county) 
  
  ## Gets a data frame of USA longitude and latitudes with state and county names.
  map_data <- map_data("county") %>%
    rename(county = subregion,
           state = region) 
  
  map_data$state <- state2abbr(map_data$state)
  
  map_data$county <- gsub(" ", "", map_data$county)
  
  ## Merges both the map data and election data by state and county names.
  joined_data <- right_join(map_data, election_data, by=c("state", "county"))
  
  ## Creates a map with the counties and which party got the majority votes.  
  joined_data %>%
    ggplot() +
    geom_polygon(aes(x=long, y = lat, group = group, fill = majority)) + 
    ggtitle(paste(year, "US Presidential Election Map By County")) +
    coord_fixed(1.3)
}
