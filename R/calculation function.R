library(formattable)
library(dplyr)


## Function Sets
table <- data.table::fread("data/presidential_2008-2016.csv.bz2", stringsAsFactors = FALSE)

percent_votes <- function(abbr_name, year, table_chosen) {
  target <- paste0(abbr_name, "_" ,year)
  total <- paste0("total_", "2008")
  percent <- percent(sum(table_chosen[[target]]) / sum(table_chosen[[total]]))
}

dem2008 <- percent_votes("dem", "2008", table)

state_info <- function(state_name) {
  state <- unique(table$state)
  separate <- table %>%
    filter(table$state == state_name)
  separate <- separate[, -c(1:2)]
  separate <- separate[, -13]
  separate2 <- data.frame(t(colSums(separate)))
  separate2 <- mutate(separate2, state = state_name)
}

## e.g. state_list <- state_info("AL")

combined_state <- function() {
  state <- unique(table$state)
  combined_table <- state_info("AL")
  for (i in 2:50) {
    combined_table <- rbind(combined_table, state_info(state[i]))
  }
  return(combined_table)
}

state_table <- combined_state()

compare_votes <- function(abbr_name, year, max_min) {
  if (max_min == "max") {
    state <- state_table %>%
      filter(state_table[[paste0(abbr_name, "_", year)]] == max(state_table[[paste0(abbr_name, "_", year)]]))
  } else {
    state <- state_table %>%
      filter(state_table[[paste0(abbr_name, "_", year)]] == min(state_table[[paste0(abbr_name, "_", year)]]))
  }
  state_name = state[1, 13]
}

## e.g. compare <- compare_votes("dem", "2008", "max")

