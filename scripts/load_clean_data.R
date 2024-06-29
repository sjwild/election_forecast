library(tidyverse)
library(cmdstanr)
library(tidybayes)
library(posterior)
library(dataverse)

# download data from dataverse
state_results <- get_dataframe_by_name(
  filename = "1976-2020-president.tab",
  dataset = "10.7910/DVN/42MVDX", 
  server = "dataverse.harvard.edu")



# group election results by state-year-party
state_results$party_simplified[state_results$party_simplified == "LIBERTARIAN"] <- "OTHER"
state_results <- state_results %>%
  group_by(state, year, party_simplified) %>%
  summarize(candidatevotes = sum(candidatevotes)) %>%
  group_by(state, year) %>%
  mutate(candidatepercent = candidatevotes/sum(candidatevotes))


# Get national results
national_results <- state_results %>%
  group_by(year, party_simplified) %>%
  summarize(candidatevotes = sum(candidatevotes)) %>%
  group_by(year) %>%
  mutate(candidatepercent = candidatevotes/sum(candidatevotes),
         state = "NATIONAL")


# combine into one dataset
election_results <- rbind(state_results[, colnames(national_results)],
                          national_results)


# pivot_wider
democratic_results <- election_results[election_results$party_simplified == "DEMOCRAT", 
                 c("state", "candidatepercent", "year")] %>% 
  pivot_wider(names_from = state, 
              values_from = candidatepercent, 
              id_cols = year)
democratic_results <- democratic_results[, 2:53]
cov(democratic_results)


# correct order for indexing
state_list <- c("NATIONAL", unique(state_results$state))
party_list <- c("DEMOCRAT", "REPUBLICAN", "OTHER")
election_results$state <- factor(election_results$state, levels = state_list)
election_results$party_simplified <- factor(election_results$party_simplified, 
                                            levels = party_list)








