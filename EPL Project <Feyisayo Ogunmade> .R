#################################################################
##################################################################
### FEYISAYO OGUNMADE <- EPL STANDING 
### FINAL PROJECT 
##################################################################
##################################################################
##################################################################


# The list of libraries I used for the project
# I already installed the packages so I did not need to reinstall the packages again.
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr) 
# Test function call:
EPL_Standings (Date = '12/12/2024', Season = '2023/24') 
#Season <- '2021/2022'
#Date <- '10/25/2021'

# defining the EPL Standing function
EPL_Standings <- function(Date, Season) {
  
  # Step 1: Correctly download the appropriate file based on the Season argument.
  # This code is explaining the 3 most current seasons of the English Premier League, indicating that whichever season is called out 
  # R should read from the various seasons' dataframes and any other season should read out or return 'Season not Available'
  
  Season <- '2021/22'  
  Date <- '07/25/2022'
  # I created a dummy variable of Season and Date with random dates to be sure that my step 1 was right before proceeding to the next
  if (Season == '2021/22') {
    df <- read_csv('https://www.football-data.co.uk/mmz4281/2122/E0.csv')
  } else if (Season == '2022/23') {
    df <- read_csv('https://www.football-data.co.uk/mmz4281/2223/E0.csv')
  } else if (Season == '2023/24') {
    df <- read_csv('https://www.football-data.co.uk/mmz4281/2324/E0.csv')
  } else {
    stop("Season not Available")
  }
  
  # Step 2: Select only the needed columns in the dataframe.
  # This code is to select the specific columns that I am dealing with for this project which I listed
  df <- df %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  # This code is to view the first few rows of the adjusted data frame to show the 6 columns that I specifies
  head(df)
  
   # Step 3: Filter the dataframe so that only the matches played up until the specified Date remain.
      # A) format the input date into a universal format
  # This code is to format the date to have the month, year and date format in the results
  date_input <- mdy(Date)
  
     # B) format the date column in the dataframe to a universal format
  # This also does the same thing that the A does
  df <- df %>% 
      mutate(Date = dmy(Date)) %>% 
    #str(df)
    # C) Use the date from A to filter the dataframe from B
    # This code filters the data frame to include only the relevant date
     filter(Date <= date_input)
  
  head(df)
  
  # Combine home and away results into a single data frame
  combined_results <- bind_rows(home_results, away_results)
  
  # Step 4: Deal with Home and Away team/goal columns
       
  # home df -- this section accounts for the matches played at home 
  home_results <- df %>% 
    mutate(Team = HomeTeam, 
           team_goals = FTHG, 
           opponent_goals = FTAG, 
           Result = ifelse(FTR == 'H', 'Win', ifelse(FTR == 'D', 'Draw','Loss')))
  
     #  away df -- this section accounts for the away matches
   away_results <- df %>% 
    mutate(Team = AwayTeam, 
           team_goals = FTAG, 
           opponent_goals = FTHG, 
           Result = ifelse(FTR == 'A', 'Win', ifelse(FTR == 'D', 'Draw', 'Loss')))
      # Combine both the home and away dataframe based on the project prompts
  combined_results <- bind_rows(home_results, away_results)
  head(combined_results)
  
  # Step 5: Do all of the aggregation (group by team)
  # Aggregate the combined results by team to calculate wins, 
  # draws, losses, goals scored, goals allowed, points, and other statistics.
    team_statistics <- combined_results %>%
    group_by(Team) %>%
    summarize(
      Wins = sum(Result == 'Win'),
      Draws = sum(Result == 'Draw'),
      Losses = sum(Result == 'Loss'),
      GS = sum(team_goals),
      GA = sum(opponent_goals),
      Points = Wins * 3 + Draws,
      MatchesPlayed = n()
    ) %>%
    mutate(
      Record = paste(Wins, Draws, Losses, sep = '-'),
      PPM = Points / MatchesPlayed, # points per match
      PtPct = Points / (3 * MatchesPlayed), # percentage of points 
      GSM = GS / MatchesPlayed, # goals scored during the match
      GAM = GA / MatchesPlayed # goals allowed during the match
    )
    View(team_statistics)
    
    
    # Step 6: Arrange the results, sorting the final dataframe descending and ascending based on PPM, Wins, GSM, GA.
     final_df <- team_statistics %>%
    arrange(desc(PPM), desc(Wins), desc(GSM), GA)
     view(final_df)
     
    # Final Step: Return the ordered data frame
     return(final_df)
}
