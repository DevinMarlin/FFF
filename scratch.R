library(ffscrapr)
library(usethis)
library(dplyr)
library(glue)
library(tibble)
library(nflfastR)
library(tibble)
library(magrittr)
library(jsonlite)
# get current year
year = as.integer(format(Sys.Date(), "%Y"))

con = espn_connect(season = 2023,
                   league_id = 631978,
                   espn_s2 = Sys.getenv("espn_s2"),
                   swid= Sys.getenv('swid'))

league = ff_league(con)

rosters = ff_rosters(con)

box_S = espn_getendpoint(con,
                       view = 'mBoxscore')

match = espn_getendpoint(con,
                         view = 'mMatchupScoreLite')

ps = ff_playerscores(con,
                     season = 2023,
                     week = 10)

raw_match = espn_getendpoint_raw(con,"https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/2023/segments/0/leagues/631978?view=mBoxscore&view=mMatchupScore&view=mRoster&view=mSettings&view=mStatus&view=mTeam&view=modular&view=mNav")

# function that returns the right list from box[[1]] based on the name attribute given
# check to see if box is present in the current environment. if not fetch it with espn_getendpoint
get_list = function(name){
  if(!exists('box')){
    con = espn_connect(season = as.integer(format(Sys.Date(), "%Y")),
                       league_id = 631978,
                       espn_s2 = Sys.getenv("espn_s2"),
                       swid= Sys.getenv('swid'))
    box = espn_getendpoint(con,view = 'mBoxscore')
  }
  for(i in 1:length(box$content$teams)){
    if(box$content$teams[[i]]$name == name){
      return(box$content$teams[[i]])
    }
  }
  
}

#function to retrieve the team name give supplied id
get_name = function(id){
  for(i in 1:length(box$content$teams)){
    if(box$content$teams[[i]]$id == id){
      return(box$content$teams[[i]]$name)
    }
  }
}

# function to get the id of a team from the name using get_list
get_id = function(name){
  get_list(name = name)$id
}

#function to get the name of a team with a supplied ID
get_name_id = function(id){
  get_list(id = id)$name
}

#replace the franchise_name column in the rosters data frame with the results of get_name function
rosters$franchise_name = sapply(rosters$franchise_id, 
                                \(x) get_name(id = x))


# replace the franchise_name column in the rosters data frame using the team name found with the get_list function
rosters$franchise_name = sapply(rosters$franchise_id,
                                \(x) get_list(name = x)$name)

# get the value of name from each list in box$content$teams
test = sapply(box$content$teams, 
              \(x) tibble(name = x$name, id = as.integer(x$id)),simplify = F) %>%
  do.call(rbind, .)


library(ffscrapr)
library(jsonlite)

get_matchup_boxscore <- function(season, week, matchup_id, conn) {
  # Create an x-fantasy-filter JSON to limit data to the desired week.
  # Here we assume that filtering by matchupPeriodId (the week) will narrow down the boxscore.
  xff <- list(matchupPeriodId = week) %>% 
    toJSON(auto_unbox = TRUE)
  
  # Retrieve boxscore data for the given week using the "mBoxscore" view.
  boxscore_data <- espn_getendpoint(conn, view = "mBoxscore", x_fantasy_filter = xff)
  
  # If the returned data includes a "matchups" element, find the one with the given matchup_id.
  if (!is.null(boxscore_data$matchups)) {
    matchup <- NULL
    for (m in boxscore_data$matchups) {
      if (!is.null(m$matchupId) && m$matchupId == matchup_id) {
        matchup <- m
        break
      }
    }
    if (is.null(matchup)) {
      stop("No matchup found with the provided matchup_id.")
    }
    teams <- matchup$teams
  } else {
    # If no explicit matchup grouping exists, assume the returned data itself contains the teams.
    teams <- boxscore_data$teams
  }
  
  if (length(teams) != 2) {
    stop("Expected exactly two teams in the matchup.")
  }
  
  # Function to extract a team's player names and points from its roster.
  extract_team_df <- function(team) {
    # We assume each team's roster is a list of player entries.
    # Each entry is assumed to contain a 'player' sublist with a 'fullName'
    # and a 'score' element with the points for that week.
    player_list <- lapply(team$roster, function(entry) {
      player_name <- if (!is.null(entry$player) && !is.null(entry$player$fullName)) {
        entry$player$fullName
      } else {
        NA
      }
      points <- if (!is.null(entry$score)) entry$score else NA
      data.frame(player_name = player_name, points = points, stringsAsFactors = FALSE)
    })
    do.call(rbind, player_list)
  }
  
  team1_df <- extract_team_df(teams[[1]])
  team2_df <- extract_team_df(teams[[2]])
  
  # Return a list with two data frames: one per team.
  list(team1 = team1_df, team2 = team2_df)
}

# Example usage:
# First, create your ESPN connection using espn_connect() from ffscrapr.
# (Make sure your connection object 'conn' has the correct league, season, and authentication details.)
# For example:
# conn <- espn_connect(league_id = "YOUR_LEAGUE_ID", season = 2024)
# Then, call the function with the desired season, week, and matchup id.
# result <- get_matchup_boxscore(season = 2024, week = 1, matchup_id = 3, conn = conn)
# View the results:
# result$team1
# result$team2


