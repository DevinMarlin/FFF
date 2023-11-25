library(ffscrapr)
library(usethis)
library(dplyr)
library(glue)
library(tibble)
library(nflfastR)
# get current year
year = as.integer(format(Sys.Date(), "%Y"))

con = espn_connect(season = 2023,
                   league_id = 631978,
                   espn_s2 = Sys.getenv("espn_s2"),
                   swid= Sys.getenv('swid'))

league = ff_league(con)

rosters = ff_rosters(con)

box = espn_getendpoint(con,view = 'mBoxscore')

match = espn_getendpoint(con,view = 'mMatchupScoreLite')

ps = ff_playerscores(con,season = 2023,week = 10)

raw_match = espn_getendpoint_raw(con,"https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/2023/segments/0/leagues/631978?view=mBoxscore&view=mMatchupScore&view=mRoster&view=mSettings&view=mStatus&view=mTeam&view=modular&view=mNav")

# function that returns the right list from box[[1]] based on the name attribute given
# check to see if box is present in the current environment. if not fetch it with espn_getendpoint
get_list = function(name,con = con){
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
rosters$franchise_name = sapply(rosters$franchise_id, function(x) get_name(id = x))


# replace the franchise_name column in the rosters data frame using the team name found with the get_list function
rosters$franchise_name = sapply(rosters$franchise_id, function(x) get_list(name = x)$name)

# get the value of name from each list in box$content$teams
test = sapply(box$content$teams, 
              function(x) tibble("{x$name}" := as.integer(x$id)),simplify = F)





box$content$teams$name

