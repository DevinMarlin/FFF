library(ffscrapr)
library(usethis)


con = espn_connect(season = 2022,
                   league_id = 631978,
                   espn_s2 = Sys.getenv("espn_s2"),
                   swid= Sys.getenv('swid'))

league = ff_league(con)

rosters = ff_rosters(con)
