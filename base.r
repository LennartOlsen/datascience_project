require("RSQLite")

main <- function() {
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite, "data/database.sqlite")
  dbListTables(con)
  matches <- dbGetQuery(con, "SELECT 
                       m.country_id, 
                       m.league_id, 
                       m.date, 
                       m.away_team_goal, 
                       m.home_team_goal, 
                       t_away.team_long_name as away_team_name, 
                       t_home.team_long_name as home_team_name
                  FROM 
                    match as m
                  JOIN 
                    team as t_away ON m.away_team_api_id = t_away.team_api_id
                  JOIN 
                    team as t_home ON m.home_team_api_id = t_home.team_api_id
                  LIMIT 400")
  
  print(matches)
}
