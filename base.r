require("RSQLite")

# Create new columns, w/L/D Column, match id column

main <- function() {
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite, "data/database.sqlite")
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
                  ")
  
  return(matches)
}

data <- main()

locate_NA <- function(x){
  ind <- which(is.na(x))
  return(ind)
}

w_l_d_column <- function(){
  for(d in data){
    
  }
  
  data$W/L/D <- w_l_d_vector
  
}
