require("RSQLite")

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
                  ORDER BY
                    m.date
                  ")
  
  return(matches)
}

data <- main()

# locates indices of where NA is present
locate_NA <- function(x){
  ind <- which(is.na(x))
  return(ind)
}

# Amount of NA data
na_data <- locate_NA(data)

# new columns with the win, loss, draw combination where win = 0, loss = 1, draw = 2
data$w_l_d_home <- ifelse(data$home_team_goal==data$away_team_goal, 2,
                          ifelse(data$home_team_goal < data$away_team_goal, 1,
                                ifelse(data$home_team_goal > data$away_team_goal, 0,NA  )))

data$w_l_d_away <- ifelse(data$home_team_goal == data$away_team_goal, 2,
                          ifelse(data$home_team_goal > data$away_team_goal, 1,
                                 ifelse(data$home_team_goal < data$away_team_goal, 0,NA  )))

# Number of wins, losses and draws
number_of_wins <- length(data$w_l_d_home[data$w_l_d_home == 0])
number_of_losses <- length(data$w_l_d_home[data$w_l_d_home == 1])
number_of_draws <- length(data$w_l_d_home[data$w_l_d_home == 2])
