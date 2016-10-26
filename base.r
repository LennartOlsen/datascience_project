require("RSQLite")

main <- function() {
  sqlite <- dbDriver("SQLite")
  con <- dbConnect(sqlite, "data/database.sqlite")
  matches <- dbGetQuery(con, "SELECT 
                       m.league_id,
                       m.date,
                       m.season,
                       m.stage,
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
                        m.date ASC")
  
  return(matches)
}

# Tidying the date
data <- main()
data$date <- sub(" 00:00:00", "", data$date)
data$date <- as.Date(data$date, "%Y-%m-%d")

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

# get individual team points in a season
get_season_points <- function(home, away){
  points = 0
  for(m in home){
    if(m == 0){
      points = points + 3
    } else if(m == 2){
      points = points + 1
    }
  }
  
  for(m in away){
    if(m == 0){
      points = points + 3
    } else if(m == 2){
      points = points + 1
    }
  }
  return(points)
}

# get individual team goal deficit in a season
get_season_deficit <- function(home_team, away, home, away_team){
  team_total <- sum(home_team) + sum(away_team)
  opposing_team_total <- sum(away) + sum(home)
  deficit <- team_total - opposing_team_total
}
