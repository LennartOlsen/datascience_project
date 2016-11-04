# load in the base.r file before this file!

#create a league rank function. 
#League_id is an int 
#seasonID is characters - E.g. "2008/2009" 
#teamName is characters - E.g. "Basel"
team_rank_in_league <- function(seasonID, leagueID, teamName){
  # retrieves all dbData from a specific season
  season_data <- dbData[dbData$season == seasonID & dbData$league_id == leagueID,]
  
  teams = c(season_data$home_team_name[!duplicated((season_data$home_team_name))])
  num_of_teams = length(teams)
  
  # Creates data frame
  league_score <- data.frame(teams, p= integer(num_of_teams),
                             g_for= integer(num_of_teams), g_against= integer(num_of_teams),
                             deficits= integer(num_of_teams))
  
  # inserts all teams into the league score table, with their season points, goals for/against and deficit
  for(team in teams){
    team_data_away <- as.data.frame(season_data[season_data$away_team_name == team,])
    team_data_home <- as.data.frame(season_data[season_data$home_team_name == team,])
    
    points <- get_season_points(team_data_home$w_l_d_home,team_data_away$w_l_d_away)
    deficit <- get_season_deficit(team_data_home$home_team_goal,team_data_home$away_team_goal, 
                                  team_data_away$home_team_goal,team_data_away$away_team_goal)
    goals_for <- sum(team_data_home$home_team_goal) + sum(team_data_away$away_team_goal)
    goals_against <- sum(team_data_home$away_team_goal) + sum(team_data_away$home_team_goal)
    
    league_score[league_score$teams == team,] <- c(team, points,goals_for, goals_against, deficit)
  }
  
  # Order teams according to the football rules, points - deficit and so on.
  attach(league_score)
  league_score <- league_score[order(p, deficits, g_for, g_against, decreasing = TRUE),]
  detach(league_score)
  
  return(which(league_score$teams == teamName))
}

#create a league rank function at specific moment of a season. 
#League_id is an int 
#stage is integer - E.g. 5
#seasonID is characters - E.g. "2008/2009" 
#teamName is characters - E.g. "Basel"
team_rank_specific_stage <- function(seasonID, stage, leagueID, teamName){
  # retrieves all data from a specific season and a current time of the season
  season_data <- dbData[dbData$season == seasonID & dbData$league_id == leagueID,]
  
  teams = c(season_data$home_team_name[!duplicated((season_data$home_team_name))])
  num_of_teams = length(teams)
  
  season_data <- season_data[season_data$stage < stage,]
  
  # Creates data frame
  league_score <- data.frame(teams, p= integer(num_of_teams),
                             g_for= integer(num_of_teams), g_against= integer(num_of_teams),
                             deficits= integer(num_of_teams))
  
  # inserts all teams into the league score table, with their season points, goals for/against and deficit
  for(team in teams){
    team_data_away <- as.data.frame(season_data[season_data$away_team_name == team,])
    team_data_home <- as.data.frame(season_data[season_data$home_team_name == team,])
    
    points <- get_season_points(team_data_home$w_l_d_home,team_data_away$w_l_d_away)
    deficit <- get_season_deficit(team_data_home$home_team_goal,team_data_home$away_team_goal, 
                                  team_data_away$home_team_goal,team_data_away$away_team_goal)
    goals_for <- sum(team_data_home$home_team_goal) + sum(team_data_away$away_team_goal)
    goals_against <- sum(team_data_home$away_team_goal) + sum(team_data_away$home_team_goal)
    
    league_score[league_score$teams == team,] <- c(team, points,goals_for, goals_against, deficit)
  }
  
  # Order teams according to the football rules, points - deficit and so on.
  attach(league_score)
  league_score <- league_score[order(p, deficits, g_for, g_against, teams, decreasing = TRUE),]
  detach(league_score)
  
  return(which(league_score$teams == teamName))
}

# Current form for a specific number of matches
current_form <- function(search_date, leagueID, teamName, num_form_games){
  league_data <- dbData[dbData$league_id == leagueID & dbData$date <= search_date,]
  
  # sort by data, descending
  attach(league_data)
  league_data <- league_data[order(date, decreasing = TRUE),]
  detach(league_data)
  
  # find the selected teams for the last matches chosen.
  league_data <- head(league_data[league_data$away_team_name == teamName | league_data$home_team_name == teamName,], num_form_games)
  
  if(nrow(league_data) < 1)
    return(0)
  
  # create vector and find form
  form <- 0
  for(i in 1:nrow(league_data)){
    if(league_data$home_team_name[i] == teamName){
      if(league_data$w_l_d_home[i] == 0)
        form <- form + 3
      if(league_data$w_l_d_home[i] == 1)
        form <- form
      if(league_data$w_l_d_home[i] == 2)
        form <- form + 1
    } else if(league_data$away_team_name[i] == teamName){
      if(league_data$w_l_d_away[i] == 0)
        form <- form + 3
      if(league_data$w_l_d_away[i] == 1)
        form <- form
      if(league_data$w_l_d_away[i] == 2)
        form <- form + 1
    }
  }
  
  # return a sum of vector of w/l/d
  return(form)
}

# get team deficit for the season
team_deficit_season <- function(seasonID, leagueID, teamName){
  # retrieves all data from a specific season and a current time of the season
  season_data <- dbData[dbData$season == seasonID & dbData$league_id == leagueID,]
  
  team_data_away <- as.data.frame(season_data[season_data$away_team_name == team,])
  team_data_home <- as.data.frame(season_data[season_data$home_team_name == team,])
  
  deficit <- get_season_deficit(team_data_home$home_team_goal,team_data_home$away_team_goal, 
                                team_data_away$home_team_goal,team_data_away$away_team_goal)
  
  return(deficit)
}

# get team deficit for the season at a current time
team_deficit_specific_date <- function(seasonID, search_date, leagueID, teamName){
  # retrieves all data from a specific season and a current time of the season
  season_data <- dbData[dbData$season == seasonID & dbData$league_id == leagueID,]
  season_data <- season_data[season_data$date <= search_date,]
  
  team_data_away <- as.data.frame(season_data[season_data$away_team_name == teamName,])
  team_data_home <- as.data.frame(season_data[season_data$home_team_name == teamName,])
  
  deficit <- get_season_deficit(team_data_home$home_team_goal,team_data_home$away_team_goal, 
                                team_data_away$home_team_goal,team_data_away$away_team_goal)
  
  return(deficit)
}
