source("base.r")
source("functions for research variables(att).r")

size <-nrow(dbData)
tempdbData <- data.frame(league_id = integer(size), dbData$season, dbData$stage,
                           dbData$home_team_name, form_5= integer(size),
                           rank = integer(size), deficits_surplus = integer(size))

for(i in 1:size){
  form_5 <- current_form(dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i], 5)
  
  if(dbData$stage[i] == 1){
    d_s <- 0
    rank <- 1
    tempdbData[i,] <- c(dbData$league_id[i], dbData$season[i],dbData$stage[i], dbData$home_team_name[i], form_5, 
                      rank, d_s)
    }else{
      rank <- team_rank_specific_stage(dbData$season[i], dbData$stage[i], dbData$league_id[i], dbData$home_team_name[i])
      #rank <- 1
      d_s <- team_deficit_specific_date(dbData$season[i], dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i])
      tempdbData[i,] <- c(dbData$league_id[i], dbData$season[i], dbData$stage[i], dbData$home_team_name[i], form_5, 
                          rank, d_s)
    }
}