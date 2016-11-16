#source("base.r")
#source("base2.r")

#Data conversion and processing
#--------------------------------------------------------------------------------------------
size <-nrow(dbData)
tempdbData_ext <- data.frame(league_id = integer(size), dbData$season, dbData$stage,
                         dbData$home_team_name, form_5= integer(size),
                         rank = integer(size), deficits_surplus = integer(size),
                         dbData$away_team_name, form_5_opp= integer(size),
                         rank_opp = integer(size), deficits_surplus_opp = integer(size))

start.time <- Sys.time()
# optimize loop search by defining the number of stages in a league, and break for loop when
# the number of matches needed is found
for(i in 1:size){
  start.function.time <- Sys.time()
  
  if(dbData$stage[i] > 5){
    form_5 <- current_form(dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i], 5)
    rank <- team_rank_specific_stage(dbData$season[i], as.numeric(dbData$stage[i]), dbData$league_id[i], dbData$home_team_name[i])
    d_s <- team_deficit_specific_date(dbData$season[i], dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i])
    form_5_away <- current_form(dbData$date[i]-1, dbData$league_id[i], dbData$away_team_name[i], 5)
    rank_away <- team_rank_specific_stage(dbData$season[i], as.numeric(dbData$stage[i]), dbData$league_id[i], dbData$away_team_name[i])
    d_s_away <- team_deficit_specific_date(dbData$season[i], dbData$date[i]-1, dbData$league_id[i], dbData$away_team_name[i])
    tempdbData_ext[i,] <- c(dbData$league_id[i], dbData$season[i], as.numeric(dbData$stage[i]), dbData$home_team_name[i], form_5,
                        rank, d_s, dbData$away_team_name[i], form_5_away, rank_away, d_s_away)
  }
  print(i);
  end.function.time <- Sys.time()
  print(end.function.time - start.function.time)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tempdbData_trimmed_ext <- tempdbData_ext[dbData$stage > 5,]

#Normalization of data with all stages
#--------------------------------------------------------------------------------------------
min_max_normalization <- function(x){
  normalized <- (x - min(x))/(max(x)-min(x))
  return(normalized)
}
# makes data numeric
tempdbData_ext$form_5 <- as.numeric(tempdbData_ext$form_5)
tempdbData_ext$rank <- as.numeric(tempdbData_ext$rank)
tempdbData_ext$deficits_surplus <- as.numeric(tempdbData_ext$deficits_surplus)
tempdbData_ext$form_5_opp <- as.numeric(tempdbData_ext$form_5_opp)
tempdbData_ext$rank_opp <- as.numeric(tempdbData_ext$rank_opp)
tempdbData_ext$deficits_surplus_opp <- as.numeric(tempdbData_ext$deficits_surplus_opp)

norm_data_ext <- tempdbData_ext[,5:7 & 9:11]
#normalizes the data
norm_data_ext$form_5 <- min_max_normalization(tempdbData_ext[,5])
norm_data_ext$rank <- min_max_normalization(tempdbData_ext[,6])
norm_data_ext$deficits_surplus <- min_max_normalization(tempdbData_ext[,7])
norm_data_ext$form_5_opp <- min_max_normalization(tempdbData_ext[,9])
norm_data_ext$rank_opp <- min_max_normalization(tempdbData_ext[,10])
norm_data_ext$deficits_surplus_opp <- min_max_normalization(tempdbData_ext[,11])

#write.csv(norm_data_ext,"normalized_data_ext.csv", quote = FALSE, row.names = FALSE)

#Normalization of data without first 5 stages
#--------------------------------------------------------------------------------------------
# makes data numeric
tempdbData_trimmed_ext$form_5 <- as.numeric(tempdbData_trimmed_ext$form_5)
tempdbData_trimmed_ext$rank <- as.numeric(tempdbData_trimmed_ext$rank)
tempdbData_trimmed_ext$deficits_surplus <- as.numeric(tempdbData_trimmed_ext$deficits_surplus)
tempdbData_trimmed_ext$form_5_opp <- as.numeric(tempdbData_trimmed_ext$form_5_opp)
tempdbData_trimmed_ext$rank_opp <- as.numeric(tempdbData_trimmed_ext$rank_opp)
tempdbData_trimmed_ext$deficits_surplus_opp <- as.numeric(tempdbData_trimmed_ext$deficits_surplus_opp)

norm_data_trimmed_ext <- tempdbData_trimmed_ext[,5:7 & 9:11]
#normalizes the data
norm_data_trimmed_ext$form_5 <- min_max_normalization(tempdbData_trimmed_ext[,5])
norm_data_trimmed_ext$rank <- min_max_normalization(tempdbData_trimmed_ext[,6])
norm_data_trimmed_ext$deficits_surplus <- min_max_normalization(tempdbData_trimmed_ext[,7])
norm_data_trimmed_ext$form_5_opp <- min_max_normalization(tempdbData_trimmed_ext[,9])
norm_data_trimmed_ext$rank_opp <- min_max_normalization(tempdbData_trimmed_ext[,10])
norm_data_trimmed_ext$deficits_surplus_opp <- min_max_normalization(tempdbData_trimmed_ext[,11])

#write.csv(norm_data_trimmed_ext,"normalized_data_no_first_five_ext.csv", quote = FALSE, row.names = FALSE)