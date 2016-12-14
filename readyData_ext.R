#source("base.r")
#source("base2.r")

#Data conversion and processing
#--------------------------------------------------------------------------------------------
size <-nrow(trim_dbData)
tempdbData_ext <- data.frame(league_id = integer(size), trim_dbData$season, trim_dbData$stage,
                         trim_dbData$home_team_name, form_5= integer(size),
                         rank = integer(size), deficits_surplus = integer(size),
                         trim_dbData$away_team_name, form_5_opp= integer(size),
                         rank_opp = integer(size), deficits_surplus_opp = integer(size),
                         trim_dbData$B365H)

start.time <- Sys.time()
# optimize loop search by defining the number of stages in a league, and break for loop when
# the number of matches needed is found
for(i in 1:size){
  start.function.time <- Sys.time()
  
  if(trim_dbData$stage[i] > 5){
    form_5 <- current_form(trim_dbData$date[i]-1, trim_dbData$league_id[i], trim_dbData$home_team_name[i], 5)
    rank <- team_rank_specific_stage(trim_dbData$season[i], as.numeric(trim_dbData$stage[i]), trim_dbData$league_id[i], trim_dbData$home_team_name[i])
    d_s <- team_deficit_specific_date(trim_dbData$season[i], trim_dbData$date[i]-1, trim_dbData$league_id[i], trim_dbData$home_team_name[i])
    form_5_away <- current_form(dbData$date[i]-1, dbData$league_id[i], trim_dbData$away_team_name[i], 5)
    rank_away <- team_rank_specific_stage(trim_dbData$season[i], as.numeric(trim_dbData$stage[i]), trim_dbData$league_id[i], trim_dbData$away_team_name[i])
    d_s_away <- team_deficit_specific_date(trim_dbData$season[i], trim_dbData$date[i]-1, trim_dbData$league_id[i], trim_dbData$away_team_name[i])
    tempdbData_ext[i,] <- c(trim_dbData$league_id[i], trim_dbData$season[i], as.numeric(trim_dbData$stage[i]), trim_dbData$home_team_name[i], form_5,
                        rank, d_s, trim_dbData$away_team_name[i], form_5_away, rank_away, d_s_away, trim_dbData$B365H[i])
  }
  print(i);
  end.function.time <- Sys.time()
  print(end.function.time - start.function.time)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

tempdbData_trimmed_ext <- tempdbData_ext[trim_dbData$stage > 5,]

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

# norm_data_ext <- tempdbData_ext[,5:7 & 9:11]
# #normalizes the data
# norm_data_ext$form_5 <- min_max_normalization(tempdbData_ext[,5])
# norm_data_ext$rank <- min_max_normalization(tempdbData_ext[,6])
# norm_data_ext$deficits_surplus <- min_max_normalization(tempdbData_ext[,7])
# norm_data_ext$form_5_opp <- min_max_normalization(tempdbData_ext[,9])
# norm_data_ext$rank_opp <- min_max_normalization(tempdbData_ext[,10])
# norm_data_ext$deficits_surplus_opp <- min_max_normalization(tempdbData_ext[,11])

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
tempdbData_trimmed_ext$B365H <- as.numeric(tempdbData_trimmed_ext$B365H)

norm_data_trimmed_ext <- tempdbData_trimmed_ext[,c(5,6,7,9,10,11,12)]
#normalizes the data
norm_data_trimmed_ext$form_5 <- min_max_normalization(tempdbData_trimmed_ext[,5])
norm_data_trimmed_ext$rank <- min_max_normalization(tempdbData_trimmed_ext[,6])
norm_data_trimmed_ext$deficits_surplus <- min_max_normalization(tempdbData_trimmed_ext[,7])
norm_data_trimmed_ext$form_5_opp <- min_max_normalization(tempdbData_trimmed_ext[,9])
norm_data_trimmed_ext$rank_opp <- min_max_normalization(tempdbData_trimmed_ext[,10])
norm_data_trimmed_ext$deficits_surplus_opp <- min_max_normalization(tempdbData_trimmed_ext[,11])
norm_data_trimmed_ext$B365H <- min_max_normalization(tempdbData_trimmed_ext[,12])


#write.csv(norm_data_trimmed_ext,"normalized_data_no_first_five_ext.csv", quote = FALSE, row.names = FALSE)

#other way to normalize, normalize the entire dataset in one instead of each column
#norm_data_trimmed_ext2 <- min_max_normalization(tempdbData_trimmed_ext)

# Standardized data
#----------------------------------------------------------------------------------------------
# function for z-score standardization
zscore_standardization <- function(x){
  standardized = (x - mean)/std
  return(standardized)
}