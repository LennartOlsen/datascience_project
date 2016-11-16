#source("base.r")
source("base2.r")

#Data conversion and processing
#--------------------------------------------------------------------------------------------
size <-nrow(dbData)
tempdbData <- data.frame(league_id = integer(size), dbData$season, dbData$stage,
                           dbData$home_team_name, form_5= integer(size),
                           rank = integer(size), deficits_surplus = integer(size))

start.time <- Sys.time()
# optimize loop search by defining the number of stages in a league, and break for loop when
# the number of matches needed is found
for(i in 1:size){
  start.function.time <- Sys.time()

  if(dbData$stage[i] > 5){
      form_5 <- current_form(dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i], 5)
      rank <- team_rank_specific_stage(dbData$season[i], dbData$stage[i], dbData$league_id[i], dbData$home_team_name[i])
      d_s <- team_deficit_specific_date(dbData$season[i], dbData$date[i]-1, dbData$league_id[i], dbData$home_team_name[i])
      tempdbData[i,] <- c(dbData$league_id[i], dbData$season[i], as.numeric(dbData$stage[i]), dbData$home_team_name[i], form_5,
                          rank, d_s)
  }
  print(i);
  end.function.time <- Sys.time()
  print(end.function.time - start.function.time)
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

knnData <- data.frame(tempdbData[,5:7])

#Normalization of data
#--------------------------------------------------------------------------------------------
min_max_normalization <- function(x){
  normalized <- (x - min(x))/(max(x)-min(x))
  return(normalized)
}
# makes data numeric
tempdbData$form_5 <- as.numeric(tempdbData$form_5)
tempdbData$rank <- as.numeric(tempdbData$rank)
tempdbData$deficits_surplus <- as.numeric(tempdbData$deficits_surplus)

norm_data <- tempdbData[,5:7]
#normalizes the data
norm_data$form_5 <- min_max_normalization(tempdbData[,5])
norm_data$rank <- min_max_normalization(tempdbData[,6])
norm_data$deficits_surplus <- min_max_normalization(tempdbData[,7])

#write.csv(norm_data,"normalized_data.csv", quote = FALSE, row.names = FALSE)
