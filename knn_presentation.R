soccer <- read.csv("norm_data_no_first_five_ext2.csv", header = TRUE)  ##Reads the CSV file and specifies that no header is present
#requires to source the base.r file
soccer$w_l_d <- trim_dbData$w_l_d_home
for(i in 1:nrow(soccer)){
  if(soccer$w_l_d[i] == 0)
    soccer$w_l_d[i] = "win"
  if(soccer$w_l_d[i] == 1)
    soccer$w_l_d[i] = "loose"
  if(soccer$w_l_d[i] == 2)
    soccer$w_l_d[i] = "draw"
}
soccer %>% ggvis(~deficits_surplus, ~deficits_surplus_opp, fill = ~w_l_d,  opacity := 0.2) %>% layer_points() ##Look up ggvis, makes awesome scatter plots


#Best k for knn 274
pro_league <- as.data.frame(c())
pro_league$form_5 <- c(0.2666666667, 0.6666666667)
pro_league$rank <- c(0.3959731544, 0.2631578947)
pro_league$deficit <- c(0.3959731544, 0.3825503356)
pro_league$form_5_opp <- c(0.3333333333, 0.7333333333)
pro_league$rank_opp <- c(0.6315789474, 0.1052631579)
pro_league$deficit_opp <- c(0.3624161074, 0.4362416107)
soccer_pred <- knn(soccer.training, pro_league, cl = soccer.trainLabels, k=162)
