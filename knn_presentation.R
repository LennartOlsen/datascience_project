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
