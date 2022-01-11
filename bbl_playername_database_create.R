### HARRY LAWSON  ###

### BBL Database Create ###
### This creates all the players csv files and the player ID/name location ###

### DO NOT RUN ###

# install.packages('rjson')
library(rjson)

name_ID <- function(files){
  
  # The following function creates a data frame and adds all the player names 
  # and player ID's from every BBL match.
  
  # parameter files:   list of files
  # return:            data frame of player names and player ID's
  # time complexity:   o(n*m^2) where n is the length of the files and m is the number of players
  # precondition:      files should be json files
  # post_condition:     columns in data frame should be of even length
  
  player_ID = vector(mode="character", length=10000) 
  player_name = vector(mode='character', length=10000)
  file_length = length(file_list) - 1
  counter = 0
  
  # go through every game and add a player name and player ID if not already in vector
  
  for (i in 1:file_length){
    json_data <- fromJSON(file=file_list[i])
    player_info <- json_data$info$registry$people
    innings_length <- length(json_data$innings)
    for (j in 1:innings_length){
      overs_length <- length(json_data$innings[[j]]$overs)
      for (k in 1:overs_length){
        deliveries_length <- length(json_data$innings[[j]]$overs[[k]]$deliveries)
        for (l in 1:deliveries_length){
          bowler_name = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$bowler
          bowler_ID = player_info[[bowler_name]]
          if (!(bowler_ID %in% player_ID)){
            counter = counter + 1
            player_ID[counter] = bowler_ID
            player_name[counter] = bowler_name
          } 
          batsmen_name = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$batter
          batsmen_ID = player_info[[batsmen_name]]
          if (!(batsmen_ID %in% player_ID)){
            counter = counter + 1
            player_ID[counter] = batsmen_ID
            player_name[counter] = batsmen_name
          } 
        }
      }
    }
  }
  player_name = player_name[player_name != ""]
  player_ID = player_ID[player_ID != ""]
  
  df <- data.frame(player_name, player_ID)
  return(df)
}


# create a list of the files from your target directory

file_list <- list.files(path=setwd("/path/folder/bbl_json"))

file_list

df <- name_ID(file_list)

setwd("/path/folder")

write.csv(df, 'bbl_player_ID.csv', row.names = FALSE)


