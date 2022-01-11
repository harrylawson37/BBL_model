### HARRY LAWSON ###

### BBL ball by ball database create ###

bbl_bbb <- function(files){
  
  # The following function creates a data frame with the ball by ball information
  # for the BBL.
  
  # parameter files:   vector of bbl match files
  # return:            data frame of ball by ball information
  # time complexity:   o(n*m^2) where n is the length of the files and m is the number of players
  # precondition:      files should be json files
  # post_condition:    columns in data frame should be of even length
  
  batting_row_vector = vector(mode="list", length=200000)
  
  file_length = length(file_list) - 1
  counter = 0
  ball_counter = 0
  
  for (i in 1:file_length){
    setwd("path/yourfolder/bbl_json")
    json_data <- fromJSON(file=file_list[i])
    player_info <- json_data$info$registry$people
    innings_length <- length(json_data$innings)
    if (innings_length > 2){
      innings_length = 2
    }
    
    print(file_list[i])

    for (j in 1:innings_length){
      overs_length <- length(json_data$innings[[j]]$overs)
      
      # determine powerplay 1 and 2 overs
      power_play_one_from = as.numeric(json_data$innings[[j]]$powerplays[[1]]$from)%/%1
      power_play_one_to = as.numeric(json_data$innings[[j]]$powerplays[[1]]$to)%/%1
      
      pp_one_overs = power_play_one_from:power_play_one_to
      
      if (!is.null(json_data$innings$powerplays[[2]])){
        pp_two = TRUE
        power_play_two_from = as.numeric(json_data$innings[[j]]$powerplays[[2]]$from)%/%1
        power_play_two_to = as.numeric(json_data$innings[[j]]$powerplays[[2]]$to)%/%1
        pp_two_overs = power_play_two_from:power_play_two_to
      } else {
        pp_two = FALSE
      }
      
      for (k in 1:overs_length){
        deliveries_length <- length(json_data$innings[[j]]$overs[[k]]$deliveries)
        for (l in 1:deliveries_length){
          
          # collect date info
          date = json_data$info$dates
          
          # collect over and ball info
          over = paste(as.character(k-1), ".", sep="") 
          over_delivery = paste(over, as.character(l), sep="")
          
          # collect bowler name info
          bowler_name = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$bowler
          
          # collect bowler_ID info
          bowler_ID = player_info[[bowler_name]]
          
          # collect batsman name info
          batsmen_name = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$batter
          
          # collect batsman_ID info
          batsmen_ID = player_info[[batsmen_name]]
          
          # collect batsman runs info
          runs = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$runs$batter
          
          # collect extras info
          if (!is.null(json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['wides']])){
            balls = 0
            wides = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['wides']]
            byes = 0
            no_balls = 0
            legbyes = 0
          } else if (!is.null(json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['no balls']])){
            balls = 1
            no_balls = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['no balls']]
            wides = 0
            byes = 0
            legbyes = 0
          } else if (!is.null(json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['byes']])){
            balls = 1
            byes = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['byes']]
            wides = 0
            no_balls = 0
            legbyes = 0
          } else if (!is.null(json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['legbyes']])){
            balls = 1
            legbyes = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$extras[['legbyes']]
            wides = 0
            no_balls = 0
            byes = 0
          } else {
            balls = 1
            wides = 0
            no_balls = 0
            byes = 0
            legbyes = 0
          }
          
          # collect wicket info and dismissal info
          if (!is.null(json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$wickets)){
            if (json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$wickets[[1]]$player_out == batsmen_name){
              batsman_out = TRUE
            } 
            dismissal = json_data$innings[[j]]$overs[[k]]$deliveries[[l]]$wickets[[1]]$kind
          } else {
            dismissal = 'none'
            batsman_out = FALSE
          }
          
          # collect powerplay info
          if ((as.numeric(over_delivery)%/%1) %in% pp_one_overs){
            powerplay = TRUE
          } else {
            if (pp_two == TRUE){
              if ((as.numeric(over_delivery)%/%1) %in% pp_two_overs){
                powerplay = TRUE
              } else {
                powerplay = FALSE
              }
            } else {
              powerplay = FALSE
            }
          }
          
          # collect stadium info
          stadium = json_data$info$venue
          
          # collect team info
          team_one = json_data$info$teams[1]
          team_two = json_data$info$teams[2]
          players_one = json_data$info$players[[team_one]]
          players_two = json_data$info$players[[team_two]]

          if (batsmen_name %in% players_one){
            batting_number = match(batsmen_name, players_one)
            team = team_one
            opposing_team = team_two
          } else {
            batting_number = match(batsmen_name, players_two)
            team = team_two
            opposing_team = team_one
          }
          
          ball_counter = ball_counter + 1
          bat_row_data <- c(date, over_delivery, batsmen_name, batsmen_ID, bowler_name, bowler_ID, runs, balls, wides, no_balls, byes, legbyes, batsman_out, dismissal, powerplay, stadium, team, opposing_team, batting_number)
          batting_row_vector[[ball_counter]] = bat_row_data
        }
      }
    }
  }
  
  batting_row_vector
  batting_row_matrix = do.call(rbind, batting_row_vector)
  x <- c("date", "over_delivery", "batsman_name", "batsman_ID", "bowler_name", "bowler_ID", "runs", "balls", "wides", "no_balls", "byes", "legbyes", "batsman_out", "dismissal", "powerplay", "stadium", "batsman_team", "bowling_team", "batting_number")
  colnames(batting_row_matrix) <- x
  batting_row_dataframe = as.data.frame(batting_row_matrix)

  return(batting_row_dataframe)

}

library(rjson)

# create a list of the files from your target directory
file_list <- list.files(path=setwd("path/yourfolder/bbl_json"))

file_list
length(file_list)

df <- bbl_bbb(file_list)
View(df)

# create batting_hand, bowling_arm, bowling_type for dataset

setwd("path/yourfolder")

player_info <- read.csv("bbl_player_ID_MAIN.csv")

View(player_info)

df_length = length(df$date)

df$batting_hand = 'none'
df$bowling_arm = 'none'
df$bowling_type = 'none'

for (i in 1:df_length){
  curr_bat = df$batsman_ID[i]
  curr_bowl = df$bowler_ID[i]
  player_IDs = player_info$player_ID
  bat_index = match(curr_bat, player_IDs)
  bowl_index = match(curr_bowl, player_IDs)
  df$batting_hand[i] = player_info$batting_hand[bat_index]
  df$bowling_arm[i] = player_info$bowling_arm[bowl_index]
  df$bowling_type[i] = player_info$bowling_type[bowl_index]
}

# remove different names for same stadiums

unique(df$stadium)

for ( i in 1:df_length){
  if (df$stadium[i] == "Docklands Stadium"){
    df$stadium[i] = "Docklands Stadium, Melbourne"
  } else if (df$stadium[i] == "Aurora Stadium"){
    df$stadium[i] = "Aurora Stadium, Launceston"
  } else if (df$stadium[i] == "Manuka Oval"){
    df$stadium[i] = "Manuka Oval, Canberra"
  } else if (df$stadium[i] == "Brisbane Cricket Ground"){
    df$stadium[i] = "Brisbane Cricket Ground, Woolloongabba"
  }
}

View(df)
unique(df$stadium)

write.csv(df, 'bbl_bbb_final.csv')
