### Harry Lawson ###

### BBL update the player info and ball by ball information database

update_player_info <- function(){
  
  # This function updates the player information database by reading through every
  # BBL game and checking for any new players. If new players are found, the program
  # will prompt the user to input some player details and will update the database.
  
  # time complexity:   o(n*m) where n is the number of balls in BBL history and m is the number of players
  # precondition:      all bbl game files should be added to the bbl_json folder
  # post_condition:    old database file should be saved as a copy and database should be correctly updated
  
  setwd("path/yourfolder/bbl_json")
  
  file_list <- list.files(path=setwd("path/yourfolder/bbl_json"))
  
  player_ID = vector(mode="character", length=10000) 
  player_name = vector(mode='character', length=10000)
  file_length = length(file_list) - 1
  counter = 0
  
  # get a list of player names and player ID's from every BBL game
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
  
  setwd("path/yourfolder")
  
  player_names <- data.frame(player_name, player_ID)

  player_names_info = read.csv('bbl_player_ID_MAIN.csv')
  
  # make a copy of player info database in case an error is made
  write.csv(player_names_info, 'bbl_player_ID_MAIN_COPY.csv', row.names = FALSE)
  
  # check if there are any new players by comparing the player database and the newly created player name/ID vectors
  new_players_name = c()
  new_players_ID = c()
  new_players_bowling_arm = c()
  new_players_bowling_type = c()
  new_players_batting_hand = c()
  new_players_country = c()
  
  for (i in 1:length(player_names$player_name)){
    if (!(player_names$player_name[i] %in% player_names_info$player_name)){
      new_players_name = append(new_players_name, player_names$player_name[i])
      new_players_ID = append(new_players_ID, player_names$player_ID[i])
    }
  }
  
  # show new players to check for errors
  print(new_players_name)
  
  # add in bowling arm, bowling type, batting hand and country through input
  if (!is.null(new_players_name)){
    for (i in 1:length(new_players_name)){
      while (correct_details == FALSE){
        bowling_arm <- readline(prompt=paste("NEW PLAYER ", new_players_name[i], "'s Bowling Arm: ", sep = ''))
        bowling_type <- readline(prompt=paste("NEW PLAYER ", new_players_name[i], "'s Bowling Type: ", sep = ''))
        batting_hand <- readline(prompt=paste("NEW PLAYER ", new_players_name[i], "'s Batting Hand: ", sep = ''))
        country <- readline(prompt=paste("NEW PLAYER ", new_players_name[i], "'s Country: ", sep = ''))
        answer <- readline(prompt="Continue? ")
        if (answer == "Y"){
          correct_details = TRUE
          new_players_bowling_arm = append(new_players_bowling_arm, bowling_arm)
          new_players_bowling_type = append(new_players_bowling_type, bowling_type)
          new_players_batting_hand = append(new_players_batting_hand, batting_hand)
          new_players_country = append(new_players_country, country)
        } else {
          correct_details = FALSE
        }
      }
      correct_details = FALSE
    }
    
    # update player_info data frame
    for (i in 1:length(new_players_name)){
      player_names_info[nrow(player_names_info) + 1,] = c(new_players_name[i], new_players_ID[i], new_players_bowling_arm[i], new_players_bowling_type[i], new_players_batting_hand[i], new_players_country[i])
    }
  }
  
  # update player info database
  write.csv(player_names_info, 'bbl_player_ID_MAIN.csv', row.names = FALSE)
}

update_bbl_bbb <- function(){
  
  # The following function creates a data frame with the ball by ball information
  # for the BBL.
  
  # time complexity:   o(n*m) where n is the number of balls in BBL history and m is the number of players
  # precondition:      paths should be setup correctly and player information database should be created
  # post_condition:    bbl_bbb_final.csv is created 
  
  setwd("path/yourfolder/bbl_json")
  
  # collect list of files
  file_list <- list.files(path=setwd("path/yourfolder/bbl_json"))
  
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
    
    for (j in 1:innings_length){
      overs_length <- length(json_data$innings[[j]]$overs)
      
      power_play_one_from = as.numeric(json_data$innings[[j]]$powerplays[[1]]$from)%/%1
      power_play_one_to = as.numeric(json_data$innings[[j]]$powerplays[[1]]$to)%/%1
      
      pp_one_overs = power_play_one_from:power_play_one_to
      
      # determine powerplay 1 and 2 overs
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
  df = as.data.frame(batting_row_matrix)
  
  setwd("path/yourfolder")
  
  player_info <- read.csv("bbl_player_ID_MAIN.csv")
  
  df_length = length(df$date)
  
  # include player info (batting hand, bowling arm, bowling type) in ball by ball information
  
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
  
  for ( i in 1:df_length){
    if (df$stadium[i] == "Docklands Stadium, Melbourne"){
      df$stadium[i] = "Docklands Stadium"
    } else if (df$stadium[i] == "Aurora Stadium, Launceston"){
      df$stadium[i] = "Aurora Stadium"
    } else if (df$stadium[i] == "Manuka Oval, Canberra"){
      df$stadium[i] = "Manuka Oval"
    } else if (df$stadium[i] == "Brisbane Cricket Ground, Woolloongabba"){
      df$stadium[i] = "Brisbane Cricket Ground"
    } else if (df$stadium[i] == "Brisbane Cricket Ground, Woolloongabba, Brisbane"){
      df$stadium[i] = "Brisbane Cricket Ground"
    } else if (df$stadium[i] == "Bellerive Oval, Hobart"){
      df$stadium[i] = "Bellerive Oval"
    } else if (df$stadium[i] == "Western Australia Cricket Association Ground"){
      df$stadium[i] = "W.A.C.A. Ground"
    } else if (df$stadium[i] == "Simonds Stadium, South Geelong, Victoria"){
      df$stadium[i] = "GMHBA Stadium, South Geelong, Victoria"
    } 
  }
  
  # make a copy of the old csv file in case an error is made
  old = read.csv('bbl_bbb_final.csv')
  write.csv(old, 'bbl_bbb_final_COPY.csv', row.names = FALSE)
  
  write.csv(df, 'bbl_bbb_final.csv')
}

update_bbl_json = function(){
  
  # This function updates the bbl match files from cricsheet.org and places them 
  # the bbl_json folder
  
  setwd("path/yourfolder/bbl_json")
  
  # WARNING: MAKE SURE PATH IS CORRECT BEFORE RUNNING FOLLOWING LINE
  unlink("path/yourfolder/bbl_json/*")
  
  url = "https://cricsheet.org/downloads/bbl_json.zip"
  
  download.file(url, "bbldata.zip")
  
  unzip("bbldata.zip")
  
  unlink("path/yourfolder/bbl_json/bbldata.zip")
  
  setwd("path/yourfolder")
  
}

update_bbl_json()
update_player_info()
update_bbl_bbb()

