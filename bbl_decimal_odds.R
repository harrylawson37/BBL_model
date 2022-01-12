### Harry Lawson ###

source('bbl_MC_simulation_log5.R')

get_decimal_odds <- function(df, team_one_name, team_two_name, team_one_players, team_two_players, team_one_bowling_order, team_two_bowling_order, MC_simulations, stadium = FALSE){
  
  # This function takes the data created from bbl_innings_MC and formats it into 
  # a decimal odds format and renames the data frames so it is readable.
  
  # return:                  A list of reformatted data frames created by bbl_innings_MC 
  # parameter team_one_name: The name of team one (character)
  # parameter team_two_name: The name of team two (character)
  
  # all other parameters are explained in bbl_innings_MC
  
  the_final_simulation = bbl_innings_MC(df, team_one_players, team_two_players, team_one_bowling_order, team_two_bowling_order, MC_simulations, stadium)
  
  team_one_batting_plus = the_final_simulation$t1_batting_plus/MC_simulations
  team_two_batting_plus = the_final_simulation$t2_batting_plus/MC_simulations
  team_one_bowling_stats = the_final_simulation$t1_bowling/MC_simulations
  team_two_bowling_stats = the_final_simulation$t2_bowling/MC_simulations
  team_one_batting_boundaries = the_final_simulation$t1_batting_boundaries/MC_simulations
  team_two_batting_boundaries = the_final_simulation$t2_batting_boundaries/MC_simulations
  team_one_batting_runs_line_over = the_final_simulation$t1_batting_runs_line_over/MC_simulations
  team_two_batting_runs_line_over = the_final_simulation$t2_batting_runs_line_over/MC_simulations
  team_one_batting_runs_line_under = the_final_simulation$t1_batting_runs_line_under/MC_simulations
  team_two_batting_runs_line_under = the_final_simulation$t2_batting_runs_line_under/MC_simulations
  
  final_decimal_odds = list()
  final_decimal_odds$team_one_decimal_odds = c(team_one_name, 1/(the_final_simulation$t1_wins/(the_final_simulation$t1_wins+the_final_simulation$t2_wins)))
  final_decimal_odds$team_two_decimal_odds = c(team_two_name, 1/(the_final_simulation$t2_wins/(the_final_simulation$t1_wins+the_final_simulation$t2_wins)))
  
  t1_final_batting_plus <- data.frame(player_name=character(11), balls_average=numeric(11), runs_average=numeric(11), 
                                      plus_ten_odds=numeric(11), plus_twenty_odds=numeric(11), plus_thirty_odds=numeric(11), 
                                      plus_fourty_odds=numeric(11), plus_fifty_odds=numeric(11), top_batting_odds=numeric(11))
  
  colnames(t1_final_batting_plus) = c("Player Name", "Balls Faced Average", "Runs Average", "Plus 10 Odds", "Plus 20 Odds",
                                      "Plus 30 Odds", "Plus 40 Odds", "Plus 50 Odds", "Top Runscorer Odds")
  
  
  t2_final_batting_plus <- data.frame(player_name=character(11), balls_average=numeric(11), runs_average=numeric(11), 
                                      plus_ten_odds=numeric(11), plus_twenty_odds=numeric(11), plus_thirty_odds=numeric(11), 
                                      plus_fourty_odds=numeric(11), plus_fifty_odds=numeric(11), top_batting_odds=numeric(11))
  
  colnames(t2_final_batting_plus) = c("Player Name", "Balls Faced Average", "Runs Average", "Plus 10 Odds", "Plus 20 Odds",
                                      "Plus 30 Odds", "Plus 40 Odds", "Plus 50 Odds", "Top Runscorer Odds")
  
  t1_final_batting_boundaries <- data.frame(player_name=character(11), fours_average=numeric(11), six_average=numeric(11), 
                                            under_2.5_fours_odds=numeric(11), over_2.5_fours_odds=numeric(11), under_0.5_sixes_odds=numeric(11), 
                                            over_0.5_sixes_odds=numeric(11))
  
  colnames(t1_final_batting_boundaries) = c("Player Name", "Fours Average", "Sixes Average", "Under 2.5 Fours Odds", "Over 2.5 Fours Odds",
                                      "Under 0.5 Six's Odds", "Over 0.5 Six's Odds")
  
  t2_final_batting_boundaries <- data.frame(player_name=character(11), fours_average=numeric(11), six_average=numeric(11), 
                                            under_2.5_fours_odds=numeric(11), over_2.5_fours_odds=numeric(11), under_0.5_sixes_odds=numeric(11), 
                                            over_0.5_sixes_odds=numeric(11))
  
  colnames(t2_final_batting_boundaries) = c("Player Name", "Fours Average", "Sixes Average", "Under 2.5 Fours Odds", "Over 2.5 Fours Odds",
                                            "Under 0.5 Six's Odds", "Over 0.5 Six's Odds")
  
  t1_final_runs_over <- data.frame(player_name=character(11), r15.5=numeric(11), r16.5=numeric(11), r17.5=numeric(11), r18.5=numeric(11), 
                                   r19.5=numeric(11), r20.5=numeric(11), r21.5=numeric(11), r22.5=numeric(11),
                                   r23.5=numeric(11), r24.5=numeric(11), r25.5=numeric(11), r26.5=numeric(11),
                                   r27.5=numeric(11), r28.5=numeric(11), r29.5=numeric(11), r30.5=numeric(11),
                                   r31.5=numeric(11), r32.5=numeric(11), r33.5=numeric(11), r34.5=numeric(11),
                                   r35.5=numeric(11), r36.5=numeric(11), r37.5=numeric(11), r38.5=numeric(11))
  
  colnames(t1_final_runs_over) = c("Player Name", "Over 15.5", "Over 16.5", "Over 17.5", "Over 18.5", "Over 19.5",
                                   "Over 20.5", "Over 21.5", "Over 22.5", "Over 23.5", "Over 24.5", "Over 25.5", 
                                   "Over 26.5", "Over 27.5", "Over 28.5", "Over 29.5", "Over 30.5", "Over 31.5",
                                   "Over 32.5", "Over 33.5", "Over 34.5", "Over 35.5", "Over 36.5", "Over 37.5",
                                   "Over 38.5")
  
  t2_final_runs_over <- data.frame(player_name=character(11), r15.5=numeric(11), r16.5=numeric(11), r17.5=numeric(11), r18.5=numeric(11), 
                                 r19.5=numeric(11), r20.5=numeric(11), r21.5=numeric(11), r22.5=numeric(11),
                                 r23.5=numeric(11), r24.5=numeric(11), r25.5=numeric(11), r26.5=numeric(11),
                                 r27.5=numeric(11), r28.5=numeric(11), r29.5=numeric(11), r30.5=numeric(11),
                                 r31.5=numeric(11), r32.5=numeric(11), r33.5=numeric(11), r34.5=numeric(11),
                                 r35.5=numeric(11), r36.5=numeric(11), r37.5=numeric(11), r38.5=numeric(11))
  
  colnames(t2_final_runs_over) = c("Player Name", "Over 15.5", "Over 16.5", "Over 17.5", "Over 18.5", "Over 19.5",
                                   "Over 20.5", "Over 21.5", "Over 22.5", "Over 23.5", "Over 24.5", "Over 25.5", 
                                   "Over 26.5", "Over 27.5", "Over 28.5", "Over 29.5", "Over 30.5", "Over 31.5",
                                   "Over 32.5", "Over 33.5", "Over 34.5", "Over 35.5", "Over 36.5", "Over 37.5",
                                   "Over 38.5")
  
  t1_final_runs_under <- data.frame(player_name=character(11), r15.5=numeric(11), r16.5=numeric(11), r17.5=numeric(11), r18.5=numeric(11), 
                                   r19.5=numeric(11), r20.5=numeric(11), r21.5=numeric(11), r22.5=numeric(11),
                                   r23.5=numeric(11), r24.5=numeric(11), r25.5=numeric(11), r26.5=numeric(11),
                                   r27.5=numeric(11), r28.5=numeric(11), r29.5=numeric(11), r30.5=numeric(11),
                                   r31.5=numeric(11), r32.5=numeric(11), r33.5=numeric(11), r34.5=numeric(11),
                                   r35.5=numeric(11), r36.5=numeric(11), r37.5=numeric(11), r38.5=numeric(11))
  
  colnames(t1_final_runs_under) = c("Player Name", "Under 15.5", "Under 16.5", "Under 17.5", "Under 18.5", "Under 19.5",
                                   "Under 20.5", "Under 21.5", "Under 22.5", "Under 23.5", "Under 24.5", "Under 25.5", 
                                   "Under 26.5", "Under 27.5", "Under 28.5", "Under 29.5", "Under 30.5", "Under 31.5",
                                   "Under 32.5", "Under 33.5", "Under 34.5", "Under 35.5", "Under 36.5", "Under 37.5",
                                   "Under 38.5")
  
  t2_final_runs_under <- data.frame(player_name=character(11), r15.5=numeric(11), r16.5=numeric(11), r17.5=numeric(11), r18.5=numeric(11), 
                                   r19.5=numeric(11), r20.5=numeric(11), r21.5=numeric(11), r22.5=numeric(11),
                                   r23.5=numeric(11), r24.5=numeric(11), r25.5=numeric(11), r26.5=numeric(11),
                                   r27.5=numeric(11), r28.5=numeric(11), r29.5=numeric(11), r30.5=numeric(11),
                                   r31.5=numeric(11), r32.5=numeric(11), r33.5=numeric(11), r34.5=numeric(11),
                                   r35.5=numeric(11), r36.5=numeric(11), r37.5=numeric(11), r38.5=numeric(11))
  
  colnames(t2_final_runs_under) = c("Player Name", "Under 15.5", "Under 16.5", "Under 17.5", "Under 18.5", "Under 19.5",
                                    "Under 20.5", "Under 21.5", "Under 22.5", "Under 23.5", "Under 24.5", "Under 25.5", 
                                    "Under 26.5", "Under 27.5", "Under 28.5", "Under 29.5", "Under 30.5", "Under 31.5",
                                    "Under 32.5", "Under 33.5", "Under 34.5", "Under 35.5", "Under 36.5", "Under 37.5",
                                    "Under 38.5")
  
  t1_final_bowling_stats_df <- data.frame(player_name=character(11), balls_average=numeric(11), runs_average=numeric(11), 
                                          wickets_average=numeric(11), top_bowling_odds=numeric(11))
  
  colnames(t1_final_bowling_stats_df) = c("Player Name", "Balls Bowled Average", "Runs Against Average", "Wickets Average", "Top Wickettaker Odds")
  
  t2_final_bowling_stats_df <- data.frame(player_name=character(11), balls_average=numeric(11), runs_average=numeric(11), 
                                          wickets_average=numeric(11), top_bowling_odds=numeric(11))
  
  colnames(t2_final_bowling_stats_df) = c("Player Name", "Balls Bowled Average", "Runs Against Average", "Wickets Average", "Top Wickettaker Odds")

  
  for (i in 1:11){
    t1_final_batting_plus[i,1] = team_one_players[i]
    t1_final_batting_plus[i,2] = team_one_batting_plus[i, 1]
    t1_final_batting_plus[i,3] = team_one_batting_plus[i, 2]
    t1_final_batting_plus[i,4] = 1/team_one_batting_plus[i, 3]
    t1_final_batting_plus[i,5] = 1/team_one_batting_plus[i, 4]
    t1_final_batting_plus[i,6] = 1/team_one_batting_plus[i, 5]
    t1_final_batting_plus[i,7] = 1/team_one_batting_plus[i, 6]
    t1_final_batting_plus[i,8] = 1/team_one_batting_plus[i, 7]
    t1_final_batting_plus[i,9] = 1/team_one_batting_plus[i, 8]
    
    t2_final_batting_plus[i,1] = team_two_players[i]
    t2_final_batting_plus[i,2] = team_two_batting_plus[i, 1]
    t2_final_batting_plus[i,3] = team_two_batting_plus[i, 2]
    t2_final_batting_plus[i,4] = 1/team_two_batting_plus[i, 3]
    t2_final_batting_plus[i,5] = 1/team_two_batting_plus[i, 4]
    t2_final_batting_plus[i,6] = 1/team_two_batting_plus[i, 5]
    t2_final_batting_plus[i,7] = 1/team_two_batting_plus[i, 6]
    t2_final_batting_plus[i,8] = 1/team_two_batting_plus[i, 7]
    t2_final_batting_plus[i,9] = 1/team_two_batting_plus[i, 8]
    
    t1_final_batting_boundaries[i, 1] = team_one_players[i]
    t1_final_batting_boundaries[i, 2] = team_one_batting_boundaries[i, 1]
    t1_final_batting_boundaries[i, 3] = team_one_batting_boundaries[i, 2]
    t1_final_batting_boundaries[i, 4] = 1/(1-team_one_batting_boundaries[i, 3])
    t1_final_batting_boundaries[i, 5] = 1/team_one_batting_boundaries[i, 3]
    t1_final_batting_boundaries[i, 6] = 1/(1-team_one_batting_boundaries[i, 4])
    t1_final_batting_boundaries[i, 7] = 1/team_one_batting_boundaries[i, 4]
    
    t2_final_batting_boundaries[i, 1] = team_two_players[i]
    t2_final_batting_boundaries[i, 2] = team_two_batting_boundaries[i, 1]
    t2_final_batting_boundaries[i, 3] = team_two_batting_boundaries[i, 2]
    t2_final_batting_boundaries[i, 4] = 1/(1-team_two_batting_boundaries[i, 3])
    t2_final_batting_boundaries[i, 5] = 1/team_two_batting_boundaries[i, 3]
    t2_final_batting_boundaries[i, 6] = 1/(1-team_two_batting_boundaries[i, 4])
    t2_final_batting_boundaries[i, 7] = 1/team_two_batting_boundaries[i, 4]
    
    t1_final_runs_over[i, 1] = team_one_players[i]
    t2_final_runs_over[i, 1] = team_two_players[i]
    t1_final_runs_under[i, 1] = team_one_players[i]
    t2_final_runs_under[i, 1] = team_two_players[i]
    
    for (h in 1:24){
      t1_final_runs_over[i, h+1] = 1/team_one_batting_runs_line_over[i, h]
      t2_final_runs_over[i, h+1] = 1/team_two_batting_runs_line_over[i, h]
      t1_final_runs_under[i, h+1] = 1/team_one_batting_runs_line_under[i, h]
      t2_final_runs_under[i, h+1] = 1/team_two_batting_runs_line_under[i, h]
    }
    
    t1_final_bowling_stats_df[i,1] = team_one_players[i]
    t1_final_bowling_stats_df[i,2] = team_one_bowling_stats[i, 1]
    t1_final_bowling_stats_df[i,3] = team_one_bowling_stats[i, 2]
    t1_final_bowling_stats_df[i,4] = team_one_bowling_stats[i, 3]
    t1_final_bowling_stats_df[i,5] = 1/team_one_bowling_stats[i, 4]
    
    t2_final_bowling_stats_df[i,1] = team_two_players[i]
    t2_final_bowling_stats_df[i,2] = team_two_bowling_stats[i, 1]
    t2_final_bowling_stats_df[i,3] = team_two_bowling_stats[i, 2]
    t2_final_bowling_stats_df[i,4] = team_two_bowling_stats[i, 3]
    t2_final_bowling_stats_df[i,5] = 1/team_two_bowling_stats[i, 4]
  }
  
  final_decimal_odds$t1_batting_plus = t1_final_batting_plus
  final_decimal_odds$t2_batting_plus = t2_final_batting_plus
  final_decimal_odds$t1_batting_boundaries = t1_final_batting_boundaries
  final_decimal_odds$t2_batting_boundaries = t2_final_batting_boundaries
  final_decimal_odds$t1_batting_runs_line_over = t1_final_runs_over
  final_decimal_odds$t2_batting_runs_line_over = t2_final_runs_over
  final_decimal_odds$t1_batting_runs_line_under = t1_final_runs_under
  final_decimal_odds$t2_batting_runs_line_under = t2_final_runs_under
  final_decimal_odds$bowling_team_one = t1_final_bowling_stats_df
  final_decimal_odds$bowling_team_two = t2_final_bowling_stats_df
  
  t1_game_odds = data.frame(c(final_decimal_odds$team_one_decimal_odds[1], final_decimal_odds$team_two_decimal_odds[1]), 
                            c(as.numeric(final_decimal_odds$team_one_decimal_odds[2]), as.numeric(final_decimal_odds$team_two_decimal_odds[2])))
  
  colnames(t1_game_odds) = c("Team", "Odds")
  
  final_decimal_odds$odds_df = t1_game_odds
  
  return(final_decimal_odds)
}
