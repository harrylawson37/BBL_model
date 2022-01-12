# BBL Model 
### A model used to create competitive odd's on team and player sports betting markets for bbl matches.

## Personal Info
I'm a Data Scientist with a keen interest for sports and I have created this model to practice my skills programming in R by applying a variety of simple data analysis techniques. Email me at harrylawson.waters@gmail.com for any questions or suggestions.

DISCLAIMER: This model has been created to practice my data science skills and I do not encourage sports betting. The model is something I complete in my spare time and I am still in the process of fine tuning it.

## Process of Creating the Model
The process of creating the model was completed by collecting data, wrangling data, creating relevant functions for player statistics, creating a function for the simulation of a ball, creating a function for the simulation of an innings, creating a monte carlo function to simulate matches, creating a function to determine different sports betting markets from the simulation and creating function to generate an excel file with the results.

#### Data Collection and Wrangling
All the data collected by automation to create this model is from cricsheet.org. The folder bbl_json contains every bbl match and is directly download from https://cricsheet.org/downloads/bbl_json.zip.

The files **bbl_bbb_database_create.R**, **bbl_bbb_database_update.R** and **bbl_bbb_playername_database_create.R** are all used to update the player information database and the ball by ball information. 

The file **bbl_bbb_final.csv** contain the ball by ball information for every bbl game.
Information from espncricinfo is used to manually add in a new player's batting hand, bowling arm, bowling type and country to **bbl_player_ID_MAIN.csv**. I have chosen to do this manually because it requires little time.

To retrieve the files from cricsheet.org and update the databases run the following code in **bbl_bbb_database_update.R**
```
update_bbl_json()
update_player_info()
update_bbl_bbb()
```

#### Functions for Player Statistics
The file **bbl_likelihood_functions.R** contains the relevant functions to simulate any ball between a bowler and batsman at a particular over and cricket ground.

The functions use the last x number of balls the bowler or batsman have faced. The value of x can be altered depending on how much recent form is weighted against a larger dataset. They also use a shrinkage estimator for players with an insufficient amount of data by moving a players statistical average towards the statistical average of that players bowling type or batting number. 

The following player statistics functions are used
```
wicket_likelihood(df)
one_likelihood(df)
two_likelihood(df)
three_likelihood(df)
four_likelihood(df)
five_likelihood(df)
six_likelihood(df)
wickets_at_over_likelihood(df, over, stadium = FALSE)
runs_at_over_likelihood(df, runs, over, stadium = FALSE)
bowler_runs_likelihood(df, player_name, runs)
bowling_wicket_likelihood(df, player_name)
batter_runs_likelihood(df, player_name, runs, batting_number)
batting_wicket_likelihood(df, player_name, batting_number)
```

#### Simulation of a Ball
The file **bbl_ball_simulation_log5.R** simulates a ball by using the log5 function for individual matchups and the functions for player statistics.

The following functions are used to simulate a ball
```
log5(z, x, y)
the_ball(over_p_zero, bowler_zero, batter_zero, over_p_one, bowler_one, batter_one, over_p_two, bowler_two, batter_two, 
                     over_p_three, bowler_three, batter_three, over_p_four, bowler_four, batter_four, over_p_five, bowler_five, 
                     batter_five, over_p_six, bowler_six, batter_six, over_p_wicket, bowler_wicket, batter_wicket)
```

#### Simulation of an Innings
The file **bbl_MC_simulation_log5.R** simulates an innings assuming that the possibilities from any ball are a zero, one, two, three, four, five, six or wicket.

The following functions are used to simulate an innings
```
over_permutation(bowling_order, first_four = FALSE)
innings_sim(overs_probs_df, bowling_probs_df, batting_probs_df, 
            bowling_order_list, ff_T_or_F, runs_required = NULL)
```

#### Simulation of Matches
The file **bbl_MC_simulation_log5.R** runs a match for a certain number of simulations and collects player and team statistics for the simulations.

The following function is used run match simulations
```
bbl_innings_MC(df, team_one, team_two, t1_bowling_order_list, t2_bowling_order_list, MC_length, 
                           t1_over_under = c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL),      
                           t2_over_under=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL), stadium = FALSE)
```

#### Creation of Odds Markets
The file **bbl_decimal_odds.R** creates a variety of dataframes containing odds for team and player sports betting markets.

The following function is used to get the decimal odds for player sports betting markets
```
get_decimal_odds(df, team_one_name, team_two_name, team_one_players, team_two_players, team_one_bowling_order, team_two_bowling_order, MC_simulations,
                             t1_over_under = c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL), 
                             t2_over_under=c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL), stadium = FALSE)
```
#### 
Creation of Excel File
The file **bbl_create_excel.R** contains a function for the template for the final excel file. The file **bbl_realmatchsimulation.R** uses a function to add the team lineup's, batting order's and bowling order's.

The following function is used to create a formatted excel file
```
create_excel_file(team_one_name, team_two_name, team_one_b1, team_two_b2, team_one_header_style, 
                              team_one_cell_style, team_two_header_style, team_two_cell_style)
```
## Example of Excel File
In **bbl_realmatchsimulation.R** run:
```
bbl_match_odds("Heat", "Strikers", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
               c(10, 7, 10, 8, 11, 11, 11, 11, 10, 10, 9, 9, 9, 8, 8, 8, 9, 7, 7, 7),
               c(2, 10, 9, 11, 11, 11, 11, 2, 2, 2, 9, 9, 9, 10, 10, 10, 7, 7, 7, 7),
               4000, stadium = "Brisbane Cricket Ground", batting_first = 'T2')
```

![alt text](https://github.com/harrylawson37/BBL_model/blob/main/example_for_readme1.png)
![alt text](https://github.com/harrylawson37/BBL_model/blob/main/example_for_readme2.png)
![alt text](https://github.com/harrylawson37/BBL_model/blob/main/example_for_readme4.png)
![alt text](https://github.com/harrylawson37/BBL_model/blob/main/example_for_readme5.png)
![alt text](https://github.com/harrylawson37/BBL_model/blob/main/example_for_readme3.png)


