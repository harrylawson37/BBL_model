# BBL Model 
### A model used to create competitive odd's on team and player sports betting markets for bbl matches.

## Personal Info
I'm a Data Scientist with a keen interest for sports and I have created this model to practice my skills programming in R by applying a variety of simple data analysis techniques. Email me at harrylawson.waters@gmail.com for any questions or suggestions.

DISCLAIMER: This model has been created to practice my data science skills and I do not encourage sports betting. The model is something I complete in my spare time and I am still in the process of fine tuning it.

## Process of Creating the Model
The process of creating the model was completed by collecting data, wrangling data, creating relevant functions for player statistics, creating a function for the simulation of a ball, creating a function for the simulation of an innings, creating a monte carlo function to simulate matches, creating a function to determine different sports betting markets from the simulation and creating function to generate an excel file with the results.

#### Data Collection and Wrangling
All the data collected by automation to create this model is from cricsheet.org. The folder bbl_json contains every bbl match and is directly download from https://cricsheet.org/downloads/bbl_json.zip.

The files bbl_bbb_database_create.R, bbl_bbb_database_update.R and bbl_bbb_playername_database_create.R are all used to update the player information database and the ball by ball information. 

Information from espncricinfo is used to manually add in a new player's batting hand, bowling arm, bowling type and country to bbl_player_ID_MAIN.csv. I have chosen to do this manually because it requires little time.

#### Functions for Player Statistics
The file bbl_likelihood_functions.R contains the relevant functions to simulate any ball between a bowler and batsman at a particular over and cricket ground.

The functions use the last x number of balls the bowler or batsman have faced. The value of x can be altered depending on how much recent form is weighted against a larger dataset. They also use a shrinkage estimator for players with an insufficient amount of data by moving a players statistical average towards the statistical average of that players bowling type or batting number. 

#### Simulation of a Ball
The file bbl_ball_simulation_log5.R simulates a ball by using the log5 function for individual matchups and the functions for player statistics.

#### Simulation of an Innings
The file bbl_MC_simulation_log5.R simulates an innings assuming that the possibilities from any ball are a zero, one, two, three, four, five, six or wicket.

#### Simulation of Matches
The file bbl_MC_simulation_log5.R runs a match for a certain number of simulations and collects player and team statistics for the simulations.

#### Creation of Odds Markets
The file bbl_decimal_odds.R creates a variety of dataframes containing odds for team and player sports betting markets.

#### Creation of Excel File
The file bbl_create_excel.R contains a function for the template for the final excel file. The file bbl_realmatchsimulation.R uses a function to add the team lineup's, batting order's and bowling order's.

## Example of Excel File



