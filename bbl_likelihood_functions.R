### Harry Lawson ###

### Create functions for general and individual player statistics

##################### GENERAL STATISTICS FUNCTIONS ############################

wicket_likelihood <- function(df){
  
  # This function gets the likelihood of a wicket for any ball
  
  # return:       the likelihood for a wicket
  # parameter df: ball by ball database for history of BBL

    total_deliveries = length(df$X)
  dismissal = df[df$dismissal != 'run out' & df$dismissal != 'none',]
  total_dismissal = length(dismissal$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_dismissal/total_deliveries)
  }
}

zero_likelihood <- function(df){
  
  # This function gets the likelihood of a zero for any ball
  
  # return:       the likelihood for a zero
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 0,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

one_likelihood <- function(df){
  
  # This function gets the likelihood of a one for any ball
  
  # return:       the likelihood for a one
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 1,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

two_likelihood <- function(df){
  
  # This function gets the likelihood of a two for any ball
  
  # return:       the likelihood for a two
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 2,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

three_likelihood <- function(df){
  
  # This function gets the likelihood of a three for any ball
  
  # return:       the likelihood for a three
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 3,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

four_likelihood <- function(df){
  
  # This function gets the likelihood of a four for any ball
  
  # return:       the likelihood for a four
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 4,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

five_likelihood <- function(df){
  
  # This function gets the likelihood of a five for any ball
  
  # return:       the likelihood for a five
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 5,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

six_likelihood <- function(df){
  
  # This function gets the likelihood of a six for any ball
  
  # return:       the likelihood for a six
  # parameter df: ball by ball database for history of BBL
  
  total_deliveries = length(df$X)
  run = df[df$runs == 6,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}


wickets_at_over_likelihood <- function(df, over, stadium = FALSE){
  
  # This function takes the bbl ball by ball database, the over and the stadium
  # to get the likelihood of a wicket for a particular over at a particular stadium
  
  # return:            the likelihood for a wicket at that over at that stadium
  # parameter df:      ball by ball database for history of BBL
  # parameter over:    the over (integer)
  # parameter stadium: the stadium (character)
  
  if (stadium != FALSE){
    df = df[df$stadium == stadium,]
  }
  over_data = df[as.numeric(df$over_delivery)%/%1 == over,]
  total_deliveries = length(over_data$X)
  dismissal = over_data[over_data$dismissal != 'run out' & over_data$dismissal != 'none',]
  total_dismissal = length(dismissal$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_dismissal/total_deliveries)
  }  
}

runs_at_over_likelihood <- function(df, runs, over, stadium = FALSE){
  
  # This function takes the bbl ball by ball database, the runs, the over and the stadium
  # to get the likelihood of a given run for a particular over at a particular stadium
  
  # return:            the likelihood for a given run at that over at that stadium
  # parameter df:      ball by ball database for history of BBL
  # parameter runs:    the runs (integer)
  # parameter over:    the over (integer)
  # parameter stadium: the stadium (character)
  
  if (stadium != FALSE){
    df = df[df$stadium == stadium,]
  }
  over_data = df[as.numeric(df$over_delivery)%/%1 == over,]
  total_deliveries = length(over_data$X)
  run = over_data[over_data$runs == runs,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    return(0)
  } else {
    return(total_run/total_deliveries)
  }
}

shrinkage_estimator <- function(overall_mean, player_mean, required_trials, player_trials){
  
  # This function takes the general likelihood of an outcome for the BBL, the 
  # the players likelihood for a general stat, the required trials for no shrinkage
  # required and the players total trials. It then shrinks the players likelihood towards 
  # the average likelihood by an amount that depends on the difference between the players 
  # likelihood and the average likelihood and the squared difference between required
  # trials and player total trials trials. 
  
  # return:                    the newly shrunk likelihood
  # parameter overall_mean:    overall likelihood (float)
  # parameter player_mean:     player likelihood (float)
  # parameter required_trials: required trials (integer)
  # parameter player_trials:   player trials (integer)
  
  difference = overall_mean-player_mean
  move_by = difference/required_trials^2
  final = overall_mean-player_trials^2*move_by
  return(final)
}

################# INDIVIDUAL BOWLING STATISTICS FUNCTIONS ######################

bowler_runs_likelihood <- function(df, player_name, runs){
  
  # This function determines an individual bowlers run likelihood. It uses the 
  # bowlers 250 most recent deliveries. If the bowler has less than 100 deliveries
  # a shrinkage estimator is called to determine the likelihood.
  
  # return:                the bowlers likelihood of given run
  # parameter df:          the ball by ball information (data frame)
  # parameter player_name: the players name (character)
  # parameter runs:        the amount of runs (integer)
  
  p_name = na.omit(df[df$bowler_name == player_name,])
  p_name = p_name[order(p_name$date, decreasing = TRUE),]
  p_name = na.omit(p_name[1:250,])
  total_deliveries = length(p_name$X)
  run = p_name[p_name$runs == runs,]
  total_run = length(run$X)
  if (total_deliveries == 0){
    player_names = read.csv("bbl_player_ID_MAIN.csv")
    index = match(player_name, player_names$player_name)
    bowling_type = player_names$bowling_type[index]
    bowling_type_data = na.omit(df[df$bowling_type == bowling_type,])
    if (runs == 0){
      overall_runs_likelihood = zero_likelihood(bowling_type_data)
    } else if (runs == 1){
      overall_runs_likelihood = one_likelihood(bowling_type_data)
    } else if (runs == 2){
      overall_runs_likelihood = two_likelihood(bowling_type_data)
    } else if (runs == 3){
      overall_runs_likelihood = three_likelihood(bowling_type_data)
    } else if (runs == 4){
      overall_runs_likelihood = four_likelihood(bowling_type_data)
    } else if (runs == 5){
      overall_runs_likelihood = five_likelihood(bowling_type_data)
    } else if (runs == 6){
      overall_runs_likelihood = six_likelihood(bowling_type_data)
    }
    return(overall_runs_likelihood)
  } else if (total_deliveries < 100){
    player_names = read.csv("bbl_player_ID_MAIN.csv")
    index = match(player_name, player_names$player_name)
    bowling_type = player_names$bowling_type[index]
    player_runs_likelihood = total_run/total_deliveries
    bowling_type_data = na.omit(df[df$bowling_type == bowling_type,])
    if (runs == 0){
      overall_runs_likelihood = zero_likelihood(bowling_type_data)
    } else if (runs == 1){
      overall_runs_likelihood = one_likelihood(bowling_type_data)
    } else if (runs == 2){
      overall_runs_likelihood = two_likelihood(bowling_type_data)
    } else if (runs == 3){
      overall_runs_likelihood = three_likelihood(bowling_type_data)
    } else if (runs == 4){
      overall_runs_likelihood = four_likelihood(bowling_type_data)
    } else if (runs == 5){
      overall_runs_likelihood = five_likelihood(bowling_type_data)
    } else if (runs == 6){
      overall_runs_likelihood = six_likelihood(bowling_type_data)
    }
    big_list = list()
    big_list$overall = overall_runs_likelihood
    big_list$original = player_runs_likelihood
    big_list$new = shrinkage_estimator(overall_runs_likelihood, player_runs_likelihood, 100, total_deliveries)
    return(big_list$new)
  } else {
    return(total_run/total_deliveries)
  }
}

bowling_wicket_likelihood <- function(df, player_name){
  
  # This function determines an individual bowlers wicket likelihood. It uses the 
  # bowlers 250 most recent deliveries. If the bowler has less than 100 deliveries
  # a shrinkage estimator is called to determine the likelihood.
  
  # return:                the bowlers likelihood of a wicket
  # parameter df:          the ball by ball information (data frame)
  # parameter player_name: the players name (character)

  p_name = na.omit(df[df$bowler_name == player_name,])
  p_name = p_name[order(p_name$date, decreasing = TRUE),]
  p_name = na.omit(p_name[1:250,])
  total_deliveries = length(p_name$X)
  dismissal = p_name[p_name$dismissal != 'run out' & p_name$dismissal != 'none',]
  total_dismissal = length(dismissal$X)
  if (total_deliveries == 0){
    player_names = read.csv("bbl_player_ID_MAIN.csv")
    index = match(player_name, player_names$player_name)
    bowling_type = player_names$bowling_type[index]
    bowling_type_data = na.omit(df[df$bowling_type == bowling_type,])
    overall_wicket_likelihood = wicket_likelihood(bowling_type_data)
    return(overall_wicket_likelihood)
  } else if (total_deliveries < 100){
    player_names = read.csv("bbl_player_ID_MAIN.csv")
    index = match(player_name, player_names$player_name)
    bowling_type = player_names$bowling_type[index]
    player_wicket_likelihood = total_dismissal/total_deliveries
    bowling_type_data = na.omit(df[df$bowling_type == bowling_type,])
    overall_wicket_likelihood = wicket_likelihood(bowling_type_data)
    big_list = list()
    big_list$overall = overall_wicket_likelihood
    big_list$original = player_wicket_likelihood
    big_list$new = shrinkage_estimator(overall_wicket_likelihood, player_wicket_likelihood, 100, total_deliveries)
    return(big_list$new)
  } else {
    return(total_dismissal/total_deliveries)
  }
}

################# INDIVIDUAL BATTING STATISTICS FUNCTIONS ######################

batter_runs_likelihood <- function(df, player_name, runs, batting_number){
  
  # This function determines an individual batsman's run likelihood. It uses the 
  # batsman's 250 most recent deliveries. If the batsman has less than 100 deliveries
  # a shrinkage estimator is called to determine the likelihood.
  
  # return:                   the batsman's likelihood of given run
  # parameter df:             the ball by ball information (data frame)
  # parameter player_name:    the players name (character)
  # parameter runs:           the amount of runs (integer)
  # parameter batting_number: the batting number (integer)
  
  p_name = df[df$batsman_name == player_name,]
  p_name = p_name[order(p_name$date, decreasing = TRUE),]
  p_name = na.omit(p_name[1:250,])
  run = p_name[p_name$runs == runs,]
  total_run = length(run$X)
  total_deliveries = length(p_name$X)
  if (total_deliveries == 0){
    batting_number_data = na.omit(df[df$batting_number == batting_number,])
    if (runs == 0){
      overall_runs_likelihood = zero_likelihood(batting_number_data)
    } else if (runs == 1){
      overall_runs_likelihood = one_likelihood(batting_number_data)
    } else if (runs == 2){
      overall_runs_likelihood = two_likelihood(batting_number_data)
    } else if (runs == 3){
      overall_runs_likelihood = three_likelihood(batting_number_data)
    } else if (runs == 4){
      overall_runs_likelihood = four_likelihood(batting_number_data)
    } else if (runs == 5){
      overall_runs_likelihood = five_likelihood(batting_number_data)
    } else if (runs == 6){
      overall_runs_likelihood = six_likelihood(batting_number_data)
    }
    return(overall_runs_likelihood)
  } else if (total_deliveries <100) {
    player_runs_likelihood = total_run/total_deliveries
    batting_number_data = na.omit(df[df$batting_number == batting_number,])
    if (runs == 0){
      overall_runs_likelihood = zero_likelihood(batting_number_data)
    } else if (runs == 1){
      overall_runs_likelihood = one_likelihood(batting_number_data)
    } else if (runs == 2){
      overall_runs_likelihood = two_likelihood(batting_number_data)
    } else if (runs == 3){
      overall_runs_likelihood = three_likelihood(batting_number_data)
    } else if (runs == 4){
      overall_runs_likelihood = four_likelihood(batting_number_data)
    } else if (runs == 5){
      overall_runs_likelihood = five_likelihood(batting_number_data)
    } else if (runs == 6){
      overall_runs_likelihood = six_likelihood(batting_number_data)
    }
    big_list = list()
    big_list$overall = overall_runs_likelihood
    big_list$original = player_runs_likelihood
    big_list$new = shrinkage_estimator(overall_runs_likelihood, player_runs_likelihood, 100, total_deliveries)
    return(big_list$new)
  } else {
    return(total_run/total_deliveries)
  }
}

batting_wicket_likelihood <- function(df, player_name, batting_number){
  
  # This function determines an individual batsman's wicket likelihood. It uses the 
  # batsman's 250 most recent deliveries. If the batsman has less than 100 deliveries
  # a shrinkage estimator is called to determine the likelihood.
  
  # return:                   the batsman's likelihood of wicket
  # parameter df:             the ball by ball information (data frame)
  # parameter player_name:    the players name (character)
  # parameter batting_number: the batting number (integer)
  
  p_name = na.omit(df[df$batsman_name == player_name,])
  p_name = p_name[order(p_name$date, decreasing = TRUE),]
  p_name = na.omit(p_name[1:250,])
  total_deliveries = length(p_name$X)
  dismissal = p_name[p_name$dismissal != 'run out' & p_name$dismissal != 'none',]
  total_dismissal = length(dismissal$X)
  if (total_deliveries == 0){
    batting_number_data = na.omit(df[df$batting_number == batting_number,])
    overall_wicket_likelihood = wicket_likelihood(batting_number_data)
    return(overall_wicket_likelihood)
  } else if (total_deliveries < 100){
    player_wicket_likelihood = total_dismissal/total_deliveries
    batting_number_data = na.omit(df[df$batting_number == batting_number,])
    overall_wicket_likelihood = wicket_likelihood(batting_number_data)
    big_list = list()
    big_list$overall = overall_wicket_likelihood
    big_list$original = player_wicket_likelihood
    big_list$new = shrinkage_estimator(overall_wicket_likelihood, player_wicket_likelihood, 100, total_deliveries)
    return(big_list$new)
  } else {
    return(total_dismissal/total_deliveries)
  }
}
