### Harry Lawson ###

source('bbl_likelihood_functions.R')

log5 <- function(z, x, y, over_stadium_likelihood){
  
  # This function uses log5 for individual match ups to determine the likelihood
  # of an event between a batsman and bowler. It then adjusts the likelihood for 
  # the state of the game (the over and stadium)
  
  # return:                            the final likelihood of the event
  # parameter z:                       the general likelihood (float)
  # parameter x:                       the likelihood of the bowler (float)
  # parameter y:                       the likelihood of the batsman (float)
  # parameter over_stadium_likelihood: the general likelihood for the over at the stadium (float)
  
  if (over_stadium_likelihood == 0){
    return(0)
  } else {
    log5_p = ((x*y)/z)/(((x*y)/z) + ((1-x)*(1-y))/(1-z))
    percentage_change = (log5_p - z)/z
    update = percentage_change*over_stadium_likelihood + over_stadium_likelihood
    return(update)
  }
}
  
the_ball <- function(over_p_zero, bowler_zero, batter_zero, over_p_one, bowler_one, batter_one, over_p_two, bowler_two, batter_two, 
                     over_p_three, bowler_three, batter_three, over_p_four, bowler_four, batter_four, over_p_five, bowler_five, 
                     batter_five, over_p_six, bowler_six, batter_six, over_p_wicket, bowler_wicket, batter_wicket,
                     zero, one, two, three, four, five, six, wicket){
  
  # this function determines all the possibilities of the outcome of a ball 
  # between bowler and batsman. It then sums these possibilities and gets a 
  # random number between 0 and 1 to determine the outcome.
  
  # return:             A number representing the outcome of the ball
  # parameter over_p_*: The likelihood of occurrence at the over and stadium (float)
  # parameter bowler_*: The likelihood of occurrence for the bowler (float)
  # parameter batter_*: The likelihood of occurrence for th batsman (float)
  # parameter *:        The general likelihood of occurrence (float)
  # time complexity:    O(1)     

  # note that * represents any number
  
  adjusted_zero_probability = log5(zero, bowler_zero, batter_zero, over_p_zero)
  adjusted_one_probability = log5(one, bowler_one, batter_one, over_p_one)
  adjusted_two_probability = log5(two, bowler_two, batter_two, over_p_two)
  adjusted_three_probability = log5(three, bowler_three, batter_three, over_p_three)
  adjusted_four_probability = log5(four, bowler_four, batter_four, over_p_four)
  adjusted_five_probability = log5(five, bowler_five, batter_five, over_p_five)
  adjusted_six_probability = log5(six, bowler_six, batter_six, over_p_six)
  adjusted_wicket_probability = log5(wicket, bowler_wicket, batter_wicket, over_p_wicket)

  sum_of_probs = adjusted_zero_probability + adjusted_one_probability + adjusted_two_probability + adjusted_three_probability + adjusted_four_probability + adjusted_five_probability + adjusted_six_probability + adjusted_wicket_probability
  
  new_prob_zero = adjusted_zero_probability/sum_of_probs
  new_prob_one = adjusted_one_probability/sum_of_probs
  new_prob_two = adjusted_two_probability/sum_of_probs
  new_prob_three = adjusted_three_probability/sum_of_probs
  new_prob_four = adjusted_four_probability/sum_of_probs
  new_prob_five = adjusted_five_probability/sum_of_probs
  new_prob_six = adjusted_six_probability/sum_of_probs
  new_prob_wicket = adjusted_wicket_probability/sum_of_probs
  
  x = runif(1)
  if (x <= new_prob_zero){
    return(0)
  } else if (x <= new_prob_zero + new_prob_one){
    return(1)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two){
    return(2)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two + new_prob_three){
    return(3)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two + new_prob_three + new_prob_four){
    return(4)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two + new_prob_three + new_prob_four + new_prob_five){
    return(5)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two + new_prob_three + new_prob_four + new_prob_five + new_prob_six){
    return(6)
  } else if (x <= new_prob_zero + new_prob_one + new_prob_two + new_prob_three + new_prob_four + new_prob_five + new_prob_six + new_prob_wicket){
    return(7)
  } 
}
