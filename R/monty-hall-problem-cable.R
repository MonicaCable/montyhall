#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param
#'  No arguments are used by the function.
#' 
#' @return
#'  The function returns a length 3 character vector
#'  indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Select a door.
#'  
#' @description
#'  `select_door()` sets three doors labeled "1", "2", and "3",
#'  then takes a random sample of one of those doors,
#'  and returns the value of the door that was selected.
#'   
#' @details
#'  At this point in the game the contestant is choosing their
#'  first door in the game. They have no information about any of 
#'  the doors, so their choice is functionally random.
#' 
#' @param
#'  No arguments are used by the function.
#' 
#' @return 
#'  The function returns a length 1 numeric vector
#'  indicating the selected door.
#' 
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  The host opens a goat door.
#'  
#' @description
#'  `open_goat_door()` sets three doors labeled "1", "2", and "3". Then,
#'  If within this game the contestant originally chose a car door,
#'  it sets the non-car doors as goat doors and
#'  takes a random sample of one of the goat doors.
#'  If the contestant originally chose a goat door,
#'  it opens the door that is not the car door and also not the chosen door.
#'  Whichever situation occurs, it then returns the value of the goat door
#'  that the host opened.
#'  
#' @details
#'  Now that the contestant has chosen their door, the host reveals a goat
#'  door. This reduces the contestant's final options down to two doors:
#'  a car door and a goat door. If the contestant chose a goat door originally
#'  then the host has to open the other goat door. If the contestant chose the
#'  car door originally then the host randomly opens one of the goat doors.
#' 
#' @param 
#'  This function uses two arguments: the game that has been created 
#'  (represented by the specific arrangements of goats and cars behind
#'  the three doors), and a.pick (the door that the contestant originally
#'  chose).
#'  
#' @return 
#'  The function returns a length 1 numeric vector indicating the door
#'  that the host opened.
#'  
#' @examples
#'   open_goat_door()
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  The contestant chooses to either keep their original door or switch to
#'  the other unopened door.
#'  
#' @description
#' `change_door()` sets three doors labeled "1", "2", and "3", then
#' determines what the final choice of the contestant will be.
#' If the contestant stays with the door they originally chose, then 
#' their final pick will be the same door that they chose in the beginning.
#' If the contestant switches doors, then their final pick will be
#' the door that isn't the door the host opened and also isn't the door
#' they picked originally. It then returns the value of the final door
#' selected.
#' 
#' @details
#'  Now that the host has revealed a goat door, the contestant has two options.
#'  They can stay with their original door choice and hope that they 
#'  picked the car door the first time, or they can switch to the unopened
#'  door and hope that that door hides the car.
#' 
#' @param 
#'  This function uses three arguments: Whether the contestant stays or not
#'  (which defaults to True), the goat door that the host opened, and the
#'  original door that the contestant opened.
#' 
#' @return 
#'  The function returns a length 1 numeric vector indicating the final door
#'  that the contestant has committed to.
#'  
#' @examples
#'   change_door()
#'   change_door( stay=F, opened.door, a.pick )
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'  Determine if the contestant has won the game.
#'  
#' @description
#'  `determine_winner()` looks at the final door that the contestant chose
#'  within the game. If that door is a car door, then it returns the word
#'  "WIN" and the contestant has won the car. If that door is a goat dore,
#'  then it returns the word "LOSE" and the contestant has not won the car.
#'
#' @details
#'  Now we reveal if the contestant has chosen a car door and won the car
#'  or if they've chosen a goat door instead and lost the game.
#' 
#' @param 
#'  This function uses two arguments: The final door that the contestant picked,
#'  and the current game (as represented by the specific arrangements of cars
#'  and goats behind the three doors).
#'  
#' @return 
#'  The function returns a string of characters. Those characters are either
#'  "WIN" or "LOSE".
#'  
#' @examples
#'   determine_winner()
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Play the game in a single function.
#'  
#' @description
#'  `play_game()` sets a specific arrangement of cars and goats as this
#'  particular new game, then the contestant selects their first door,
#'  and then the host opens a goat door based on the arrangement of
#'  this new game and the door that the contestant originally opened.
#'  It then sets what the final door will be if the contestant stays
#'  and what the final door will be if the contestant switches.
#'  Then it determines the winner within this game when the contestant stays
#'  and when the contestant switches. Finally, it prepares to make a data frame
#'  showing the game results of staying vs switching. It does this by
#'  making the strategy our column headers, the outcome of staying and
#'  switching our next row, and then making a data frame of those objects.
#'  
#' @details
#'  This function plays the entire game. The doors are set, the contestant
#'  chooses their original door, the host opens a goat door, the contestant
#'  decides if they want to stay with their original choice or switch to the
#'  other unopened door, and then the winner is determined.
#'  
#' @param 
#'  No arguments are used by the function.
#'  
#' @return 
#'  This function returns a data frame of 2x2.
#' 
#' @examples
#'   play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Play the game multiple times.
#'  
#' @description
#'  `play_n_games()` loads the "dplyr" library, then sets the list function
#'  as the results list and runs the game in a single loop. Then for each
#'  instance between 1 and however many games you're playing (defaults to 100),
#'  it plays the game and determines the game outcome, saves that game result
#'  as the game result for its iteration within the loop,
#'  and then sets the new loop count as the next iteration of the game until
#'  it runs out of games to play. It then makes a data frame of the list
#'  of all the results from all the games that were played and outputs 
#'  those results in a table that shows what proportion of the time
#'  each game strategy resulted in winning or losing. It also returns
#'  the count of how many winning and losing games there were in this set.
#' 
#' @details
#'  This function plays the entire game several times, records how many times
#'  the strategy of staying or switching led to a winning game or a losing
#'  game, and then exports both the count of those game stats and the
#'  proportions of winning and losing games per strategy.
#'  
#' @param 
#'  This function has one argument: n. N refers to the number of games that
#'  the function will play and give statistics on. Defaults to 100 games.
#'  
#' @return 
#'  This function prints a table of the win/lose proportions of the 
#'  results data frame, and also returns the results data frame.
#'  
#' @examples
#'   play_n_games()
#'   play_n_games(n=1000)
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
