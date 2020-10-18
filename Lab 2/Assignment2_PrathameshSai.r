#Variables of either staying or switching are initialized to be 0. 
n_stay <- 0   
n_switch <- 0
n_stayOrSwitch <-0

for ( i in 1:100) {  # A for-loop is used with variable i to run 100 games.
  door <- c(1,2,3) # We make a vector called door that contains the numbers 1,2 and 3 to represent 3 doors. 
  cardoor <- sample(door,1) # Randomly select one of the 3 doors to have a prize (In this case, a car.) behind it.
  #Now the variable cardoor contains a random value from those in the vector door (we have 3 doors)
  #In other words, a door for the car has been chosen.
  choice <- sample(door,1) # Randomly select the contestants choice of the door.
  #From this, we have a vector of doors, a variable showing which door has the car which was chosen randomly, and a variable indicating which door has chosen by the contestant out of random aswell.
  goatdoors <- setdiff(door, cardoor) # Make a new vector that holds the values corresponding to goats. (a goat is behind the other two doors)
  reveal_options <- setdiff(goatdoors, choice) # We will look at the options we have for the revealing of the doors.
  # We need to focus on if-else blocks to continue our code.
  
  # Firstly, we have the possibility in which there are 2 goats to choose from, and we choose one randomly and assign it to the variable reveal.
  if (choice == cardoor) { 
      reveal <- sample(reveal_options,1)  }  
  # Otherwise, there will be only a single element in reveal_options which we assign to reveal.
  else {
    reveal <- reveal_options 
  }
  remaining_doors <-setdiff(door, reveal) # This creates a new vector which identifies the 2 unrevealed doors that are left.
  newchoice <- setdiff(remaining_doors, choice) # This creates a new variable recording the final choice of door if the contestant switches.
  # The variable choice records their original door.
  if (choice == cardoor) {
    n_stay <- n_stay + 1 # If the contestant choses to stay, we increment the variable to stay.
  }
  
  if (newchoice == cardoor) {
    n_switch <- n_switch + 1 # If the contestant choses to switch, we increment the variable to switch.
  }
  
  if ( sample(remaining_doors,1) == cardoor) {
    n_stayOrSwitch <- n_stayOrSwitch +1; # If the contestant choses to switch or stay, we increment the variable to switch or stay.
  }
   
}
#To represent both variables in terms of probability, the maxiumum value should be 1 and the miniumum should be 0. Hence, we divide by 100 since we had 100 games.
#Let's print out our variables to see our results.
print(n_stay/100) 
print(n_switch/100)
print (n_stayOrSwitch/100)


