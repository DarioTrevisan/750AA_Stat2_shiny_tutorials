werms_logo_description <- "Logo opening splash screen for a game called WERMS insipired by worms game with several blue worms and one red worm faces, graphics early 2000's games fun, Bing Image Creator"

werms_description <- "# Win by Empirical Risk Minimization (WERMS)
Welcome to the Empirical Risk Minimization (ERM) challenge! This interactive game is designed to help students grasp the concept of empirical risk minimization through hands-on experience. In this game, players will randomly sample one-dimensional points and engage in the process of minimizing the residuals by hand. They will explore various loss functions and determine optimal values such as the mean, median, quartiles, and more.

## Game Mechanics

1. **Choose a Loss Function**: Players can select from various loss functions, such as:
   - Squared Loss
   - Absolute Loss
   - Huber Loss
   - Quantile Loss

2. **Random Sampling**: The game will provide a set of random 1D data points, allowing players to visualize and analyze the distribution of values.

3. **Minimization Process**: Players will manually find the optimal point that minimizes the chosen loss function. They will:
   - Plot the points on a graph.
   - Identify the point that results in the smallest total loss.
   - **Beware!** In case of multiple minimizers, choose the smallest (i.e. leftmost) one.

4. **Winning Condition**: If the player's proposed point is sufficiently close to the actual minimizer, they win!

5. **Exploration of Concepts**: As players progress, they will learn about key statistical concepts, including:
   - The differences between mean and median.
   - The significance of quartiles in data distribution.
   - The impact of different loss functions on the minimization process."


# define huber loss function

hub <- function(x, delta=1){
  if (abs(x)<delta){
    x^2/2
  }
  else{
    delta*(abs(x) -delta/2)
  }
}

huber <- Vectorize(hub, vectorize.args = c("x"))


# define loss function


loss <- function(value, points, choice){
  res = points - value
  eps=0.001
  if (choice == "OLS"){
    mean((res)^2)
  }
  else if (choice == "ABS"){
    mean(abs(res))+ eps*value
  }
  else if (choice == "HUB"){
    mean(huber(res))+eps*value
  }
  else if (choice == "1QUART"){
    mean(  3*pmax(0, -res) ) + mean( pmax(0, res) )+eps*value 
  }
  else if (choice == "10PERC"){
    mean(  9*pmax(0, -res) + pmax(0, res) )+eps*value 
  }
  else if (choice == "EXP"){
    mean( exp(abs(res)))
  }
  else {
    0
  }
}

# Function that outputs the formula for the loss


loss_formula <- function(choice){
  if (choice == "OLS") {
    "z^2."
  }
  else if (choice == "ABS") {
    "|z|."
  }
  else if (choice == "HUB"){
    "z^2/2 if |z|<1, otherwise (|z|-1/2)"
  }
  else if (choice == "1QUART") {
    "3 z^- + z^+"
  }
  else if (choice == "10PERC") {
    "9 z^- + z^+"
  }
  else if (choice == "EXP") {
    "exp(|z|)"
  }
}

# vectorize for application to ggplot_fun

vLoss <- Vectorize(loss, c("value"))

# nlm to compute minimizer

empirical_minimizer <- function(points, choice){
  optimize(loss, c(-10, 10), points = points, choice=choice)$minimum
}



