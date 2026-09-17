game_logo_description <- ""
game_description <- "
# Welcome to Angry Bayes!

Embark on a whimsical adventure in **Angry Bayes**, a playful twist on the beloved *Angry Birds*. This engaging mini-game introduces you to the fundamental concepts of Bayesian probability and statistics through interactive gameplay.

## Game Overview

In **Angry Bayes**, you’ll learn to navigate the world of Bayesian inference by:

- **Drawing Prior Distributions**: Each level presents a unique, poetic description that hints at the underlying probability distribution. You’ll get to create your own prior curve based on these hints, setting the stage for your Bayesian adventure.

- **Tossing Unfair Coins**: After establishing your prior, you'll engage in a series of coin tosses, where each toss provides new evidence to refine your beliefs. The unfair coins reflect the real-world uncertainty, allowing you to experience how data can influence your understanding of probability.

- **Updating Beliefs Using Bayes' Theorem**: As you gather evidence, you’ll apply Bayes' formula to update your beliefs. This dynamic process illustrates how prior knowledge and new information combine to shape your understanding of the world.

- **Submitting Your Estimate**: When you feel confident in your posterior distribution, you can submit your estimate based on the Maximum a Posteriori (MAP) criterion, which identifies the mode of the posterior distribution. Your accuracy will be rewarded with coins, giving you a tangible incentive to refine your estimates.

## Demo Mode

In addition to the main game, **Angry Bayes** features a **Demo Mode** that allows players to experiment freely. Here, you can:

- **Tweak the Probability Parameter**: Adjust the underlying probability (p) and observe how changes affect your beliefs.
- **Run Multiple Coin Tosses**: Simulate numerous coin tosses to visualize how the prior and likelihood combine to form the posterior distribution, illustrating Bayesian inference in action.
- **Explore Concepts at Your Own Pace**: This mode is designed for exploration and understanding, enabling you to grasp the principles of Bayesian statistics without the pressure of competition.

## Learning Objectives

By playing **Angry Bayes**, you will:

- Understand the basic components of Bayesian inference: prior, likelihood, and posterior.
- Learn how to update beliefs in light of new evidence using Bayes' theorem.
- Gain hands-on experience with probability distributions and their applications in real-world scenarios.

## Conclusion

**Angry Bayes** is not just a game; it's a fun and interactive way to dive into the fascinating world of Bayesian probability. Whether you're a novice or have some experience in statistics, you'll find valuable insights and engaging gameplay that enhance your understanding of these crucial concepts.

Get ready to unleash your inner statistician and enjoy the adventure that awaits in **Angry Bayes**!
"

# Grid used throughout the Demo and Play tabs for the (discretized) probability
# density functions (prior / likelihood / posterior). Defined once here so both
# server_demo.R and server_game.R (sourced into the same environment) share the
# exact same values instead of redefining (and potentially clashing on) them.
step <- 0.01
grid <- seq(0, 1, by = step)

# Poetic descriptions of each possible coin bias (p), read from the data file
# shipped with the app. Column names must match "value" and "description".
values_p <- read.csv("values.csv", stringsAsFactors = FALSE)
