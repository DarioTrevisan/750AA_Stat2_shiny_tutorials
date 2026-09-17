medoids_logo_description <- "prova"

demo_medoids_description <- "How to use this demo:
- double click on the plot to add points
- single click on the plot to select a point
- use button Show medoid to check your answer,
- to simulate the actual game, you can use the button to remove the medoid and keep playing!"

medoids_description <- "# Medoids Game


Welcome to **Medoids**, an engaging game designed to help players understand the concept of medoids, a central point in a dataset that minimizes the distance to all other points. Drawing inspiration from the classic **Arkanoid** game, players will interact with colorful tiles, honing their skills in identifying the medoid.

## Objective

The objective of the game is to click on the correct medoid from a set of randomly colored tiles. Players must double-click to confirm their choice. Successfully identifying the medoid allows players to progress to the next level, where the challenge increases.

## Game Mechanics

### Setup

1. **Visual Interface**:
   - The game starts with a set of tiles on the screen at random positions.
   - Each tile represents a data point in a hypothetical dataset.

2. **Medoid Identification**:
   - A medoid is the tile that has the minimum total distance to all other tiles in the current level.
   - Players must use their understanding of distance and centrality to identify the correct tile.

### Gameplay

1. **Selecting the Medoid**:
   - Players click on a tile to highlight it.
   - A double-click (or a click on the dedicated button) is required to confirm their selection.

2. **Feedback**:
   - If the player selects the correct medoid, that tile is removed from the grid.
   - If the selection is incorrect, a message is displayed, encouraging the player to improve in the next trials.

3. **Progression**:
   - The game continues until only two tiles remain.
   - At this point, players automatically move to the next level, where a new set of tiles is presented.

### Levels

- Each level presents a different configuration of tiles and varying distances between them.
- As players advance, the number of tiles increases, making it more challenging to identify the medoid.

## Educational Value


- The game provides an interactive way to grasp the concept of medoids in clustering algorithms, particularly in the **Partitioning Around Medoids (PAM)** method.
- Players learn to visualize and compute distances, enhancing their understanding of clustering techniques.

Join us in this fun and educational journey as we explore the world of Medoids!"


# define a function to find the closest point from a data frame

closest_point <- function(value, points) {
  # compute the distances from the value point to all the other points
  distances = (value$x - points$x) ** 2 + (value$y - points$y) ** 2
  # find the minimum distance index
  which.min(distances)
}



compute_medoid <- function(points) {
  # for each point in points compute the distance to all the other points
  distances = NULL
  for (i in 1:nrow(points)) {
    distances <- c(distances, sum(sqrt(
      (points$x[i] - points$x) ** 2 + (points$y[i] - points$y) ** 2
    )))
  }
  which.min(distances)
}
