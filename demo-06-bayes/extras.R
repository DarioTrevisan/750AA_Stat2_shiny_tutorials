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

# Create the data frame
values_p <- data.frame(
  value = c( p= seq(0, 1, by=0.01)), 
  description = c(
    "At the edge of the abyss, where nothing exists.",
    "A faint glimmer of hope, just beginning to emerge.",
    "A small spark, barely noticeable in the vastness.",
    "The first signs of life, struggling to thrive.",
    "An uncertain path, full of potential but fraught with challenges.",
    "A timid flicker, hinting at what could be.",
    "A hint of curiosity, ready to explore.",
    "A vibrant pulse, full of energy and promise.",
    "A flourishing presence, radiating strength and assurance.",
    "On the brink of greatness, almost there but not quite.",
    "A flicker of optimism, hinting at future possibilities.",
    "A cautious advance, testing the waters ahead.",
    "A steady climb, gaining momentum with each step.",
    "A hopeful ascent, fueled by determination.",
    "A balanced approach, weighing every option carefully.",
    "A growing presence, starting to take shape.",
    "A determined effort, carving out a niche.",
    "A promising venture, full of potential.",
    "A strong foundation, ready for expansion.",
    "A curious exploration, seeking new horizons.",
    "A cautious optimism, beginning to manifest.",
    "A vibrant endeavor, brimming with possibilities.",
    "A confident leap, embracing the unknown.",
    "A blossoming ambition, reaching for the stars.",
    "A dynamic force, gaining traction with each step.",
    "A significant milestone, marking the journey.",
    "A robust presence, echoing with potential.",
    "An adventurous spirit, ready to forge ahead.",
    "A bold initiative, paving the way for success.",
    "A rising tide, lifting all boats.",
    "A solid commitment, standing firm in the face of challenges.",
    "A lively journey, filled with unexpected twists.",
    "A clear vision, guiding the path forward.",
    "A growing influence, shaping the landscape.",
    "A determined resolve, pushing through barriers.",
    "A radiant glow, illuminating the way.",
    "An inspiring story, waiting to unfold.",
    "A flourishing opportunity, just around the corner.",
    "A strategic maneuver, positioning for success.",
    "A vibrant community, coming together in unity.",
    "A pivotal moment, ready to define the future.",
    "A powerful surge, breaking through limitations.",
    "A confident stride, moving forward with purpose.",
    "A strong heartbeat, echoing with life.",
    "An expanding horizon, filled with promise.",
    "A determined march, resolute in its path.",
    "A harmonious balance, finding equilibrium.",
    "An exciting adventure, brimming with discovery.",
    "A fruitful endeavor, yielding rewards.",
    "A radiant sunrise, marking a new beginning.",
    "The midpoint, a balance between risk and reward.",
    "A hopeful ascent, fueled by determination.",
    "A steady climb, gaining momentum with each step.",
    "A confident advance, ready to face the challenges.",
    "A vibrant journey, full of potential.",
    "A blossoming future, waiting to unfold.",
    "A promising opportunity, beckoning from afar.",
    "A rising star, shining brightly in the night.",
    "A dynamic presence, energizing the surroundings.",
    "A strong foundation, ready to support growth.",
    "A flourishing moment, capturing the essence of success.",
    "A radiant vision, guiding the path ahead.",
    "A bold leap, embracing new possibilities.",
    "A vibrant pulse, full of life and energy.",
    "A bright path, illuminated by hope.",
    "A significant breakthrough, paving the way forward.",
    "A powerful wave, reshaping the landscape.",
    "A confident approach, embracing the journey.",
    "A flourishing spirit, ready to soar.",
    "An inspiring journey, filled with potential.",
    "A celebratory moment, marking success.",
    "A steady rise, climbing higher with each step.",
    "A vibrant opportunity, waiting to be seized.",
    "A confident stride, moving towards greatness.",
    "A radiant glow, illuminating the path ahead.",
    "A transformative experience, ready to unfold.",
    "A powerful force, driving towards success.",
    "A promising future, filled with possibilities.",
    "A dynamic journey, rich with discovery.",
    "A hopeful ascent, reaching new heights.",
    "A vibrant expression, celebrating achievement.",
    "A flourishing moment, filled with promise.",
    "A confident embrace, welcoming the future.",
    "An exciting adventure, brimming with potential.",
    "A steady heartbeat, echoing with life.",
    "A radiant dawn, signaling new beginnings.",
    "A strong connection, binding the journey.",
    "A vibrant legacy, waiting to be written.",
    "A bold vision, guiding the way.",
    "A powerful moment, on the brink of greatness.",
    "A confident leap, embracing the unknown.",
    "A flourishing presence, radiating strength.",
    "A transformative journey, ready to take flight.",
    "A dynamic force, shaping the future.",
    "A bright star, shining in the night sky.",
    "A vibrant pulse, full of life and energy.",
    "A strong foundation, supporting growth.",
    "A radiant glow, illuminating the path ahead.",
    "A celebratory moment, marking the journey.",
    "On the brink of greatness, almost there but not quite.",
    "Total achievement, the pinnacle of success."
  )
)

