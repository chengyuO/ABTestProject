# Mobile Game AB Test Analysis

## Introduction:
A fictional math puzzle game has been introduced a new experience. This is a mobile game where players solve levels of math questions to progress through a map. The game is free to play, but players can purchase hints for a level. To test players’ responses to a new experience, an experiment ran from 2017-05-04 to 2017-05-22.
The objective of this project is to analyze the results of an AB test and determine whether the new feature improved the game.

## Methods
The data were originally stored on Google BigQuery and queried using R dplyr package. Metrics tested are conversion rates, Day1 retention rate, average gamerounds per player and average purchases per player. Testing methods used are Welch's T-test and Fisher's exact test.

