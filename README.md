# Expected Goals(xG) Model
This repository contains a working model of the xG metric used in the sport of Football.
![xg](https://github.com/ivan-reni-varghese/xG-Model/assets/86720578/62d7d3f4-dc9d-4a02-8dd5-00eb8e91dd1e)


# What is xG?
- metric designed to measure the probability of a shot resulting in a goal
- uses historical information from thousands of shots with similar characteristics
- likelihood of a goal on a scale between 0 and 1.

# How is xG calculated?
Major features/factors fed into the model :
- Distance to goal
- Angle to goal
- Body part with which the shot was taken with
- Type of assist/situation

# Models Used :
- Random Forest
- CatBoost
- XG Boost
- LightGBM
