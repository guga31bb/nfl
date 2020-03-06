# Notes

* `0_get_pbp.R` assembles the data used for everything else, covering the 1999-2019 NFL seasons
* `helpers.R` contains functions to assist with cleaning and analyzing the data
  * `scrape` is heavily borrowed from Lee Sharpe and assists with pulling completed games in-season
  * `fix_pbp` cleans up the play-by-play data for use in analysis as covered [in the tutorial](https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701) with some modifications to work with the older data
  * CPOE stuff has been updated
  * `fix_fumbles` still a work in progress
* `code/cpoe.R` looks at CPOE stuff
  
# To-do list
* Make sure fix_fumbles works on 2-pt and end of half
