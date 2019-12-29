# 2048

Final Team Project for CS 3110: Data Structures and Functional Programming 

(For installation instructions please see INSTALL.txt)

An OCaml implementation of the popular 2048 game, to be played on the command line, along with some additional features (not-exhaustive):

NPC-AI Units - implemented a number of AI modules to determine where to place newly generated tiles corresponding to the difficulty level. Also implemented AI-player modules which choose swiping direction in order to locally maximize score and in-game survivability.

Game Modes - implemented a variety of game modes, including two-player mode (one player chooses where to place new tiles), reversed mode (the player chooses where to place tile while the NPC swipes) and timed mode (move must be made within set time). Includes difficulty settings for all modes (except two-player mode).

Account System - allows players to save and load games under their name; includes a leaderboard that contains the highest scores of all the players. Implemented a small functional database to store player data, including records of all the games previously played by the respective players.
