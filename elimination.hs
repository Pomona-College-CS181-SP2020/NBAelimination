import MaxFlow
import GamesLib



  
g_teams = loadTeams "teams.csv"     
g_games_all = loadGames "nba.csv"  g_teams   
g_games = cutofround g_games_all  23  
g_games_toplay = gamesToPlay g_games          
g_east_standing = standing g_teams g_games EAST
g_west_standing = standing g_teams g_games WEST
g_gamestoplay = gamesToPlay g_games 
g_elimination=testTeamEliminationBruteForce g_teams g_games "Toronto Raptors"
-- relevantgames = getRelevantGames g_teams g_games "Toronto Raptors" 

g_teams_test_1 = loadTeams "teams_test_1.csv"     
g_games_all_test_1 = loadGames "games_test_1.csv"  g_teams_test_1   
g_games_test_1 = cutofround g_games_all_test_1  6
g_east_standing_test_1 = standing g_teams_test_1 g_games_test_1 EAST
g_elimination_test_1=testTeamEliminationBruteForce g_teams_test_1 g_games_test_1   "team_2" 
---relevantgames = getRelevantGames g_teams_test_1 g_games_test_1  "team_2" 
---stand = standing g_teams_test_1 relevantgames EAST 
g_elist_test_1  = eliminationBruteForce g_teams_test_1 g_games_test_1 

