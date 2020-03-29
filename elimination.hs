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
--relevantgames = gamesForMaxFlowElimination g_teams g_games "Toronto Raptors" 

g_teams_test_1 = loadTeams "teams_test_1.csv"     
g_games_all_test_1 = loadGames "games_test_1.csv"  g_teams_test_1   
g_games_test_1 = cutofround g_games_all_test_1  5
g_east_standing_test_1 = standing g_teams_test_1 g_games_test_1 EAST
g_elimination_test_1=testTeamEliminationBruteForce g_teams_test_1 g_games_test_1   "team_2" 
relevantgames = gamesForMaxFlowElimination g_teams_test_1 g_games_test_1  "team_5" 
gamesSummary   =  gamesToPlaySummary  relevantgames
stand = standing g_teams_test_1 relevantgames EAST 
g_elist_test_1  = eliminationBruteForce g_teams_test_1 g_games_test_1 

sourceVertex = "s" 
syncVertex = "t"  

-- Get the list of games that are relevant for the MaxFlow - games that were not played yet and do not include teams that 
-- can't be in the first place  and do not include the team that is tested for elimination.
gamesForMaxFlowElimination::IO Teams -> IO Games -> String ->  IO Games
gamesForMaxFlowElimination teams games team = gamesToPlay $ getRelevantGames teams games team 

--- Build the source vertex of the network flow graph that has edge to each pair of teams with a capacity that is the number of games 
--- to be played btween the teams.
buildSourceVertex:: IO  [(String, String, Int)] -> IO Graph 
buildSourceVertex gSummary = do 
                               gSummary' <-  gSummary
                               return (buildSourceVertex' gSummary')

buildSourceVertex':: [(String, String, Int)] -> Graph 
buildSourceVertex' gSummary = [ (Vertex sourceVertex edges  (maxBound::Int)  "") ] where edges =  buildSourceVertex_helper gSummary                                           
                                                     
buildSourceVertex_helper:: [(String, String, Int)] -> [(String,Int)]        
buildSourceVertex_helper [] = []
buildSourceVertex_helper ((team1,team2,geamsCount):xs ) =   (team1 ++ "-" ++ team2,geamsCount):buildSourceVertex_helper xs   



--- Build the vertices that represent the games between 2 teams. From each of those vertices there is 2 edges 1 for each team with 
--- infinit capacity.
buildGamesVertices:: IO [(String, String, Int)] -> IO  Graph 
buildGamesVertices gSummary = do 
                               gSummary' <-  gSummary
                               return (buildGamesVertices' gSummary')

buildGamesVertices':: [(String, String, Int)] -> Graph 
buildGamesVertices' [] = []
buildGamesVertices' ((team1,team2,geamsCount):xs)  =  (Vertex (team1 ++ "-" ++ team2) [ (team1,maxBound::Int) ,(team2,maxBound::Int) ]  (maxBound::Int)  ""):(buildGamesVertices' xs)          



buildTeamsVertices':: Teams -> Games -> String -> Graph 
buildTeamsVertices'  teams  games  team = let conf = getConf  teams team 
                                              stand = standing' teams games conf
                                              maxPoints = maxPointsforTeam'  teams games team 
                                              ret = foldr (\(TeamScore team' conf' points')  acc  -> (Vertex team' [(syncVertex,maxPoints - points' )] (maxBound::Int)  ""):acc ) []  stand
                                          in ret     


                              