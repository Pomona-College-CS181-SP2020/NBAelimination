module Elimination 
(eliminationMaxFlow,eliminationMaxFlow',eliminationMaxFlowFromFile,eliminationBruteForce',eliminationBruteForce,eliminationBruteForceFromFile,loadTeams,loadGames,cutofround,cutofdate) where

import MaxFlow
import GamesLib

  

---------------------------------public interface -------------------------------------------------------------------


--- Takes a list of teams and games (including not yet played games) and generate list of teams that can't be the first place in thier conference under any scenario
eliminationMaxFlow:: IO Teams -> IO Games -> IO [String]
eliminationMaxFlow teams games  = do 
                                         teams' <- teams
                                         games' <- games
                                         return (eliminationMaxFlow' teams' games' )
                                                
eliminationMaxFlow':: Teams -> Games -> [String]
eliminationMaxFlow' teams games = foldr (\(Team name conf ) acc  -> if ( testTeamEliminationMaxFlow' teams games name ) then name:acc else acc)[] teams

--- Takes teams and  games files and generate list of teams that can't be the first place in thier conference under any scenario
eliminationMaxFlowFromFile:: String  -> String -> IO [String]
eliminationMaxFlowFromFile teamsfile gamesfile  = let   teams = loadTeams  teamsfile 
                                                        games = loadGames gamesfile teams
                                                  in  (eliminationMaxFlow teams games)
 
--- Takes teams and  games files and generate list of teams that can't be the first place in thier conference under any scenario (using brute force)
eliminationBruteForceFromFile:: String  -> String -> IO [String]
eliminationBruteForceFromFile teamsfile gamesfile  = let   teams = loadTeams  teamsfile 
                                                           games = loadGames gamesfile teams
                                                     in  (eliminationBruteForce teams games)
 
-----------------------------------implementation functions--------------------------------------------------------------


sourceVertex = "s" 
syncVertex = "t"  

-- Get the list of games that are relevant for the MaxFlow - games that were not played yet and do not include teams that 
-- can't be in the first place  and do not include the team that is tested for elimination.
gamesForMaxFlowElimination::IO Teams -> IO Games -> String ->  IO Games
gamesForMaxFlowElimination teams games team = do 
                                                 games' <- games 
                                                 teams' <- teams 
                                                 return (gamesForMaxFlowElimination' teams'  games' team)

gamesForMaxFlowElimination'::Teams -> Games -> String -> Games
gamesForMaxFlowElimination' teams games team = gamesToPlay' $ getRelevantGames' teams games team


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


buildTeamsVertices:: IO Standings  -> IO Teams ->  IO Games -> String -> Int -> IO Graph 
buildTeamsVertices stand teams  games  team teamMaxPoints = do 
                                                                stand' <-  stand
                                                                teams' <-  teams
                                                                games' <-  games
                                                                return (buildTeamsVertices' stand' teams'  games' team teamMaxPoints)

buildTeamsVertices':: Standings  -> Teams ->  Games -> String -> Int -> Graph 
buildTeamsVertices' stand teams  games  team teamMaxPoints = let conf = getConf  teams team                                                                         
                                                                 relevantTeams = foldr (\(Game round t location hometeam awayteam result ) acc -> if elem hometeam acc  &&  elem awayteam  acc then acc 
                                                                                                                                                else if elem hometeam acc  && not ( elem awayteam  acc) then awayteam:acc 
                                                                                                                                                else if not (elem hometeam acc ) &&   elem awayteam  acc then hometeam:acc 
                                                                                                                                                else awayteam:hometeam:acc 
                                                                                                                                            )[] games
                                                                 ret = foldr ( \(TeamScore teamname conf' points  ) acc -> if  elem teamname  relevantTeams then  
                                                                                                                          (Vertex teamname [ (syncVertex ,if conf == conf' then  teamMaxPoints-points else (maxBound::Int))] (maxBound::Int)  "" ):acc
                                                                                                                           else  acc  
                                                                                                                    )[]   stand                                    
                                                             in ret     


---Build the full maxflow graph to test elimination of a given team 
buildGraphFromGames:: IO Teams -> IO  Games -> String ->  IO Graph 
buildGraphFromGames teams games team = do 
                                          teams' <- teams
                                          games' <- games
                                          return (buildGraphFromGames' teams' games' team)

buildGraphFromGames':: Teams ->  Games -> String ->  Graph 
buildGraphFromGames' teams games team = let maxFlowGames = gamesForMaxFlowElimination' teams games team 
                                            stand = (standing' teams games EAST )++ (standing' teams games WEST)
                                            maxFlowGamesSummary = gamesToPlaySummary' maxFlowGames
                                            sourceVertex = buildSourceVertex' maxFlowGamesSummary
                                            gameVertices = buildGamesVertices' maxFlowGamesSummary
                                            teamVertices = buildTeamsVertices' stand teams maxFlowGames team (maxPointsforTeam' teams games team )
                                        in  sourceVertex ++  gameVertices ++   teamVertices ++ [(Vertex syncVertex [] (maxBound::Int)  "" )]
 


testTeamEliminationMaxFlow:: IO  Teams -> IO Games -> String -> IO Bool  
testTeamEliminationMaxFlow teams games team = do 
                                                 ret <- putStr $ "testing elimination for team " ++ team
                                                 teams' <- teams
                                                 games' <- games
                                                 return (testTeamEliminationMaxFlow' teams' games' team)
 
testTeamEliminationMaxFlow'::  Teams ->  Games -> String -> Bool
testTeamEliminationMaxFlow' teams games team =  let maxFlowGraph = buildGraphFromGames' teams games team
                                                    numberOfGames = length $ gamesForMaxFlowElimination' teams games team 
                                                    simpleElimination = checkForNegativeCapacity maxFlowGraph
                                                    maxFlow =  solveMaxFlow maxFlowGraph sourceVertex syncVertex
                                                in if (length maxFlowGraph <= 2) then False else if simpleElimination then True else (snd maxFlow) < numberOfGames
                                                



testTeamEliminationMaxFlowDebug teams games team = do 
                                                 teams' <- teams
                                                 games' <- games
                                                 return (testTeamEliminationMaxFlowDebug' teams' games' team)
 

testTeamEliminationMaxFlowDebug' teams games team =  let maxFlowGraph = buildGraphFromGames' teams games team
                                                         numberOfGames = length $ gamesForMaxFlowElimination' teams games team 
                                                         simpleElimination = checkForNegativeCapacity maxFlowGraph
                                                         maxFlow =  solveMaxFlow maxFlowGraph sourceVertex syncVertex
                                                         
                                                     in (snd maxFlow,numberOfGames,simpleElimination,(maxPointsforTeam' teams games team ))
 
 
g_teams = loadTeams "teamsnba.csv"     
g_games_all = loadGames "gamesnba.csv"  g_teams   
g_games = cutofround g_games_all  10 
g_games_toplay = gamesToPlay g_games          
g_east_standing = standing g_teams g_games EAST
g_west_standing = standing g_teams g_games WEST
g_gamestoplay = gamesToPlay g_games 
g_elimination=testTeamEliminationBruteForce g_teams g_games "Toronto Raptors"
--relevantgames = gamesForMaxFlowElimination g_teams g_games "Toronto Raptors" 

g_teams_test_1 = loadTeams "teams_test_1.csv"     
g_games_all_test_1 = loadGames "games_test_1.csv"  g_teams_test_1   
g_games_test_1 = g_games_all_test_1
g_east_standing_test_1 = standing g_teams_test_1 g_games_test_1 EAST
g_elimination_test_1=testTeamEliminationBruteForce g_teams_test_1 g_games_test_1   "team_2" 
relevantgames = gamesForMaxFlowElimination g_teams_test_1 g_games_test_1  "team_5" 
gamesSummary   =  gamesToPlaySummary  relevantgames
stand = standing g_teams_test_1 relevantgames EAST 
g_elist_test_1  = eliminationBruteForce g_teams_test_1 g_games_test_1  


 