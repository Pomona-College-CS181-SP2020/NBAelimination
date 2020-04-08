module Elimination 
(eliminationMaxFlow,eliminationMaxFlow',eliminationMaxFlowFromFile,eliminationBruteForce',eliminationBruteForce,eliminationBruteForceFromFile,loadTeams,loadGames,setcutofRound,setcutofDate,testTeamEliminationBruteForce) where

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
 
 

--- Test alimination for all teams using brute force  and return a list of eliminated teams
eliminationBruteForce:: IO Teams-> IO Games -> IO [String]
eliminationBruteForce teams games =  do
                                        teams' <- teams          
                                        games' <- games 
                                        return (eliminationBruteForce'   teams'  games'  )                

eliminationBruteForce':: Teams-> Games -> [String]
eliminationBruteForce' teams games =  foldr(\team acc -> if  snd ( testTeamEliminationBruteForce'  teams games (name team) )  then ( (name team):acc) else acc) []  teams 
 
-----------------------------------implementation functions--------------------------------------------------------------

--- The label of source vertex in the networkFlow 
sourceVertex = "s" 

--- The label of the sync vertex in the networkFlow 
syncVertex = "t"  


--- brutce force all scenarios to test if a team is eliminated or not.  
testTeamEliminationBruteForce:: IO Teams-> IO Games -> String -> IO  (Games,Bool)
testTeamEliminationBruteForce  teams  games team = do
                                                      teams' <- teams          
                                                      games' <- games 
                                                      return (testTeamEliminationBruteForce'  teams'  games' team )

testTeamEliminationBruteForce':: Teams-> Games -> String -> (Games,Bool)
testTeamEliminationBruteForce'  teams  games team = 
                                                      --- get relevant games   
                                                      let relevantgames = getRelevantGames' teams games team 
                                                      
                                                        --- Set the remaining games of the team to be winning
                                                          teamGames = map (\g -> setTeamToWinGame g  team ) $ filter (\g -> hometeam g == team || awayteam g == team) $ gamesToPlay' games
                                                                                                                
                                                        --- calculate the maximum point the team could get                                            
                                                          maxPoints = maxPointsforTeam' teams games team 
                                                          
                                                      
                                                        --- get team conference 
                                                          conf =  getConf teams team
                                                      
                                                        --- compute standing so far       
                                                          standing = standing' teams relevantgames conf
                                                      
                                                        --- Now get all the remaining games
                                                          gamestoplay = gamesToPlay' relevantgames 
                                                                                                                                                         
                                                          
                                                        --- Get all possible results of the remaining games
                                                          outcomes = allPossibleResults' gamestoplay [] 

                                                        --- check if any of the possible outcome can lead for the given team to be the first
                                                          (games',eliminated') = foldr (\outcome  (g,eliminated)-> if not eliminated 
                                                                                                     then (g,eliminated)  
                                                                                                     else if  getFirstPlacePoints' (addStanding' standing  (standing' teams outcome conf)) <= maxPoints 
                                                                                                          then (outcome,False) 
                                                                                                          else  (g,eliminated)) ([],True)  outcomes                                                       
                                                                                                          
                                                      
                                                      in ( if eliminated' then (games',eliminated') else (teamGames++games',eliminated') )


-- Get the list of games that are relevant for the MaxFlow. This includes games that were not played yet excluding teams that 
-- can't be in the first place  and also the team that is tested for elimination.
gamesForMaxFlowElimination::IO Teams -> IO Games -> String ->  IO Games
gamesForMaxFlowElimination teams games team = do 
                                                 games' <- games 
                                                 teams' <- teams 
                                                 return (gamesForMaxFlowElimination' teams'  games' team)

gamesForMaxFlowElimination'::Teams -> Games -> String -> Games
gamesForMaxFlowElimination' teams games team = gamesToPlay' $ getRelevantGames' teams games team


--- Build the source vertex of the network flow graph that has edges to each pair of teams with a capacity that is the number of games 
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



--- Build the vertices that represent the games between 2 teams. From each of those vertices there is 2 edges ,1 for each team with 
--- infinit capacity.
buildGamesVertices:: IO [(String, String, Int)] -> IO  Graph 
buildGamesVertices gSummary = do 
                               gSummary' <-  gSummary
                               return (buildGamesVertices' gSummary')

buildGamesVertices':: [(String, String, Int)] -> Graph 
buildGamesVertices' [] = []
buildGamesVertices' ((team1,team2,geamsCount):xs)  =  (Vertex (team1 ++ "-" ++ team2) [ (team1,maxBound::Int) ,(team2,maxBound::Int) ]  (maxBound::Int)  ""):(buildGamesVertices' xs)          


--- Build the vertices that the teams. Each vertex has an edge to the sync vertex with a capacity equal to the maximum points possible for the team to gain without capturing the first place in the conference of the tested team.
--- If the team is in a different conference , the capcity is set to infinity.
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
                                                                 ret = foldr ( \(TeamScore teamname conf' points  ) acc -> if elem teamname  relevantTeams  then
                                                                                                                              if teamMaxPoints==points then (Vertex teamname [] (maxBound::Int)  "" ):acc
                                                                                                                              else (Vertex teamname [ (syncVertex ,if conf == conf' then  teamMaxPoints-points else (maxBound::Int))] (maxBound::Int)  "" ):acc
                                                                                                                           else  acc  
                                                                                                                    )[]   stand                                    
                                                             in ret     


---Build the full maxflow graph to test elimination of a given team - take the teams and relevant games and return a graph
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
 

--- Test whether a given  team is eliminated or not
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
                                                


--- debug helper function 
testTeamEliminationMaxFlowDebug teams games team = do 
                                                 teams' <- teams
                                                 games' <- games
                                                 return (testTeamEliminationMaxFlowDebug' teams' games' team)
 

testTeamEliminationMaxFlowDebug' teams games team =  let maxFlowGraph = buildGraphFromGames' teams games team
                                                         numberOfGames = length $ gamesForMaxFlowElimination' teams games team 
                                                         simpleElimination = checkForNegativeCapacity maxFlowGraph
                                                         maxFlow =  solveMaxFlow maxFlowGraph sourceVertex syncVertex
                                                         
                                                     in (snd maxFlow,numberOfGames,simpleElimination,(maxPointsforTeam' teams games team ),maxFlowGraph)
 

--- Some ad hoc declation for debugging  
g_teams = loadTeams "teamsnba.csv"     
g_games_all = loadGames "gamesnba.csv"  g_teams   
g_games = setcutofRound g_games_all  10 
g_games_toplay = gamesToPlay g_games          
g_east_standing = standing g_teams g_games EAST
g_west_standing = standing g_teams g_games WEST
g_gamestoplay = gamesToPlay g_games 
g_elimination=testTeamEliminationBruteForce g_teams g_games "Toronto Raptors"
--relevantgames = gamesForMaxFlowElimination g_teams g_games "Toronto Raptors" 

g_teams_test_1 = loadTeams "teamstest.csv"     
g_games_all_test_1 = loadGames "gamestest_3.csv"  g_teams_test_1   
g_games_test_1 = g_games_all_test_1
g_east_standing_test_1 = standing g_teams_test_1 g_games_test_1 EAST


g_elimination_test_1=testTeamEliminationBruteForce g_teams_test_1 g_games_test_1   "team_6" 
debug=testTeamEliminationMaxFlowDebug g_teams_test_1 g_games_test_1   "team_6" 

maxFlowGames = gamesForMaxFlowElimination g_teams_test_1 g_games_test_1 "team_6"  
st = (standing g_teams_test_1 g_games_test_1 EAST )
maxFlowGamesSummary = gamesToPlaySummary maxFlowGames
source = buildSourceVertex maxFlowGamesSummary
gameVertices = buildGamesVertices maxFlowGamesSummary
teamVertices = buildTeamsVertices st g_teams_test_1 maxFlowGames  "team_6"    5






relevantgames = gamesForMaxFlowElimination g_teams_test_1 g_games_test_1  "team_5" 
gamesSummary   =  gamesToPlaySummary  relevantgames
stand = standing g_teams_test_1 relevantgames EAST 
g_elist_test_1  = eliminationBruteForce g_teams_test_1 g_games_test_1  


 