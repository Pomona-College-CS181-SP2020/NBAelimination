import Elimination
import GamesLib
import MaxFlow
import System.Environment
import System.Directory
import System.Exit
import System.IO
import Numeric.LinearProgramming


main :: IO ()
main = runEliminationTests 


-------------------------- elimintation tests   --------------------------------------------------------------
runEliminationTests::IO ()
runEliminationTests = do                     
              let testdir ="test"               
              elimination_0 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_noelimination.csv" )
              printResult "\nelimination test 1: no elimination" elimination_0 []           
 
              elimination_1 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_1.csv" )
              elimination_1_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_1.csv") 
              printResult "elimination test 2: team 8 eliminated,trivial" elimination_1 elimination_1_exp
              
              elimination_2 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_2.csv" )
              elimination_2_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_2.csv") 
              printResult "elimination test 3: teams 8 eliminated,non trivial" elimination_2 elimination_2_exp
              
              elimination_3 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_3.csv" )
              elimination_3_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_3.csv") 
              printResult "elimination test 4: teams 4,6,8 eliminated,non trivial" elimination_3 elimination_3_exp
              
              --- long test ----
              let nbaTeams  =  loadTeams "test/teamsnba.csv"          
              nbaGames'  <- setcutofDate  (loadGames "test/gamesnba.csv" nbaTeams) "10/4/2019 00:00"
              nbaTeams' <- nbaTeams
              let elimination_4 =  eliminationMaxFlow'  nbaTeams' nbaGames'
                  elimination_4_exp =  eliminationBruteForce'  nbaTeams' nbaGames' 
              printResult "elimination test 5: nba after 10/4/2019 00:00"    elimination_4 elimination_4_exp

              --- based on https://en.wikipedia.org/wiki/2018%E2%80%9319_NBA_season
              let nbaStandingEastEndofSEeason = [TeamScore {teamname = "New York Knicks", conf = EAST, points = 17},TeamScore {teamname = "Cleveland Cavaliers", conf = EAST, points = 19},TeamScore {teamname = "Chicago Bulls", conf = EAST, points = 22},TeamScore {teamname = "Atlanta Hawks", conf = EAST, points = 29},TeamScore {teamname = "Washington Wizards", conf = EAST, points = 32},TeamScore {teamname = "Charlotte Hornets", conf = EAST, points = 39},TeamScore {teamname = "Miami Heat", conf = EAST, points = 39},TeamScore {teamname = "Detroit Pistons", conf = EAST, points = 41},TeamScore {teamname = "Brooklyn Nets", conf = EAST, points = 42},TeamScore {teamname = "Orlando Magic", conf = EAST, points = 42},TeamScore {teamname = "Indiana Pacers", conf = EAST, points = 48},TeamScore {teamname = "Boston Celtics", conf = EAST, points = 49},TeamScore {teamname = "Philadelphia 76ers", conf = EAST, points = 51},TeamScore {teamname = "Toronto Raptors", conf = EAST, points = 58},TeamScore {teamname = "Milwaukee Bucks", conf = EAST, points = 60}]
              standnba <- standing  nbaTeams  (loadGames "test/gamesnba.csv" nbaTeams) EAST 
              printResult "GamesLib test 1: computation of standing from of nba east conference at end of season" ( toListofTouples standnba) ( toListofTouples nbaStandingEastEndofSEeason)

              --- based on https://en.wikipedia.org/wiki/2018%E2%80%9319_NBA_season
              let nbaStandingWEsttEndofSEeason = [TeamScore {teamname = "Phoenix Suns", conf = WEST, points = 19},TeamScore {teamname = "Dallas Mavericks", conf = WEST, points = 33},TeamScore {teamname = "Memphis Grizzlies", conf = WEST, points = 33},TeamScore {teamname = "New Orleans Pelicans", conf = WEST, points = 33},TeamScore {teamname = "Minnesota Timberwolves", conf = WEST, points = 36},TeamScore {teamname = "Los Angeles Lakers", conf = WEST, points = 37},TeamScore {teamname = "Sacramento Kings", conf = WEST, points = 39},TeamScore {teamname = "LA Clippers", conf = WEST, points = 48},TeamScore {teamname = "San Antonio Spurs", conf = WEST, points = 48},TeamScore {teamname = "Oklahoma City Thunder", conf = WEST, points = 49},TeamScore {teamname = "Utah Jazz", conf = WEST, points = 50},TeamScore {teamname = "Houston Rockets", conf = WEST, points = 53},TeamScore {teamname = "Portland Trail Blazers", conf = WEST, points = 53},TeamScore {teamname = "Denver Nuggets", conf = WEST, points = 54},TeamScore {teamname = "Golden State Warriors", conf = WEST, points = 57}]
              stand <- standing  nbaTeams  (loadGames "test/gamesnba.csv" nbaTeams) WEST 
              printResult "GamesLib test 2: computation of standing from of nba west conference at end of season" (toListofTouples stand) (toListofTouples nbaStandingWEsttEndofSEeason)


              let stand1 = [TeamScore {teamname = "teams_1", conf = WEST, points = 19} ,TeamScore {teamname = "teams_2", conf = WEST, points = 18}   ] 
                  stand2 = [TeamScore {teamname = "teams_1", conf = WEST, points = 5} ,TeamScore {teamname = "teams_2", conf = WEST, points = 3} ,TeamScore {teamname = "teams_3", conf = WEST, points = 6}  ] 
                  stand = addStanding' stand1 stand2
                  stand_exp = [TeamScore {teamname = "teams_3", conf = WEST, points = 6},TeamScore {teamname = "teams_2", conf = WEST, points = 21},TeamScore {teamname = "teams_1", conf = WEST, points = 24}]
              printResult "GamesLib test 3: merge 2 standings" (toListofTouples stand) (toListofTouples stand_exp)
              
              
              let stand1 = [] 
                  stand2 = [TeamScore {teamname = "teams_1", conf = WEST, points = 5} ,TeamScore {teamname = "teams_2", conf = WEST, points = 3} ,TeamScore {teamname = "teams_3", conf = WEST, points = 6}  ] 
                  stand = addStanding' stand1 stand2
                  stand_exp = stand2
              printResult "GamesLib test 4: merge 2 standings with one standing empty" (toListofTouples stand) (toListofTouples stand_exp)

             
              let nicksMaxPoints =  maxPointsforTeam'  nbaTeams' nbaGames' "New York Knicks"   
                  nicksMaxPoints_exp = 18              
              printResult "GamesLib test 5: compute maximum possible points for Nicks given the standing before the last round " nicksMaxPoints nicksMaxPoints_exp
              
              let network =  [  Vertex "0" [("1",16), ("2",13)  ] (maxBound::Int)  "", Vertex "1" [("2",10), ("3",12)  ] (maxBound::Int) "",  Vertex "2" [("4",14) ,("1",4)        ] (maxBound::Int) ""    ,  Vertex "3" [ ("5",20), ("2",9)] (maxBound::Int) "" ,  Vertex "4" [("5",4), ("3",7) ] (maxBound::Int) "" ,   Vertex "5" [ ] (maxBound::Int) ""  ] 
                  solution = solveMaxFlow network "0" "5"
                  solution_exp = 23
              printResult "Maxflow test 1 based on example from https://www.geeksforgeeks.org/max-flow-problem-introduction/" (snd solution) solution_exp


              let network = [  Vertex "s" [("u",10), ("v",10)  ] (maxBound::Int)  "", Vertex "u" [("v",15), ("t",5)  ] (maxBound::Int) "",  Vertex "v" [("t",10)  ]  (maxBound::Int)  "" ,  Vertex "t" [ ] (maxBound::Int)  ""   ]
                  solution = solveMaxFlow network "s" "t"
                  solution_exp = 15 
              printResult "Maxflow test 2 based on example from https://en.wikipedia.org/wiki/Maximum_flow_problem#/media/File:MFP1.jpg" (snd solution) solution_exp
              
              let network =[  Vertex "s" [("a",10), ("c",8)  ] (maxBound::Int)  "", Vertex "a" [("b",5), ("c",2)  ] (maxBound::Int) "",  Vertex "b" [("t",7)  , ("d",8)  ]  (maxBound::Int)  "" ,  Vertex "c" [ ("d",10)] (maxBound::Int)  ""      ,  Vertex "d" [ ("t",10)] (maxBound::Int)  "",  Vertex "t" [ ] (maxBound::Int) ""        ]
                  solution = solveMaxFlow network "s" "t"
                  solution_exp = 15 
              printResult "Maxflow test 3 based on example from https://www.hackerearth.com/practice/algorithms/graphs/maximum-flow/tutorial/" (snd solution) solution_exp

              let network = [  Vertex "0" [("1",20), ("3",10) , ("2",30)  ] (maxBound::Int)  "", Vertex "1" [("4",30), ("2",40)  ] (maxBound::Int) "",  Vertex "2" [("4",20) ,("3",10)    ]  (maxBound::Int)  "" ,  Vertex "3" [ ("4",20),("2",10)]  (maxBound::Int)  "" ,  Vertex "4" [ ] (maxBound::Int) ""    ]
                  solution = solveMaxFlow network "0" "4"
                  solution_exp = 60
              printResult "Maxflow test 4 based on example from https://developers.google.com/optimization/flow/maxflow" (snd solution) solution_exp 

              let network = [  Vertex "s" [("a",7), ("d",4)  ] (maxBound::Int)  "", Vertex "a" [("b",5), ("c",3)  ] (maxBound::Int) "",  Vertex "b" [("t",8)   ]  (maxBound::Int)  "" ,  Vertex "c" [ ("b",3),("t",5)] (maxBound::Int)  "" , Vertex "d" [ ("a",3),("c",2)] (maxBound::Int)  ""      , Vertex "t" [ ] (maxBound::Int) ""    ]
                  solution = solveMaxFlow network "s" "t"
                  solution_exp = 10
              printResult "Maxflow test 5 based on example from https://cp-algorithms.com/graph/edmonds_karp.html" (snd solution) solution_exp 
             
              let network = [  Vertex "1" [("2",1), ("0",1)  ] (maxBound::Int)  "", Vertex "0" [("1",1), ("3",1)  ] (maxBound::Int) "",  Vertex "2" [("1",1)    ]  (maxBound::Int)  "" ,  Vertex "3" [ ("0",1),("7",1),("4",1)] (maxBound::Int)  ""      ,  Vertex "4" [ ("7",1), ("3",1),("6",1),("5",1) ](maxBound::Int)  "" , Vertex "5" [ ("6",1), ("4",1) ](maxBound::Int)  ""  ,Vertex "6" [ ("5",1), ("4",1), ("7",1) ](maxBound::Int)  "" , Vertex "7" [ ("3",1), ("4",1), ("6",1) ](maxBound::Int)  "" ]
                  solution = bfs_path network "0" "7"
                  solution_exp = ["0","3","7"]             
              printResult "BFS test 1 based on example from https://www.geeksforgeeks.org/shortest-path-unweighted-graph/" solution solution_exp 

              downloadGamesfromWeb  "https://fixturedownload.com/download/nba-2019-EasternStandardTime.csv" "test/nba-2019-EasternStandardTime.csv"
              elimination_nba_2019 <- eliminationMaxFlowFromFile  "test/teamsnba.csv"  "test/nba-2019-EasternStandardTime.csv"
              let elimination_nba_2019_exp = ["Brooklyn Nets","New York Knicks","Detroit Pistons","Chicago Bulls","Cleveland Cavaliers","Orlando Magic","Charlotte Hornets","Washington Wizards","Atlanta Hawks","Portland Trail Blazers","Minnesota Timberwolves","Golden State Warriors","Sacramento Kings","Phoenix Suns","San Antonio Spurs","New Orleans Pelicans"]
              printResult "filnal test elimination of nba 2019-2020 suspended season" elimination_nba_2019 elimination_nba_2019_exp 
             
    --- 


--- check the test result against  the expected result
printResult::Eq a => [Char] -> a -> a -> IO ()              
printResult test actual expected = if  actual /= expected then error ( test ++ " - failed ")  else  putStrLn(test ++ " - passed ")  

--- Convert  standings to list of touples for having the operator  (== ) test each field
toListofTouples::Standings -> [(String ,CONF,Int) ]
toListofTouples standings = map(\(TeamScore teamname conf points ) -> (teamname,conf,points)  ) standings