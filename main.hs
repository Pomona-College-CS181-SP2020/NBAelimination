import Elimination
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main =  do 
            putStrLn  " Enter a team file " 
            teamFile <- getLine 
            putStrLn  " Enter a games  file " 
            gamesFile <- getLine 
            eliminated <- eliminationMaxFlowFromFile  teamFile  gamesFile
            putStrLn  $ "The eliminated teams are  "  ++  show  eliminated       
                                            
                    
-------------------------- elimintation tests   --------------------------------------------------------------
runEliminationTests = do                 
              elimination_1 <- (eliminationMaxFlowFromFile  "teams_test_1.csv"  "games_no_elimination.csv" )
              printResult "no elimination" elimination_1 []           
 
              elimination_2 <- (eliminationMaxFlowFromFile  "teams_test_1.csv"  "games_test_1.csv" )
              elimination_2_exp <- (eliminationBruteForceFromFile    "teams_test_1.csv"  "games_test_1.csv") 
              printResult "team 8 eliminated" elimination_2 elimination_2_exp
              
              --- long test ----
              let nbaTeams  =  loadTeams "teams.csv"          
              nbaGames'  <- cutofdate  (loadGames "nba.csv" nbaTeams) "10/4/2019 00:00"
              nbaTeams' <- nbaTeams
              let elimination_3 =  eliminationMaxFlow'  nbaTeams' nbaGames'
                  elimination_3_exp =  eliminationBruteForce'  nbaTeams' nbaGames' 
              printResult "nba after 10/4/2019 00:00"  elimination_3 elimination_3_exp
              
              
 
printResult::Eq a => [Char] -> a -> a -> IO ()              
printResult test actual expected = if  actual /= expected then putStrLn ("test " ++ test ++ " - failed ")  else  putStrLn("test " ++ test ++ " - passed ")  

--- some some ad hoc variables defnition for more testing and debugging 
g_nbaTeams  =  loadTeams "teams.csv" 
g_nbaGames = cutofdate  (loadGames "nba.csv" g_nbaTeams) "10/4/2019 00:00"
res = eliminationMaxFlow  g_nbaTeams g_nbaGames
res' = eliminationBruteForce g_nbaTeams g_nbaGames
