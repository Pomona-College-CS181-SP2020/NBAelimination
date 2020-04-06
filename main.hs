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
              elimination_0 <- (eliminationMaxFlowFromFile  "teamstest.csv"  "gamestest_noelimination.csv" )
              printResult "no elimination" elimination_0 []           
 
              elimination_1 <- (eliminationMaxFlowFromFile  "teamstest.csv"  "gamestest_1.csv" )
              elimination_1_exp <- (eliminationBruteForceFromFile    "teamstest.csv"  "gamestest_1.csv") 
              printResult "team 8 eliminated" elimination_1 elimination_1_exp
              
              elimination_2 <- (eliminationMaxFlowFromFile  "teamstest.csv"  "gamestest_2.csv" )
              elimination_2_exp <- (eliminationBruteForceFromFile    "teamstest.csv"  "gamestest_2.csv") 
              printResult "team xxx eliminated" elimination_2 elimination_2_exp
              
              --- long test ----
              let nbaTeams  =  loadTeams "teamsnba.csv"          
              nbaGames'  <- setcutofDate  (loadGames "gamesnba.csv" nbaTeams) "10/4/2019 00:00"
              nbaTeams' <- nbaTeams
              let elimination_3 =  eliminationMaxFlow'  nbaTeams' nbaGames'
                  elimination_3_exp =  eliminationBruteForce'  nbaTeams' nbaGames' 
              printResult "nba after 10/4/2019 00:00"  elimination_3 elimination_3_exp
              
              
 
printResult::Eq a => [Char] -> a -> a -> IO ()              
printResult test actual expected = if  actual /= expected then putStrLn ("test " ++ test ++ " - failed ")  else  putStrLn("test " ++ test ++ " - passed ")  

--- some some ad hoc variables defnition for more testing and debugging 
g_nbaTeams  =  loadTeams "teamsnba.csv" 
g_nbaGames = setcutofDate  (loadGames "gamesnba.csv" g_nbaTeams) "10/4/2019 00:00"
res = eliminationMaxFlow  g_nbaTeams g_nbaGames
res' = eliminationBruteForce g_nbaTeams g_nbaGames

g_Teams  =  loadTeams "teamstest.csv" 
g_Games =   loadGames "gamestest_2.csv" g_Teams
res2 = eliminationMaxFlow  g_Teams g_Games
res2' = eliminationBruteForce g_Teams g_Games