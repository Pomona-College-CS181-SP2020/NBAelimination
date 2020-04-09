import Elimination
import System.Environment
import System.Directory
import System.Exit
import System.IO


main :: IO ()
main = runEliminationTests

-------------------------- elimintation tests   --------------------------------------------------------------
runEliminationTests::IO ()
runEliminationTests = do                     
              let testdir ="test"               
              elimination_0 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_noelimination.csv" )
              printResult "no elimination" elimination_0 []           
 
              elimination_1 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_1.csv" )
              elimination_1_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_1.csv") 
              printResult "team 8 eliminated -trivial" elimination_1 elimination_1_exp
              
              elimination_2 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_2.csv" )
              elimination_2_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_2.csv") 
              printResult "teams 8 eliminated-non trivial" elimination_2 elimination_2_exp
              
              elimination_3 <- (eliminationMaxFlowFromFile  "test/teamstest.csv"  "test/gamestest_3.csv" )
              elimination_3_exp <- (eliminationBruteForceFromFile    "test/teamstest.csv"  "test/gamestest_3.csv") 
              printResult "teams 4,6,8 eliminated-non trivial" elimination_3 elimination_3_exp
              
              --- long test ----
              let nbaTeams  =  loadTeams "test/teamsnba.csv"          
              nbaGames'  <- setcutofDate  (loadGames "test/gamesnba.csv" nbaTeams) "10/4/2019 00:00"
              nbaTeams' <- nbaTeams
              let elimination_4 =  eliminationMaxFlow'  nbaTeams' nbaGames'
                  elimination_4_exp =  eliminationBruteForce'  nbaTeams' nbaGames' 
              printResult "nba after 10/4/2019 00:00"  elimination_4 elimination_3_exp
              
              
appendResult::IO [Bool] -> IO Bool -> IO [Bool]
appendResult results res = do
                            results' <-  results
                            res'     <- res 
                            return  (results' ++ [res'])

printResult::Eq a => [Char] -> a -> a -> IO ()              
printResult test actual expected = if  actual /= expected then error ("test: " ++ test ++ " - failed ")  else  putStrLn("test: " ++ test ++ " - passed ")  