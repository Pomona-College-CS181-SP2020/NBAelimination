import Elimination
import GamesLib
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
              printResult "elimination test 5: nba after 10/4/2019 00:00"  elimination_4 elimination_4_exp

              let nbastandingEastEndofSEeason = [TeamScore {teamname = "New York Knicks", conf = EAST, points = 17},TeamScore {teamname = "Cleveland Cavaliers", conf = EAST, points = 19},TeamScore {teamname = "Chicago Bulls", conf = EAST, points = 22},TeamScore {teamname = "Atlanta Hawks", conf = EAST, points = 29},TeamScore {teamname = "Washington Wizards", conf = EAST, points = 32},TeamScore {teamname = "Charlotte Hornets", conf = EAST, points = 39},TeamScore {teamname = "Miami Heat", conf = EAST, points = 39},TeamScore {teamname = "Detroit Pistons", conf = EAST, points = 41},TeamScore {teamname = "Brooklyn Nets", conf = EAST, points = 42},TeamScore {teamname = "Orlando Magic", conf = EAST, points = 42},TeamScore {teamname = "Indiana Pacers", conf = EAST, points = 48},TeamScore {teamname = "Boston Celtics", conf = EAST, points = 49},TeamScore {teamname = "Philadelphia 76ers", conf = EAST, points = 51},TeamScore {teamname = "Toronto Raptors", conf = EAST, points = 58},TeamScore {teamname = "Milwaukee Bucks", conf = EAST, points = 60}]
              stand <- standing  nbaTeams  (loadGames "test/gamesnba.csv" nbaTeams) EAST 
              printResult "GamesLib test 1: standing from games  computation of nba east conference at end of season"  stand nbastandingEastEndofSEeason


              let nbastandingWesttEndofSEeason = [TeamScore {teamname = "Phoenix Suns", conf = WEST, points = 19},TeamScore {teamname = "Dallas Mavericks", conf = WEST, points = 33},TeamScore {teamname = "Memphis Grizzlies", conf = WEST, points = 33},TeamScore {teamname = "New Orleans Pelicans", conf = WEST, points = 33},TeamScore {teamname = "Minnesota Timberwolves", conf = WEST, points = 36},TeamScore {teamname = "Los Angeles Lakers", conf = WEST, points = 37},TeamScore {teamname = "Sacramento Kings", conf = WEST, points = 39},TeamScore {teamname = "LA Clippers", conf = WEST, points = 48},TeamScore {teamname = "San Antonio Spurs", conf = WEST, points = 48},TeamScore {teamname = "Oklahoma City Thunder", conf = WEST, points = 49},TeamScore {teamname = "Utah Jazz", conf = WEST, points = 50},TeamScore {teamname = "Houston Rockets", conf = WEST, points = 53},TeamScore {teamname = "Portland Trail Blazers", conf = WEST, points = 53},TeamScore {teamname = "Denver Nuggets", conf = WEST, points = 54},TeamScore {teamname = "Golden State Warriors", conf = WEST, points = 57}]
              stand <- standing  nbaTeams  (loadGames "test/gamesnba.csv" nbaTeams) WEST 
              printResult "GamesLib test 2: standing from games computation of nba west conference at end of season"  stand nbastandingWesttEndofSEeason
              
              
appendResult::IO [Bool] -> IO Bool -> IO [Bool]
appendResult results res = do
                            results' <-  results
                            res'     <- res 
                            return  (results' ++ [res'])

printResult::Eq a => [Char] -> a -> a -> IO ()              
printResult test actual expected = if  actual /= expected then error ( test ++ " - failed ")  else  putStrLn(test ++ " - passed ")  