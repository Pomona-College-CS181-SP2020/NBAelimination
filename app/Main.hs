import Elimination
import System.Environment
import System.Directory
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
 
                                         
                    


--- some some ad hoc variables defnition for more testing and debugging 
g_nbaTeams  =  loadTeams "teamsnba.csv" 
g_nbaGames = setcutofDate  (loadGames "gamesnba.csv" g_nbaTeams) "10/4/2019 00:00"
res = eliminationMaxFlow  g_nbaTeams g_nbaGames
res' = eliminationBruteForce g_nbaTeams g_nbaGames

