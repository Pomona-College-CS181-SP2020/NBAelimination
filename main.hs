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
                                            
                    
                    