module GamesLib
(CONF(EAST),
CONF(WEST),
Game(Game),
Games,
Team(Team),
Teams,
teamname,
name,
hometeam,
awayteam, 
setTeamToWinGame,
allPossibleResults,
allPossibleResults',
getFirstPlacePoints',
addStanding',
points,
TeamScore(TeamScore),
Standings,
loadGames,
loadTeams,
setcutofDate,
setcutofRound,
standing,
standing',
getRelevantGames,
getRelevantGames',
gamesToPlaySummary,
gamesToPlaySummary',
getConf, 
maxPointsforTeam,
maxPointsforTeam',
gamesToPlay',
gamesToPlay) where



import MaxFlow
import Data.Dates
import Data.Time
import Data.List.Split
import Data.Maybe
import Data.Sort
import Data.Ord
import Data.Set (Set)
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------public interface ----------------------------------------------

data CONF = EAST |WEST deriving (Show, Eq)

data Game = Game {
                    round ::Int
                  , time :: Maybe LocalTime
                  , location :: String 
                  , hometeam :: String 
                  , awayteam :: String 
                  , result :: Maybe (Int, Int) 
                  } deriving (Show)

type Games = [Game]

--- NBA teams are assigned a conference (east or west)
data Team = Team {
                    name::String
                    ,conference::CONF
                  } deriving (Show) 

type Teams = [Team]

instance Eq Team where
  (Team name1 _ ) == (Team name2 _ ) = name1 == name2

-- the points for each team 
data TeamScore = TeamScore{
                    teamname::String
                    ,conf::CONF
                    ,points::Int 
                  } deriving (Show) 

type Standings = [TeamScore]

instance Eq TeamScore where
  (TeamScore name1 conf1 points1) == (TeamScore name2 conf2 points2) = points1 == points2

instance Ord TeamScore where
  compare  (TeamScore name1 conf1 points1) (TeamScore name2 conf2 points2)  | points1 == points2 = EQ
                                                                            | points1 < points2 = LT
                                                                            | otherwise = GT
                

--- loads the teams information from a file 
loadTeams::String -> IO Teams    
loadTeams fileName =   do
                        content <- readFile fileName                         
                        let thelines = tail (lines content)
                        return (map linetoTeam thelines)   


--- The games remaining to play 
gamesToPlay:: IO Games -> IO Games
gamesToPlay games =   do 
                        games' <- games
                        return (gamesToPlay' games')

gamesToPlay':: Games -> Games
gamesToPlay' games = (filter (\(Game round t location hometeam awayteam result ) -> result == Nothing ) games)


--- Load the teams information from a file
linetoTeam :: String -> Team 
linetoTeam line =  let tokens = (splitOn "," line)::[String]
                       name  =  (tokens!!0) :: String 
                       conference  =  filter (\c -> isAlpha c) $ (tokens!!1) :: String   
                       cnf = if conference == "east" then EAST else if   conference == "west" then WEST else error "bad conference"  
                   in (Team name cnf ) 

--- Load the NBA game results from a file 
loadGames::String -> IO Teams -> IO Games    
loadGames fileName teams =   do
                        content <- readFile fileName                         
                        teams' <- teams 
                        let thelines = tail (lines content)
                            games = (map linetoGame $ filter(\l -> length l > 10 && not (startsWithstr "--" l) )  thelines  )
                            games' = if checkGames games teams' then games else error "invaid game data " 
                        return games'


--- Get the standing of the teams
standing:: IO Teams-> IO Games ->  CONF -> IO Standings
standing teams games conf = do 
                        games' <- games
                        teams' <- teams
                        return (standing' teams' games' conf)

standing':: Teams-> Games -> CONF -> Standings 
standing' teams games conf = let emptymap = (Map.fromList [])::Map.Map String  TeamScore
                                 updatedmap =  foldl (\acc game  ->   prcoessGame teams game acc) emptymap games     
                                 l = sort  $  map(\x -> snd x)  $ Map.toList updatedmap                               
                                 in  ( filter (\(TeamScore _ conf' points) -> conf == conf') l )


--- Generate a play  summary out of a given list of games. The output is a pair of teams and the number of remaining games btween them. 
gamesToPlaySummary:: IO Games -> IO  [(String,String,Int)]
gamesToPlaySummary games = do 
                               games' <- games 
                               return (gamesToPlaySummary' games'  )                             

gamesToPlaySummary':: Games -> [(String,String,Int)]
gamesToPlaySummary' games =    let emptymap = (Map.fromList [])::Map.Map (String, String) Int
                                   updatedmap = foldl (\acc game  ->   addgameToPlaySummary game acc) emptymap $ gamesToPlay' games
                                   l =  (Map.toList updatedmap )   
                                   in map(\((team1,team2),val) -> (team1,team2,val) ) l



-- Calculate the maximum possible points a team can get 
maxPointsforTeam :: IO Teams -> IO Games -> String -> IO Int 
maxPointsforTeam  teams games team = do
                                        teams' <- teams
                                        games' <- games
                                        return (maxPointsforTeam' teams' games' team)


maxPointsforTeam' :: Teams -> Games -> String -> Int 
maxPointsforTeam' teams games team = let teamgames = filter (\(Game _ _ _ hometeam awayteam _ ) -> hometeam == team ||  awayteam == team) games

                                       --- get team conference 
                                         conf =  getConf teams team
                                       
                                       --- set team to win all its remaining games 
                                         adjustedgames = map (\g -> setTeamToWinGame g team ) teamgames   
                                         
                                         stand = filter (\score ->  teamname score == team ) $ standing' teams adjustedgames conf 
                                   
                                        --- return the maximum possible points the team can have
                                     in (points $ head  stand )

--- Return the points for the  team in the first place 
getFirstPlacePoints:: IO Standings ->  IO Int
getFirstPlacePoints standings  = do 
                                     standings' <- standings
                                     return (getFirstPlacePoints' standings')

getFirstPlacePoints':: Standings ->  Int 
getFirstPlacePoints' standings  = points $  last (sort standings)



-- Erease the results for games after specific date so we have some data to work with                      
setcutofDate:: IO Games -> String -> IO Games
setcutofDate games s  = do
                        games' <- games 
                        return (setcutofDate'  games' s) 
                      


setcutofDate':: Games -> String -> Games
setcutofDate' games s =  case strtoTime  s of
                            Nothing -> error "Bad date- date should be provided the format of dd/mm/yyyy hh:mm "
                            ( Just ltime) ->   map (\(Game round t location hometeam awayteam result ) -> if t <  ( Just ltime) then  (Game round t location hometeam awayteam result ) else (Game round t location hometeam awayteam Nothing ) ) games    


-- Erease the results for games after specific round so we have some data to work with                      
setcutofRound:: IO Games -> Int -> IO Games
setcutofRound games r  = do
                        games' <- games 
                        return (setcutofRound'  games' r) 
                      

setcutofRound':: Games -> Int -> Games
setcutofRound' games r =  map (\(Game round t location hometeam awayteam result ) -> if round <= r then  (Game round t location hometeam awayteam result ) else (Game round t location hometeam awayteam Nothing ) ) games 
  
-----------------------------------implementation functions----------------------------------------------------------

--- fake result of home team winning or away team winning 
homeWin = (1,0)
awayWin = (0,1)

-- check if the games data is well formatted  
checkGames::Games -> Teams -> Bool 
checkGames [] teams = True
checkGames games []  = False   
checkGames ((Game round t location hometeam awayteam result ):xs)  teams = let t1 = isNothing (getTeam hometeam teams)
                                                                               t2 = isNothing (getTeam awayteam teams)
                                                                           in ( if t1 || t2 then error $ "bad data" ++ hometeam ++ " " ++  awayteam  else checkGames xs teams   )
                                                                               
                                                                                                                                                           

--- return team information 
getTeam::String -> Teams -> (Maybe Team)     
getTeam teamname []  = Nothing 
getTeam teamname ((Team name conference):xs)  = if name == teamname then (Just  (Team name conference))  else  getTeam teamname xs    

--- Convert a single line of input into a Game type 
linetoGame:: String -> Game
linetoGame line  =  let tokens = (splitOn "," line)::[String]
                        round = read (tokens!!0) :: Int   
                        time = strtoTime (tokens!!1)
                        location = tokens!!2                      
                        hometeam = tokens!!3
                        awayteam = tokens!!4
                        result =  gameresult (tokens!!5 )
                    in (Game round  time location  hometeam awayteam result )


-- Extract the result of a game from a string or return Nothing if not exists. For example, "100-96" become (100,96)
gameresult:: String -> Maybe(Int,Int) 
gameresult str  = let  tokens = (splitOn "-" str)::[String]
                  in if (length tokens) /= 2 then Nothing else Just ((read (tokens!!0) :: Int   , read (tokens!!1) :: Int  ))


--make sure that the date string has 2 digits for the month and the day in the month. 
formatDatestr :: [Char] -> [Char]
formatDatestr str =   let tokens = (splitOn "/" str)::[String]                
                          day  = if length (tokens!!0)  < 2 then "0" ++(tokens!!0) else tokens!!0
                          month  = if length (tokens!!1)  < 2 then "0" ++ (tokens!!1) else tokens!!1
                      in   day ++ "/" ++   month ++ "/" ++ tokens!!2
                        
--- Convert a date string into a date
strtoTime :: [Char] -> Maybe LocalTime
strtoTime str = case  ((parseTime  defaultTimeLocale  "%d/%m/%Y %H:%M" (formatDatestr(str)) )::Maybe ZonedTime )of                   
                    Nothing -> Nothing
                    Just (ZonedTime ltime zone) -> (Just ltime)  


--- Return a distinc list of teams 
teams:: IO Games -> IO [String]
teams games =  do 
                    games' <- games
                    return (teams' games')


teams':: Games -> [String]
teams' games =  let teams = map (\(Game  _ _ _  hometeam _ _) -> hometeam)  games
                    in  (foldr  (\x acc -> if elem x acc then acc else x:acc ) [] teams)


--- Gets a teams score 
getScore::String -> Standings -> Maybe TeamScore
getScore teamName [] =  Nothing 
getScore teamName ((TeamScore name conf points):xs) = if teamName == name then Just (TeamScore name conf points) else getScore teamName xs 


--- checks is a string starts with a given prefix 
startsWithstr :: Eq a => [a] -> [a] -> Bool
startsWithstr [] _ = True;
startsWithstr x [] = False;
startsWithstr (x:xs) (y:ys) =  x == y && startsWithstr  xs ys


--- Add(merge) a score to a list of scores (standing)
addScore::Standings -> TeamScore -> Standings
addScore standing (TeamScore name conf points) = let ret = case  getScore name standing of 
                                                                Nothing ->  (TeamScore name conf points) : standing 
                                                                Just (TeamScore name' conf' points') -> map (\(TeamScore name'' conf'' points'') ->  (TeamScore name'' conf'' (if name'' == name then (points'' + points) else points'') ) )standing
                                                 in (sort ret )

addStanding':: Standings -> Standings ->Standings
addStanding' stand1 stand2  = foldr (\x acc ->  addScore acc x ) stand2 stand1 

--- Combine 2 standings 
addStanding:: IO Standings -> IO Standings -> IO Standings
addStanding stand1 stand2  = do 
                                stand1' <- stand1 
                                stand2' <- stand2
                                return (addStanding' stand1' stand2')                                



--- process a single game and add the points to the winning team
prcoessGame:: Teams-> Game ->  Map.Map String TeamScore -> Map.Map String  TeamScore
prcoessGame teams (Game round t location hometeam awayteam Nothing ) scoremap = scoremap
prcoessGame teams (Game round t location hometeam awayteam (Just(home,away)) ) scoremap = let (wteam,lteam)  =  if home >away then (hometeam,  awayteam) else (awayteam,hometeam)                                                                                              
                                                                                              wconf = getConf teams wteam
                                                                                              lconf = getConf teams lteam                                                                                               
                                                                                              updated = case Map.lookup wteam scoremap of 
                                                                                                         Nothing -> Map.insert  wteam (TeamScore wteam wconf 1) scoremap
                                                                                                         (Just (TeamScore _ conf points)) ->  Map.insert  wteam (TeamScore wteam conf (points + 1) ) scoremap
                                                                                              updated2 = case Map.lookup lteam scoremap of 
                                                                                                         Nothing -> Map.insert  lteam (TeamScore lteam lconf 0) updated
                                                                                                         (Just (TeamScore _ conf points)) -> updated      
                                                                                          in  updated2 
--- Get the conference for the team
getConf :: [Team] -> [Char] -> CONF                                                                                   
getConf teams teamName  = let (Team name conference) = head $ filter ( \(Team name conference) -> name == teamName) teams
                          in conference  


--- tranform a pair of teams into a canonical form.
canonicalform:: Ord b => (b, b) -> (b, b)
canonicalform (x,y) = if x < y then (x,y) else (y,x) 



--- add a agmes between 2 teams to a games summary map
addgameToPlaySummary:: Num a => Game -> Map.Map (String, String) a -> Map.Map (String, String) a
addgameToPlaySummary (Game round t location hometeam awayteam result) m =  let key = canonicalform (hometeam,awayteam)
                                                                               updated  = case Map.lookup key m of 
                                                                                            Nothing -> Map.insert  key 1 m 
                                                                                            ((Just val) )->  Map.insert  key (1+val) m 
                                                                           in updated                                                                                      

--- Take a list of unplayed games and generate a list of lists of those games with all possible outcomes (home team wins or away team wins)
allPossibleResults::IO Games  -> IO  [Games]
allPossibleResults games = do
                            games' <- games
                            return ( allPossibleResults' games' [] )

allPossibleResults'::Games  -> Games  -> [Games]
allPossibleResults' [] build  =  [build]
allPossibleResults' ((Game round t location hometeam awayteam Nothing):xs) build  = (allPossibleResults' xs (build ++ [(Game round t location hometeam awayteam (Just homeWin ) )])  ) ++ (allPossibleResults' xs (build ++ [(Game round t location hometeam awayteam (Just awayWin ) )])  )
allPossibleResults' ((Game round t location hometeam awayteam (Just res )):xs) _  = error "allPossibleResults error : Games should not have result"


---For a team that is tested for elimination, keep only the relevant games.
-- Games btween 2 teams that can't end up before the given team are filtered out. 
--- In addition set all the remaining games  for the tested team to winning  
getRelevantGames::IO Teams-> IO Games -> String -> IO Games
getRelevantGames teams games team = do
                                        teams' <- teams          
                                        games' <- games 
                                        return (getRelevantGames'  teams'  games' team )     


getRelevantGames'::Teams-> Games -> String -> Games
getRelevantGames' teams games team =      ---  set the team to win all its remaining games 
                                         let adjustedgames = map (\g -> setTeamToWinGame g  team ) games
                                          
                                                --- calculate the maximum point the team could get                                            
                                             maxPoints = maxPointsforTeam' teams games team 

                                                --- filter out non relevant games                                          
                                             adjustedgamesFiltered = filter (\g -> maxPointsforTeam' teams games  (hometeam g) >=  maxPoints || maxPointsforTeam' teams games  (awayteam g) >= maxPoints) adjustedgames   

                                         in(adjustedgamesFiltered)


--- Set a result of a non played game to be a win for a given team if played by this team   
setTeamToWinGame :: Game -> String -> Game
setTeamToWinGame (Game round t location hometeam awayteam Nothing)  team = (Game round t location hometeam awayteam (if team == hometeam then Just homeWin else if  team == awayteam then Just awayWin else Nothing))  
setTeamToWinGame game   team = game    



g_teams = loadTeams "teamstest.csv"
g_games = loadGames "gamestest_2.csv" g_teams


