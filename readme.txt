Introduction: 
This is a library that checks ,in the middle of the season, which (NBA) team could not finish first in its conference  under any outcomes of the remaining games. This team is considered “eliminated”. Solving this problem in a brute force is  exponentials with the number of remaining games and hence not practical in many cases. The library converts our problem to the Maximum Flow problem and solves the problem in polynomial time (V^3).

Usage:
Install the following libraries by running :
cabal install sort 
cabal install dates 
cabal install split

Now test that everything is working by running  from GHCI command line 
:l main.hs 
and then run the tests 
runEliminationTests 


The simplest way to use the library is to run:  

eliminationMaxFlowFromFile  <teams file>    <games file> 
and you will get a list of the eliminated teams. 
