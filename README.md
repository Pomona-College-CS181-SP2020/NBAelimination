Introduction: 
This is a library that checks ,in the middle of the season, which (NBA) team could not finish first in its conference  under any outcomes of the remaining games. This team is considered “eliminated”. Solving this problem in a brute force is  exponentials with the number of remaining games and hence not practical in many cases. The library converts our problem to the Maximum Flow problem and solves the problem in polynomial time (V^3).

Ensuring all works:
stack build 
stack test 


The simplest way to use the Elimination module is to call:  

eliminationMaxFlowFromFile  <teams file>    <games file> 
and you will get a list of the eliminated teams. 