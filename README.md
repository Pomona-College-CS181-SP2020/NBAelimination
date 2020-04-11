Introduction: 
Suppose that in the middle of an NBA season you would like to figure out if a specific team cannot finish first in its conference (i.e. "eliminated") even if it wins all of its remaining games. Of course, if the current leading team has a large enough gap that even if our team wins all of its games it cannot come in first, we can answer this question with a "yes" and our team is eliminated. But in other cases the solution is not trivial. We can try to evaluate with brute force all possible future game outcomes but the runtime will be exponential with the number of games yet to be played. However, we can use a graph theory called flow network to solve the problem in polynomial time (V^3). 

The library: 
The haskell library in this project solves the problem in a generic way. The library converts the problem to the Maximum Flow problem and solves it in polynomial time (V^3).

To ensure all works run :
stack test 


The simplest way to use the Elimination module is to call:  

eliminationMaxFlowFromFile  teams_file games_file

and you will get a list of the eliminated teams. 