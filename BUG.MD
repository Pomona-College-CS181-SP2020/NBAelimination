bug 2020-04-05 #1 
The program does not handle well the case where there are no games left to play. In that case the solution is trivial and I should not use network flow at all.

bug 2020-04-06 #2 
The test "teams 6 and 8 are eliminated" is failing - brute force method and maxflow method  gives different results - need to investigate 