case class Building      (start: Double, end: Double, height: Double)
case class SkylineSegment(start: Double, end: Double, height: Double)

/* Input */

/*

#####
##$$#
@@$$#  ^^
@@$$# &^^&&&&&&
---------------
123456789012345

*/


val buildings = List(
    Building(3.1, 4, 3),  // 2 $
    Building(1.4, 5, 4),  // 3 #
    Building(1.8, 2, 2),  // 1 @

/*
=>

#####
#####
#####
#####
*/
    
    Building(8.2, 10, 2), // 4 ^
    Building(7.6, 15, 1)  // 5 &
/*
=>

       $$
      @$$%%%%%%
*/
)






/*
#####
#####
#####
#####
    

       $$
      @$$%%%%%%
*/



/* Output */

/*

#####
#####
#####  $$
##### @$$%%%%%%
---------------

*/

val skyline = List(
    SkylineSegment(1, 5, 4),  // # building 3 
    SkylineSegment(6, 6, 0),  //
    SkylineSegment(7, 7, 1),  // @ part of building 5
    SkylineSegment(8, 9, 2),  // $ building 4
    SkylineSegment(10, 15, 1) // % part of building 5 
)

Console.println("nothing to see")
