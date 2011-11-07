//
// Used the same feature (repeated evaluation of rhs of =) that is typical
// in the definition of an Enumeration.
//

var i = 0

// a .. f as 1 .. 6
val a, b, c, d, e, f = {i += 1; i}
