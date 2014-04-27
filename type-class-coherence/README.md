From https://gist.github.com/markhibberd/10784706

So what I understand to be your questions:
 1. What is a coherency problem?
 2. What does over constained code look like / cause?  
 3. How do you lose your "desired" instance?
 
A way to step through understanding this problem:
 - Oh shit, If I have local type classes, I have to handle crazy wacky cases in my implementation, this
   will likely have performance and correctness implications (see Coherency.scala)
 - What happens if I close over constraint on construction? Oops if I close over it, I end up with OverConstrained code (see OverConstrainedCode.scala) and worse I still have coherency issues, and the ability to lose my intended behavious (LosingAnInstance.scala)
 - Oh wow, if I just don't do local type classes, by never define conflicting implicits, and ascribe a single type to each behaviour, everything is simple and just works.


The end of all this, is why worry when there is a _really_ easy and low cost solution. Just wrap up each behaviour, and ascribe it a type. The type maps to a single-globally-coherent behaviour. There are lots of decent tools in scala for doing so (@@ Tags, case class NewTypeWrapper(x: X)). The issue I have with the discussion, is that people think really hard how to hack around these corner cases (closing over instances, not closing over instances, what ever), when it is a complexity easily avoided. Programming is hard enough without tricking ourselves into this rubbish. 

All that said, I am not trying to convince you of anything, just trying to lay out the issues. There are even deeper issues once you get past this trivial ones. In particular scalaz's type class hierarchy is totally broken, and forces you to make a trade off between overlapping instances and over-constrained code, which just isn't good enough.
