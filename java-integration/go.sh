javac -d classes steshaw/*.java &&
  scalac -cp classes -d classes *.scala &&
  scala -cp classes Main
