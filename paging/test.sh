#!/bin/bash

function die {
  exit 1
}

javac -d . *.java || die
scalac *.scala || die

#javaOptions="-verbose:gc -XX:+PrintGCDetails"

echo PagingJava
java $javaOptions steshaw.PagingJava > incorrect || die

echo PagingJavaWithBugFixed
java $javaOptions steshaw.PagingJavaWithBugFixed >java.out || die
diff java.out expected

scala="java -cp $SCALA_HOME/lib/scala-library.jar:. $javaOptions"

echo scala Paging
$scala steshaw.Paging >paging.out || die
diff paging.out expected
echo scala Paging2
$scala steshaw.Paging2 >paging2.out || die
diff paging2.out expected
echo scala Paging3
$scala steshaw.Paging3 >paging3.out || die
diff paging3.out expected
