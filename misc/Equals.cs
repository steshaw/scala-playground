using System;

class Equals {
  static void println(object msg) { Console.WriteLine(msg); }

  public bool eqls(object that) {
    if (this == null) println("this = null");
    else println("this = " + this);

    if (this == null) return that == null;
    else return this.Equals(that);
  }

  public static void Main(string[] args) {
    Equals eN = null;
    Equals e1 = new Equals();
    println(eN);
    println(e1);

    println("e1 eqls eN");
    println(e1.eqls(eN));

    println("eN eqls e1");
    println(eN.eqls(e1)); // NullPointerException!
  }
}
