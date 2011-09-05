class Equals {
  static final void println(Object msg) { System.out.println(msg); }

  final boolean eqls(Object that) {
    System.out.println(this);
    return this.equals(that);
  }

  public static void main(String[] args) {
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
