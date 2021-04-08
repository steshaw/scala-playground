// 
// See http://gbracha.blogspot.com/2009/06/ban-on-imports.html
//
// Following code posted by Miles Sabin.
//

object BanOnImports {
  trait Dock {
    def name : String
    def player : Player
  }

  trait Player {
    def name : String
    def dock : Dock

    def play = println("Player "+name+" playing in "+dock.name)
  }

  class SoundSystem(p : => Player) extends Dock {
    override val name = "Basic Dock"
    override def player = p
  }

  class IPod(d : => Dock) extends Player {
    override val name = "IPod"
    override def dock = d
  }

  class Zune(d : => Dock) extends Player {
    override val name = "Zune"
    override def dock = d
  }

  object Configuration1 {
    object dock extends SoundSystem(player)
    object player extends IPod(dock)
  }

  object Configuration2 {
    object dock extends SoundSystem(player)
    object player extends Zune(dock)
  }

  def main(args : Array[String]) {
    Configuration1.player.play
    Configuration2.player.play
  }
}
