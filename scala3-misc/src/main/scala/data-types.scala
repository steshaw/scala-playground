// Examples from scala-lang.org

import java.time.LocalDate as Date
import Payment.* // Strange to import from enum defined later in this file...

val name = "Bib Fortuna"
val digits = 4242_4242_4242_4242L
val expires = Date.of(1983, 5, 25)

enum Payment:
  case Card(name: String, digits: Long, expires: Date)
  case PayPal(email: String)

def process(kind: Payment) = kind match
  case Card(name, digits, expires) =>
    s"Processing credit card $name, $digits, $expires"
  case PayPal(email) =>
    s"Processing PayPal account $email"

@main
def main =
  val a = process(Card(name, digits, expires))
  println(a)
