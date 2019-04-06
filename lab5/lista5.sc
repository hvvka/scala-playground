// Hanna Grodzicka

/*
  1. A class for the modifiable polymorphic pair
  with two modifiable fields (fst & snd) with getters, setters and toString
 */
class Pair[A, B](var fst: A, var snd: B) {
  override def toString: String = "(" + fst + ", " + snd + ")"
}

val p: Pair[Int, Int] = new Pair(1, 2)
p.toString == "(1, 2)"
p.fst == 1
p.snd == 2
var r: Pair[Int, Int] = new Pair(0, -1)
r.toString == "(0, -1)"
r.fst == 0
r.snd == -1
r.fst = 8
r.snd = 9
r.toString == "(8, 9)"


/* 2. */
class BankAccount(initialBalance: Double) {
  private var balance = initialBalance

  def checkBalance = balance

  def deposit(amount: Double) = {
    balance += amount
    balance
  }

  def withdraw(amount: Double) = {
    balance -= amount
    balance
  }
}

/* 2a. Extends BankAccount. A fee of 1$ is charged for each transaction. */
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double) = super.deposit(amount - 1)

  override def withdraw(amount: Double) = super.withdraw(amount + 1)
}

val checkingAccount = new CheckingAccount(10)
checkingAccount.deposit(15) == 24
checkingAccount.checkBalance == 24
checkingAccount.withdraw(2) == 21
checkingAccount.checkBalance == 21

/*
  2b. Extends BankAccount. An interest rate of 0.02% is added to the account every month.
  Three transactions per month are free of charge, for others the fee is $1.
*/
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private[this] var transactionsCount: Int = 0

  override def deposit(amount: Double) = {
    transactionsCount += 1
    super.deposit(if (transactionsCount > 3) amount - 1 else amount)
  }

  override def withdraw(amount: Double) = {
    transactionsCount += 1
    super.withdraw(if (transactionsCount > 3) amount + 1 else amount)
  }

  def earnMonthlyInterest() = {
    super.deposit(super.checkBalance * 0.0002)
    transactionsCount = 0
  }
}

val savingsAccount = new SavingsAccount(20)
savingsAccount.deposit(1) == 21
savingsAccount.deposit(1) == 22
savingsAccount.withdraw(1) == 21
savingsAccount.deposit(2) == 22
savingsAccount.earnMonthlyInterest()
savingsAccount.deposit(1) == 23.0044



/*
  3a. An abstract class with immutable name field.
  Its constructor allows to create an animal with a default name or given as an argument.
 */
abstract class Zwierz(imie: String = "bez imienia") {
  private[this] val imieZwierza: String = imie

  def rodzaj: String = getClass.getName

  def dajGlos: String

  override def toString: String = rodzaj + " " + imieZwierza + " daje glos " + dajGlos
}

val kotek = new Zwierz {
  override def dajGlos: String = "miau miau"
}

val piesek = new Zwierz("Hektor") {
  override def dajGlos: String = "hau hau"
}

kotek.toString == "$anon$1 bez imienia daje glos miau miau"
piesek.toString == "$anon$1 Hektor daje glos hau hau"


/* 3b. Two animals created by inheritance. */
class Kot(imie: String = "anonimowy") extends Zwierz(imie) {
  override def dajGlos = "prr prr"
}

class Pies(imie: String = "anonimowy") extends Zwierz(imie) {
  override def dajGlos = "arf arf"
}

val kot = new Kot("Filemon")
kot.toString

val pies = new Pies()
pies.toString


/*
  3c. A testing program that creates a collection of several animals and writes out info about them.
  Observe the effect of the inclusion polymorphism and dynamic binding.
 */
object TestZwierza {
  def main(args: Array[String]): Unit = {
    val vector = Vector(new Kot("Alfons"),
                        new Pies(),
                        new Kot("Kulka"),
                        new Pies("Kruczek"))

    vector.foreach(x => println(x.toString))
  }
}

TestZwierza.main(Array())
