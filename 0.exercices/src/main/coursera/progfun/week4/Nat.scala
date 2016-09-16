package coursera.progfun.week4

import java.util.NoSuchElementException

/**
  * Created by jschoreels on 16.09.16.
  */
abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if (that.isZero) Zero else throw new NoSuchElementException

  override def predecessor: Nat = throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = this.predecessor + that.successor

  override def -(that: Nat): Nat = if(that.isZero) this else this.predecessor - that.predecessor

  override def predecessor: Nat = n
}