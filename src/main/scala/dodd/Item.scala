package dodd

import scala.annotation.tailrec

class Item[T](val value: T, var next: Option[Item[T]]) extends Iterable[Option[Item[T]]] {

  println(f"Creating item: $value%s")

  def printGetNext: Option[Item[T]] = {
    print(f"$value${if (next.isEmpty) "\n" else ", "}")
    next
  }

  def apply(n: Int): Option[Item[T]] = {
    var item: Option[Item[T]] = Some(Item.this)
    for (_ <- 0 until n) {
      item = item match {
        case Some(x) => x.next
        case None => None
      }
    }
    item
  }

  override def iterator: Iterator[Option[Item[T]]] = new Iterator[Option[Item[T]]]() {
    var item: Option[Item[T]] = Some(Item.this)

    override def hasNext: Boolean = item.isDefined

    override def next: Option[Item[T]] = {
      val next = item
      item = item match {
        case Some(x) => x.next
        case None => None
      }
      next
    }
  }
}

object Item {

  @tailrec
  def fold[T, A, R](fSome: (Item[T], Item[T], A) => A, fLast: (Item[T], A) => R, fEmpty: A => R, accumulator: A, item: Option[Item[T]]): R = {
    item match {
      case Some(x) =>
        val next = x.next
        next match {
          case Some(y) => fold(fSome, fLast, fEmpty, fSome(x, y, accumulator), next)
          case None => fLast(x, accumulator)
        }
      case None => fEmpty(accumulator)
    }
  }

  @tailrec
  def foldback[T, A, R](fSome: (Item[T], Item[T], A) => A, fLast: Item[T] => A, fEmpty: () => A, generator: A => R, item: Option[Item[T]]): R = {
    item match {
      case Some(x) =>
        val next = x.next
        next match {
          case Some(y) => foldback(fSome, fLast, fEmpty, (innerVal: A) => generator(fSome(x, y, innerVal)), next)
          case None => generator(fLast(x))
        }
      case None => generator(fEmpty())
    }
  }
}
