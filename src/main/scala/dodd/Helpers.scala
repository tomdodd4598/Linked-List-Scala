package dodd

import scala.annotation.{tailrec, unused}

object Helpers {

  def insertItem[T](start: Option[Item[T]], value: T, insertBefore: (T, Item[T]) => Boolean)
  : Option[Item[T]] = {
    println(f"Creating item: $value%s")
    var head = start
    var current = head
    var previous: Option[Item[T]] = None

    while (current.isDefined && !insertBefore.apply(value, current.get)) {
      previous = current
      current = current.get.next
    }
    val item = new Item(value, current)

    previous match {
      case Some(x) => x.next = Some(item)
      case None => head = Some(item)
    }

    head
  }

  def removeItem[T](start: Option[Item[T]], value: T, valueEquals: (Item[T], T) => Boolean)
  : Option[Item[T]] = {
    var head = start
    var current = head
    var previous: Option[Item[T]] = None

    while (current.isDefined && !valueEquals.apply(current.get, value)) {
      previous = current
      current = current.get.next
    }

    current match {
      case Some(x) =>
        previous match {
          case Some(y) => y.next = x.next
          case None => head = x.next
        }
        println(f"Removed item: $value%s")
      case None => println(f"Item $value%s does not exist!")
    }

    head
  }

  def removeAll[T](@unused start: Option[Item[T]]): Option[Item[T]] = None

  def printList[T](start: Option[Item[T]]): Unit = {
    var item = start
    while (item.isDefined) {
      item = item.get.printGetNext
    }
  }

  def printIterator[T](start: Option[Item[T]]): Unit = {
    start match {
      case Some(x) => x.foreach(_.foreach(_.printGetNext))
      case None =>
    }
  }

  def printArray[T](start: Option[Item[T]]): Unit = {
    start match {
      case Some(x) =>
        var item = start
        var i = 0
        while (item.isDefined) {
          item = x(i).get.printGetNext
          i += 1
        }
      case None =>
    }
  }

  @tailrec
  def printRecursive[T](start: Option[Item[T]]): Unit = {
    start match {
      case Some(x) => printRecursive(x.printGetNext)
      case None =>
    }
  }

  def printFold[T](start: Option[Item[T]]): Unit = {
    val fSome = (current: Item[T], _: Item[T], accumulator: String) => f"$accumulator%s${current.value}%s, "
    val fLast = (current: Item[T], accumulator: String) => f"$accumulator%s${current.value}%s\n"
    val fEmpty = (accumulator: String) => accumulator
    print(Item.fold(fSome, fLast, fEmpty, "", start))
  }

  def printFoldback[T](start: Option[Item[T]]): Unit = {
    val fSome = (current: Item[T], _: Item[T], innerVal: String) => f"${current.value}%s, $innerVal%s"
    val fLast = (current: Item[T]) => f"${current.value}\n"
    val fEmpty = () => ""
    print(Item.foldback(fSome, fLast, fEmpty, (x: String) => x, start))
  }
}
