package dodd

import java.math.BigInteger
import java.util.regex.Pattern
import scala.io.StdIn

object Main extends App {

  val VALID_REGEX = Pattern.compile("^(0|-?[1-9][0-9]*|[A-Za-z][0-9A-Z_a-z]*)$")
  val NUMBER_REGEX = Pattern.compile("^-?[0-9]+$")

  def isValidString(str: String) = VALID_REGEX.matcher(str).matches()

  def isNumberString(str: String) = NUMBER_REGEX.matcher(str).matches()

  def insertBefore(value: String, oth: Item[String]) = {
    if (isNumberString(value) && isNumberString(oth.value)) {
      new BigInteger(value).compareTo(new BigInteger(oth.value)) < 1
    }
    else {
      value.compareTo(oth.value) < 1
    }
  }

  def valueEquals(item: Item[String], value: String) = item.value == value

  def main(): Unit = {
    var start: Option[Item[String]] = None

    var begin = true

    while (true) {
      if (!begin) {
        println
      }
      else {
        begin = false
      }

      println("Awaiting input...")
      var input = StdIn.readLine

      if (input.isEmpty) {
        println("\nProgram terminated!")
        start = Helpers.removeAll(start)
        return
      }
      else if (input.charAt(0) == '~') {
        if (input.length == 1) {
          println("\nDeleting list...")
          start = Helpers.removeAll(start)
        }
        else {
          input = input.substring(1)
          if (isValidString(input)) {
            println("\nRemoving item...")
            start = Helpers.removeItem(start, input, valueEquals)
          }
          else {
            println("\nCould not parse input!")
          }
        }
      }
      else if (input == "l") {
        println("\nList print...")
        Helpers.printList(start)
      }
      else if (input == "i") {
        println("\nIterator print...")
        Helpers.printIterator(start)
      }
      else if (input == "a") {
        println("\nArray print...")
        Helpers.printArray(start)
      }
      else if (input == "r") {
        println("\nRecursive print...")
        Helpers.printRecursive(start)
      }
      else if (input == "f") {
        println("\nFold print...")
        Helpers.printFold(start)
      }
      else if (input == "b") {
        println("\nFoldback print...")
        Helpers.printFoldback(start)
      }
      else if (isValidString(input)) {
        println("\nInserting item...")
        start = Helpers.insertItem(start, input, insertBefore)
      }
      else {
        println("\nCould not parse input!")
      }
    }
  }

  main()
}
