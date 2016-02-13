package co.uk.hackthetower.exercises

import cats.data.Validated.{Valid, Invalid}
import cats.data.{OneAnd, ValidatedNel}
import co.uk.hackthetower.commands.server.{React, Welcome, Goodbye, ServerCommand}

/**
  * First exercise: Implement method 'parseInput'.
  *
  * This method must validate the input received from the server and return an object indicating
  * if the input was correct or not.
  * As per specification the input, if valid, will contain a single command, and will match one of the
  * 'ServerCommands' defined in the codebase. Incorrect scenarios may include invalid commands,
  * multiple commands, etc.
  *
  * Aims:
  * - Learn to work with Validated and OneAnd instances (NonEmptyList in this case)
  * - Understand the differences between Validated and Xor
  * - Understand the differences between OneAnd and NonEmptyList
  *
  * See:
  * - http://typelevel.org/cats/tut/validated.html
  * - http://typelevel.org/cats/tut/oneand.html
  */
object Ex1ValidateInput {

  implicit class StringTuple2(s: String) {
    def toTuple2 = {
      val a = s.split(":")
      (a(0).toInt, a(1).toInt)
    }
  }

  //TODO: START HERE
  /**
    * This method parses the input of the server and validates it to ensure we got a valid command
    *
    * @param input the input sent by the server. As per specification it will only have 1 command.
    * @return a ValidatedNel[String, ServerCommand], equivalent to Validated[NonEmptyList[String], ServerCommand]
    */

  //"Welcome(name=String,apocalypse=int,round=int,maxslaves=int)"
  val welcome = "Welcome\\((.*)\\)".r
  //"Goodbye(energy=int)"
  val goodbye = "Goodbye\\(energy=(.*)\\)".r
  //"React(generation=int,name=string,time=int,view=string,energy=string,master=int:int,collision=int:int,slaves=int,...)"
  val react = "React\\((.*)\\)".r
  def parseInput(input: String): ValidatedNel[String, ServerCommand] = input match {
    case welcome(s) => {
      val m = toMap(s)
      val w = Welcome(m("name"),m("apocalypse").toInt,m("round").toInt,m("maxslaves").toInt)
      Valid(w)
    }
    case goodbye(n) => {
      Valid( Goodbye(n.toInt))
    }
    case react(s) =>{
      val m = toMap(s)
      val r = React(
        m("generation").toInt,
        m("name"),
        m("time").toInt,
        m("view"),
        m("energy"),
        m("master").toTuple2,
        m("collision").toTuple2,
        m("slaves").toInt,
        Map()
      )
      Valid(r)
    }

    case _ => Invalid(OneAnd("Invalid", List()))
  }

  def toMap(arg:String)= arg.split(",").map(_.split("=")).map(x => (x(0),x(1))).toMap

}
