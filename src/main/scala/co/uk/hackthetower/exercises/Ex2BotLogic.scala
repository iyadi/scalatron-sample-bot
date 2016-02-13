package co.uk.hackthetower.exercises

import cats.data.Xor
import co.uk.hackthetower.commands.bot._
import co.uk.hackthetower.commands.server.{React, Goodbye, Welcome, ServerCommand}

/**
  * Second exercise: Implement method 'processServerCommand'
  *
  * This method receives an Xor[String, ServerCommand] instance and will return an Xor answer.
  *
  * Input:
  * - we receive a Left of String if the input is invalid
  * - we receive a Right of ServerCommand if we got the right command
  *
  * Note that receiving a Left doesn't mean we can't still send BotCommands to the server with instructions.
  *
  * Output:
  * - if the processing fails, we will send a Left with an error message.
  * This will be automatically converted to Say and Log commands for the server.
  * - if the processing succeeds, we will send a Right with a list of BotCommands to send to the server
  *
  * Aims:
  * - Learn to work with Xor to propagate error states
  * - Learn to use both right/left sides as well as mapping over them
  *
  * See:
  * - http://typelevel.org/cats/tut/xor.html
  */
object Ex2BotLogic {

  def processServerCommand(command: Xor[String, ServerCommand]): Xor[String, List[BotCommands]] = {
    println(s"Received command ${command}")
    command.fold(
      x => {
        println(s"Received invalid input $x")
        Xor.left("try again")
      },
      serverCommand => serverCommand match {
        case w:Welcome => Xor.right(List(Say("hi")))
        case g:Goodbye => Xor.left("Bye")
        case r:React   => Xor.right(List(Move(5,5)))
        case  _        => Xor.right(List(Say(s"Do understand command ${command}")))
      }
    )

  }
}
