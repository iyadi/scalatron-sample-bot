
/**
  * Do not edit this file without checking Scalatron documentation first
  * or your bot may stop working
  */
class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}

class ControlFunction {
  def respond(input: String) = "Status(text=Hello Hack The Tower)"
}