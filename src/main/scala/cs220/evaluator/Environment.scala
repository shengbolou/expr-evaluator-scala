package cs220.evaluator

/**
 * A Binding represents a binding of a variable to a value.
 * @param v the variable
 * @param a the value
 */
case class Binding(v: Var, a: Value) {
  override def toString: String = v + " -> " + a
}

/**
 * An Environment holds bindings from variables to values.
 */
abstract class Environment {
  /**
   * lookup returns `Some(b)` for some binding `b` if the
   * variable `v` is found in the environment; None otherwise.
   * @param v the variable to lookup
   * @return `Some(b)` if variable is found; `None` otherwise
   */
  def lookup(v: Var): Option[Binding]

  /**
   * Extends the environment with a new binding v -> a.
   * @param v the variable
   * @param a the value
   * @return the new [[Environment]]
   */
  def extend(v: Var, a: Value): Environment

  /**
   * Returns the list of bindings.
   * @return the list of bindings
   */
  def toList: List[Binding]
}

/**
 * This is a class representing the *initial* environment. The
 * initial environment is empty.
 */
abstract class InitialEnvironment extends Environment {
  // TODO: Part 4 - implement the initial environment
  def lookup(v: Var): Option[Binding] = None
  def extend(v: Var, a: Value): Environment =
    new ExtendedEnvironment(Nil,Environment).extend(v, a)
  def toList: List[Binding] = List()
  override def toString: String = "{}"
}

/**
 * An extended environment is an environment that is created
 * with a list of bindings and a previous environment.
 * @param bindings the list of bindings
 * @param prev the previous environment
 */
private class ExtendedEnvironment(val bindings: List[Binding],
                                  val prev: Environment)
                                  extends Environment {
  // TODO: Part 4 - implement the extended environment
  def extend(v: Var, a: Value): Environment = {
      // new list of bindings that contains the new Value
     val newlist = bindings ::: List(Binding(v,a))
     // return the new EntendedEnvironment object
     new ExtendedEnvironment(newlist,this)

  }

  def lookup(v: Var): Option[Binding] = {
   lookupmatching(bindings,v)
  }
  //lookup help method
  def lookupmatching(xs: List[Binding],v: Var):Option[Binding]=
    xs match {
     case Nil                 => None
     case x :: xs if x.v == v => Some(x)
     case x :: xs             => lookupmatching(xs, v)
   }

  def toList: List[Binding] = bindings
  override def toString: String =
    "{" + toList.mkString(", ") + "}"
}

/** The initial empty environment */
object Environment extends InitialEnvironment
