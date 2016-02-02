package cs220.evaluator

/**
 * An EvaluationException represents a problem with an expression program.
 * An EvaluationException is thrown when there is a problem with
 * evaluating an expression program.
 */
class EvaluationException(msg: String) extends RuntimeException(msg)

/**
 * An EvaluationResult represents the result of an evaluation.
 */
case class EvaluationResult(value: Value, env: Environment)

/**
 * An AbstractEvaluator defines the operations that our evaluator
 * will use to evaluation [[Expr]] objects in an [[Environment]].
 */
abstract class AbstractEvaluator {
  /**
   * eval returns an [[EvaluationResult]] given an expression `expr` and
   * an environment `env`. It determines which of the other `eval` methods
   * to invoke based off of the type of [[Expr]].
   */
  def eval(expr: Expr, env: Environment): EvaluationResult

  /**
   * evalNumber evaluates a [[Number]] `num` in an [[Environment]] `env`.
   */
  def evalNumber(num: Number, env: Environment): EvaluationResult

  /**
   * evalVar evaluates a [[Var]] `v` in an [[Environment]] `env`.
   */
  def evalVar(v: Var, env: Environment): EvaluationResult

  /**
   * evalAdd evaluates an add expression in an [[Environment]] `env`.
   */
  def evalAdd(op: Add, env: Environment): EvaluationResult

  /**
   * evalSub evaluates an subtract expression in an [[Environment]] `env`.
   */
  def evalSub(op: Sub, env: Environment): EvaluationResult

  /**
   * evalMul evaluates a multiply expression in an [[Environment]] `env`.
   */
  def evalMul(op: Mul, env: Environment): EvaluationResult

  /**
   * evalDiv evaluates a divide expression in an [[Environment]] `env`.
   */
  def evalDiv(op: Div, env: Environment): EvaluationResult

  /**
   * evalAssign evaluates an assignment expression in an [[Environment]] `env`.
   */
  def evalAssign(op: Assign, env: Environment): EvaluationResult

  /**
   * evalProgram evaluates a program expression in an [[Environment]] `env`.
   */
  def evalProgram(prog: Program, env: Environment): EvaluationResult
}

// TODO: Part 5 - implement a simple evaluator.
class SimpleEvaluator extends AbstractEvaluator {
  def eval(expr: Expr, env: Environment): EvaluationResult = expr match{
        //an expr can be any expressions, so use matching
        case Var(a)       => evalVar(Var(a),env)
        case Number(a)    => evalNumber(Number(a),env)
        case Add(a, b)    => evalAdd(Add(a,b),env)
        case Sub(a, b)    => evalSub(Sub(a,b),env)
        case Mul(a,b)     => evalMul(Mul(a,b),env)
        case Div(a, b)    => evalDiv(Div(a,b),env)
        case Assign(a, b) => evalAssign(Assign(a,b),env)
        case Program(a)   => evalProgram(Program(a),env)
       /**if there is no such expression or wrong expression is entered,
       then throw an EvaluationException */
        case _            => throw new EvaluationException("???")
  }

  def evalNumber(num: Number, env: Environment): EvaluationResult = {

    EvaluationResult(Value(num.value.toDouble),Environment)
  }
  def evalVar(v: Var, env: Environment): EvaluationResult = {
      // if the element is not found, throw an EvaluationException
      if(env.lookup(v) == None) throw new EvaluationException("ELEMENT NOT FOUND")
      EvaluationResult(env.lookup(v).get.a,env)

  }
  /** for evalAdd, evalSub, evalMul and evalDiv methods,
  just get both left and right values
  and return an EvaluationResult object*/
  def evalAdd(op: Add, env: Environment): EvaluationResult = {
    val a = eval(op.left,env).value.i
    val b = eval(op.right,env).value.i
    EvaluationResult(Value(a + b),env)
  }
  def evalSub(op: Sub, env: Environment): EvaluationResult = {
    val a = eval(op.left,env).value.i
    val b = eval(op.right,env).value.i
    EvaluationResult(Value(a - b),env)
  }
  def evalMul(op: Mul, env: Environment): EvaluationResult = {
    val a = eval(op.left,env).value.i
    val b = eval(op.right,env).value.i
    EvaluationResult(Value(a * b),env)
  }
  def evalDiv(op: Div, env: Environment): EvaluationResult = {
    val a = eval(op.left,env).value.i
    val b = eval(op.right,env).value.i
    EvaluationResult(Value(a / b),env)
  }
  def evalAssign(op: Assign, env: Environment): EvaluationResult = {
    //get the right side value
    val a = eval(op.right,env).value
    //assign to the left side Var
    EvaluationResult(a,env.extend(op.left,a))

  }
   def evalProgram(prog: Program, env: Environment): EvaluationResult =
     prog.exprs match{
       // recurse to the last expression then evaluate it
       case y :: ys if ys == Nil   => eval(y,env)
       /** recurse through the list of Exprs,
       and pass the new Environemnt down everytime */
       case y :: ys                => evalProgram(Program(ys),eval(y,env).env)
    }
}

/** A factory object for an evaluator. */
object Evaluator extends SimpleEvaluator
