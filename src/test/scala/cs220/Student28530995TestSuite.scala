package cs220

import org.scalatest.FunSuite
import cs220.evaluator._
import cs220.parser._

class Student28530995TestSuite extends FunSuite{

  //test NO.1
   test("extend method works correctly"){
     val e =  Environment
     try{
       assert(e.extend(Var("a"), Value(5)).toList ==
              List(Binding(Var("a"),Value(5))))
     }
     catch {
       case _: NotImplementedError => fail("You did not implement this method yet!")
     }

   }


   //test NO.2
 test("lookup method works correctly"){
   val e =  Environment
   try{
     e.extend(Var("a"), Value(5)).lookup(Var("a")) match{
       case None => None
       case Some(Binding(x,y)) => assert(x == Var("a"))
        }
   }
   catch {
     case _: NotImplementedError => fail("You did not implement this method yet!")
   }
 }
 //test NO.3
   test("Add expression works correctly"){
     try{
      assert(Add(Number(12),Number(5)).toString() == "12 + 5")

    }
    catch {
      case _: NotImplementedError => fail("You did not implement this method yet!")
    }
  }
  //test NO.4
    test("Sub expression works correctly"){
     try{
      assert(Sub(Number(12),Number(5)).toString() == "12 - 5")

    }
    catch {
      case _: NotImplementedError => fail("You did not implement this method yet!")
    }
  }
  //test NO.5
    test("Mul expression works correctly"){
     try{
      assert(Mul(Number(12),Number(5)).toString() == "12 * 5")

    }
    catch {
      case _: NotImplementedError => fail("You did not implement this method yet!")
    }
  }
  //test NO.6
    test("Div expression works correctly"){
     try{
      assert(Div(Number(12),Number(5)).toString() == "12 / 5")

    }
    catch {
      case _: NotImplementedError => fail("You did not implement this method yet!")
    }
  }
  //test NO.7
      test("Assign expression works correctly"){
       try{
        assert(Assign(Var("a"),Number(5)).toString() == "a = 5")

      }
      catch {
        case _: NotImplementedError => fail("You did not implement this method yet!")
      }
    }
    //test NO.8
    test("evalVar method exception test"){
     try{
      val e = Environment
      /* there is no Var("a") in the exvironment,
      so an EvaluationException should be thrown */
      intercept[EvaluationException]{
        Evaluator.evalVar(Var("a"),e)
       }
    }
    catch {
      case _: NotImplementedError => fail("You did not implement this method yet!")
    }
  }

  //test NO.9
  test("Evaluation of expressions NO.2 "){
   try{
    val e = Environment
    val a = Assign(Var("a"),Number(5)) // a -> 5
    assert(Evaluator.eval(a,e).value.i == 5)

  }
  catch {
    case _: NotImplementedError => fail("You did not implement this method yet!")
  }
}


//test NO.10
test("Evaluation of expressions NO.3 "){
 try{
  val e = Environment
  val a = Program(
    List(
      Assign(Var("a"), Number(5)),// a -> 5

      Assign(Var("b"), Number(6)),// b -> 6
      // c -> (a+b)*2
      Assign(Var("c"), Mul(Add(Var("a"), Var("b")), Number(2))),
      // c + 1
      Add(Var("c"), Number(1))
      )
  )
      // answer should be 23
      assert(Evaluator.eval(a,e).value.i == 23)
}

catch {
  case _: NotImplementedError => fail("You did not implement this method yet!")
  }
}






}
