package niceCalculator

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/* A class for the Exception  to throw*/
class CalculatorException extends Exception("Operation Failed")

/* A class to perform the Addition operation  */
class Addition extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  /* A method to perform the addition operation */
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head + operands.last)
  }
}

/* A class to perform the subtract operation */
class Subtraction extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    if (operands.head > operands.last) Seq(operands.head - operands.last)
    else Seq(operands.last - operands.head)
  }
}

/* A class to perform the Multiplication operation */
class Multiplication extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2 && operands.head != 0 && operands.last != 0) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(operands.head * operands.last)
  }
}

/* A class to perform the Division operation */
class Division extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2 && operands.last != 0) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val numerator = operands.head
    val denominator = operands.last
    Seq(numerator / denominator)
  }
}

/* A class to perform the Power operation */
class Power extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2 && operands.head >= 1 && operands.last >= 1) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val base = operands.head
    val exponent = operands.last.toInt

    val repeatedBaseValues = List.fill(exponent)(base.toInt)

    val factorials = repeatedBaseValues.foldLeft(List(1)) { (accumulator, currentBaseValue) =>
      val currentFactorial = currentBaseValue * accumulator.head
      currentFactorial :: accumulator
    }.reverse

    factorials.map(_.toDouble)
  }
}

/* A class to perform the square root of the number */
class SquareRoot extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1 && operands.head >= 1) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(Math.sqrt(operands.head))
  }
}

/* A class to perform the Factorial of the number */
class Factorial extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1 && operands.head >= 1) true
    else false
  }
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    def factorial(number: Double, accumulator: Double): Double = {
      if (number <= 1) accumulator
      else factorial(number - 1, accumulator * number)
    }

    val inputNumber = operands.head
    val factorialOfInputNumber = factorial(inputNumber, 1)
    Seq(factorialOfInputNumber)
  }
}

/* This class is used to perform the sum of list of elements*/
class SumOfElements extends Operator with Validator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val operandsList = operands.toList
    val sum = operandsList.reduceLeft((accumulator: Double, currentValue: Double) => accumulator + currentValue)
    Seq(sum)
  }
}

/* This method is used to perform the GCD Operation */
class GreatestCommonDivisor extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 2 && operands.head > operands.last && operands.head >= 1 && operands.last >= 1) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val firstNumber = operands.head.toInt
    val secondNumber = operands.last.toInt

    val divisorsOfFirstNumber = (1 to firstNumber).filter { divisor => firstNumber % divisor == 0 }.toSet
    val divisorsOfSecondNumber = (1 to secondNumber).filter { divisor => secondNumber % divisor == 0 }.toSet

    val commonDivisors = divisorsOfFirstNumber.intersect(divisorsOfSecondNumber)

    Seq(commonDivisors.max)
  }
}

/* This class is used to find the Odd elements of the Sequence */
class OddElements extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filterNot(value => value % 2 == 0)
  }
}

/* This class is used to find Even Elements Of Sequence */
class EvenElements extends Operator with Validator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }

  override protected def execute(numbers: Seq[Double]): Seq[Double] = {
    numbers.filter(number => number % 2 == 0)
  }
}

/* This class is used to find the list of Fibonacci number */
private class Fibonacci extends Operator {
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.size == 1) true
    else false
  }

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val number = operands.head.toInt
    var fibonacciList = Seq[Double](0, 1)
    (2 until number).map { index =>
      fibonacciList = fibonacciList :+ (fibonacciList(index - 1) + fibonacciList(index - 2))
    }
    fibonacciList
  }
}

/* This is a different class that is used to perform multiple operations */
class MultipleOperator {
  /* This method is used to check the Square of expression is true or not */
  def squareOfExpression(firstOperand: Double, secondOperand: Double): String = {
    val addition = new Addition
    val power = new Power
    val multiplication = new Multiplication
    val operands = Seq(firstOperand, secondOperand)
    val resultOfLhs = power.validateAndExecute(addition.validateAndExecute(operands) ++ Seq(2.0))
    val resultOfRhsHalfPart = addition.validateAndExecute(power.validateAndExecute(Seq(firstOperand, 2)) ++ power.validateAndExecute(Seq(secondOperand, 2)))
    val totalResultOfRhs = addition.validateAndExecute(resultOfRhsHalfPart ++ multiplication.validateAndExecute(multiplication.validateAndExecute(Seq(2, firstOperand)) ++ Seq(secondOperand)))
    if (resultOfLhs == totalResultOfRhs) "Equal"
    else "Not Equal"
  }


  /* This method is used to find the number whose factorial is greater than 6^num */
  def find(numbers: Seq[Double]): Future[Seq[Double]] = {
    @tailrec
    def findFactorial(number: Double, accumulator: Double): Double = {
      if (number <= 1) accumulator
      else findFactorial(number - 1, accumulator * number)
    }

    val filteredNumbers = numbers.filter { num =>
      val factorial = findFactorial(num, 1)
      factorial > math.pow(6, num)
    }

    Future(filteredNumbers)
  }


  /* This method is used to find the average after performing the fibonacci on each number, filter the odd elements */
  def findAverageAfterChainingOperations(numbers: Seq[Double]): Future[Double] = {
    Future {
      def fibonacci(times: Double, numberOne: Double, numberTwo: Double): Double = {
        if (times <= 1) numberTwo
        else fibonacci(times - 1, numberTwo, numberOne + numberTwo)
      }

      val filteredDataNumbers = numbers.filter { num =>
        val res = fibonacci(num.toInt, 0, 1)
        res % 2 != 0
      }
      filteredDataNumbers.foldLeft(0.0)((numOne: Double, numTwo: Double) => numOne + numTwo) / filteredDataNumbers.size
    }
  }
}

object Calculator {
  /* This method is used to match with operator and call the execute method */
  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    operator match {
      case "+" => execute(new Addition, operands)
      case "-" => execute(new Subtraction, operands)
      case "*" => execute(new Multiplication, operands)
      case "/" => execute(new Division, operands)
      case "^" => execute(new Power, operands)
      case "sqrt" => execute(new SquareRoot, operands)
      case "!" => execute(new Factorial, operands)
      case "sum" => execute(new SumOfElements, operands)
      case "gcd" => execute(new GreatestCommonDivisor, operands)
      case "odd" => execute(new OddElements, operands)
      case "even" => execute(new EvenElements, operands)
      case "fibonacci" => execute(new Fibonacci, operands)
      case _ => throw new CalculatorException
    }
  }

  /* This method is used to call the specific class for performing the operation as matched from above method */
  private def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = {
    Future {
      val result = operator.validateAndExecute(operands)
      result
    }.recoverWith {
      case ex => Future.failed(ex)
    }
  }
}