object ComplexNumber {
  import scala.language.implicitConversions;
  implicit def rn2cn(i:Double):ComplexNumber = ComplexNumber(i, 0);
}

case class ComplexNumber(val real:Double, val imaginary:Double) {

  def +(that:ComplexNumber):ComplexNumber = ComplexNumber(this.real + that.real, this.imaginary + that.imaginary);
  def -(that:ComplexNumber):ComplexNumber = this + that.additiveInverse;
  def *(that:ComplexNumber):ComplexNumber = ComplexNumber(this.real * that.real - this.imaginary * that.imaginary,
      this.real * that.imaginary + this.imaginary * that.real);
  def /(that:ComplexNumber):ComplexNumber = this * that.multiplicativeInverse;

  def +(that:Double):ComplexNumber = ComplexNumber(this.real + that, this.imaginary);
  def -(that:Double):ComplexNumber = ComplexNumber(this.real - that, this.imaginary);
  def *(that:Double):ComplexNumber = ComplexNumber(this.real * that, this.imaginary * that);
  def /(that:Double):ComplexNumber = ComplexNumber(this.real / that, this.imaginary / that);

  override def equals(that:Any):Boolean = {
    that match {
      case c:ComplexNumber => this.real == c.real && this.imaginary == c.imaginary;
      case _:Byte => this.real == that && this.imaginary == 0;
      case _:Short => this.real == that && this.imaginary == 0;
      case _:Int => this.real == that && this.imaginary == 0;
      case _:Long => this.real == that && this.imaginary == 0;
      case _:Float => this.real == that && this.imaginary == 0;
      case _:Double => this.real == that && this.imaginary == 0;
      case _ => false;
    }
  }

  def additiveInverse():ComplexNumber = this * ComplexNumber(-1,0);

  def multiplicativeInverse():ComplexNumber = {
    val denominator = this.real * this.real + this.imaginary * this.imaginary;
    ComplexNumber(this.real / denominator, -this.imaginary / denominator);
  }

  def complexConjugate():ComplexNumber = ComplexNumber(this.real, this.imaginary * -1);

  override def toString:String = {
    val realByItself:String = if (this.real == 0 && this.imaginary != 0) "" else
      if (this.real % 1 == 0) this.real.toInt.toString else this.real.toString;
    val space:String = if (this.imaginary != 0 && this.real != 0) " " else "";
    val operand:String = if (this.imaginary == 0 || this.real == 0 && this.imaginary > 0) "" else
          if (this.imaginary < 0) "-" else "+";
    val imaginaryByItself:String = if (this.imaginary == 0) "" else
          if (Math.abs(this.imaginary) == 1) "i" else
            if (this.imaginary % 1 == 0) s"${Math.abs(this.imaginary.toInt)}i" else s"${Math.abs(this.imaginary)}i";
    return realByItself + space + operand + space + imaginaryByItself;
  }
}

val ComplexAdditiveIdentity = ComplexNumber(0,0);
val ComplexMultiplicativeIdentity = ComplexNumber(1,0);

// Code above is copypasted from complex-number.scala just to make this file be able to run by itself

case class RealVector[N:Numeric](vector:Vector[N])
case class ComplexVector(vector: Vector[ComplexNumber])

import scala.language.implicitConversions
implicit def rv[N:Numeric](vector:Vector[N]) = RealVector(vector)
implicit def cv(vector:Vector[ComplexNumber]) = ComplexVector(vector)

object CoordVector {
  def apply[N:Numeric](components:RealVector[N]):CoordVector = {
    CoordVector(components.vector.map(c => ComplexNumber(implicitly[Numeric[N]].toDouble(c), 0)))
  }
}

case class CoordVector(val components:ComplexVector) {
    def +(that:CoordVector):CoordVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot add vectors of different dimensions.");
        CoordVector((this.components.vector zip that.components.vector).map(c => c._1 + c._2));
    }
    def -(that:CoordVector):CoordVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot subtract vectors of different dimensions.");
        CoordVector((this.components.vector zip that.components.vector).map(c => c._1 - c._2));
    }

    def dot(that:CoordVector):ComplexNumber = {
        (this.components.vector zip that.components.vector).map(c => c._1 * c._2).reduceLeft(_ + _);
    }

    def cross(that:CoordVector):CoordVector = {
        if (this.dimension != 3 || that.dimension != 3) throw new Exception("Dimensions of both vectors must be 3.");
        return CoordVector(Vector(this.components.vector(1)*that.components.vector(2)-this.components.vector(2)*that.components.vector(1),
        this.components.vector(2)*that.components.vector(0)-this.components.vector(0)*that.components.vector(2),
        this.components.vector(0)*that.components.vector(1)-this.components.vector(1)*that.components.vector(0)));
    }

    def complexConjugate():CoordVector = CoordVector(this.components.vector.map(c => ComplexNumber(c.real, -c.imaginary)));

    def dimension():Integer = this.components.vector.length;

    def magnitude():Double = Math.sqrt(this.dot(this.complexConjugate).real);

    override def toString:String = {
        return "(" + this.components.vector.mkString(", ") + ")";
    }
}

val a0 = Vector[ComplexNumber]();
val a1 = Vector(ComplexNumber(1,2), ComplexNumber(3,1), ComplexNumber(5,2));
val a2 = Vector(ComplexNumber(1,2), ComplexNumber(3,1), ComplexNumber(5,2));
val a3 = Vector(ComplexNumber(2,4), ComplexNumber(6,2), ComplexNumber(10,4));
println(s"${CoordVector(a0).dimension == 0}");
println(s"${(CoordVector(a1)).dimension == 3}");
println(s"${(new CoordVector(a1)) + (new CoordVector(a2))} == ${new CoordVector(a3)}");
println(s"${(new CoordVector(a1)) + (new CoordVector(a2)) - (new CoordVector(a3))}");

println(s"${(CoordVector(a1)) == (CoordVector(a2))}");
println(s"${CoordVector(a1) + CoordVector(a2)} == ${CoordVector(a3)}");
println(s"${CoordVector(a1) + CoordVector(a2) == CoordVector(a3)}");
val a4 = Vector(1.0, 1.0, 1.0);
println(s"${(CoordVector(a4)) == (CoordVector(a4))}");
println(s"${CoordVector(a4).magnitude} == ${Math.sqrt(3)}");
//println(s"cross product: ${CoordVector(a0).cross(CoordVector(a1))}");
println(s"cross product: ${CoordVector(a4).cross(CoordVector(a1))}");
println(s"cross product: ${CoordVector(a4).cross(CoordVector(a4))}");


// val a0= Vector[Double]();
// val a1= Vector(1.0, 3.0, 5.0);
// val a2= Vector(1, 3, 5);
// val a3= Vector(2, 6, 10);
// println(s"${CoordVector(a0).dimension == 0}");
// println(s"${CoordVector(a1).dimension == 3}");
// println(s"${(CoordVector(a1)) + CoordVector(a2)} == ${CoordVector(a3)}");
// println(s"${(CoordVector(a1)) + CoordVector(a2) - CoordVector(a3)}");

// println(s"${(CoordVector(a1)) == (CoordVector(a2))}");
// println(s"${CoordVector(a1) + CoordVector(a2)} == ${CoordVector(a3)}");
// println(s"${CoordVector(a1) + CoordVector(a2) == CoordVector(a3)}");
// val a4:Vector[Double] = Vector(1, 1, 1);
// println(s"${CoordVector(a4).magnitude} == ${Math.sqrt(3)}");
// //println(s"cross product: ${CoordVector(a0).cross(CoordVector(a1))}");
// println(s"cross product: ${CoordVector(a4).cross(CoordVector(a1))}");