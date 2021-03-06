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
      case r:java.lang.Number => this.real == that && this.imaginary == 0;
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



// Some really bad tests
println(s"${ComplexNumber(5,-2)} == 5 - 2i");
println(s"${ComplexNumber(5,-1)} == 5 - i");
println(s"${ComplexNumber(0,1)} == i");
println(s"${ComplexNumber(0,-1)} == -i");
println(s"${ComplexNumber(5,2)} == 5 + 2i");
println(s"${ComplexNumber(0,2)} == 2i");
println(s"${ComplexNumber(0,0)} == 0");
println(s"${ComplexNumber(2,0)} == 2");
println(s"${ComplexNumber(0,-7)} == -7i");

println(s"${ComplexNumber(5,2) + 2} == 7 + 2i");
println(s"${ComplexNumber(0,2) / 2} == i");
println(s"${ComplexNumber(0,0) - 1} == -1");
println(s"${ComplexNumber(2,0) * 3} == 6");
println(s"${3 * ComplexNumber(2,0)} == 6");

val c1 = ComplexNumber(5,-2);
val c2 = ComplexNumber(-7,5);

println(s"${c1 + c2} == ${ComplexNumber(-2,3)}");
println(s"${c1 + c1.additiveInverse} == ${ComplexAdditiveIdentity}");
println(s"${c1 * c1.multiplicativeInverse} == ${ComplexMultiplicativeIdentity}");
println(s"${c1 * (ComplexNumber(5,2))} == 29");
println(s"${c1 - (ComplexNumber(2,4))} == ${ComplexNumber(3, -6)}");
println(s"${c1 / (ComplexNumber(2,2))} == ${ComplexNumber(.75, -1.75)}");

println(c1 + c2 == ComplexNumber(-2,3));
println(c1 + c1.additiveInverse == ComplexAdditiveIdentity);
println(c1 * c1.multiplicativeInverse == ComplexMultiplicativeIdentity);
println(s"${c1 * (ComplexNumber(5,2))} == ${29}");
println(s"${c1 * (ComplexNumber(5,2)) == 29} should be true");
println(s"${c1 * (ComplexNumber(5,2)) == 28} should be false");
println(c1 - (ComplexNumber(2,4)) == ComplexNumber(3, -6));
println(c1 / (ComplexNumber(2,2)) == ComplexNumber(.75, -1.75));