//import Numeric._

class ComplexNumber(val real:Double, val imaginary:Double) {
  def +(that:ComplexNumber):ComplexNumber = new ComplexNumber(this.real + that.real, this.imaginary + that.imaginary);
  def -(that:ComplexNumber):ComplexNumber = this + that.additiveInverse;
  def *(that:ComplexNumber):ComplexNumber = new ComplexNumber(this.real * that.real - this.imaginary * that.imaginary,
      this.real * that.imaginary + this.imaginary * that.real);
  def /(that:ComplexNumber):ComplexNumber = this * that.multiplicativeInverse;

  def +(that:Double):ComplexNumber = new ComplexNumber(this.real + that, this.imaginary);
  def -(that:Double):ComplexNumber = new ComplexNumber(this.real - that, this.imaginary);
  def *(that:Double):ComplexNumber = new ComplexNumber(this.real * that, this.imaginary * that);
  def /(that:Double):ComplexNumber = new ComplexNumber(this.real / that, this.imaginary / that);

  override def equals(that:Any):Boolean = {
    case class ComplexNumber(real:Double, imaginary:Double);
    that match {
      case ComplexNumber(real, imaginary) => this.real == real && this.imaginary == imaginary;
      case Byte => this.real == that && this.imaginary == 0;
      case Short => this.real == that && this.imaginary == 0;
      case Int => this.real == that && this.imaginary == 0;
      case Long => this.real == that && this.imaginary == 0;
      case Float => this.real == that && this.imaginary == 0;
      case Double => this.real == that && this.imaginary == 0;
      case _ => false;
    }
  }

  def additiveInverse():ComplexNumber = this * (new ComplexNumber(-1,0));

  def multiplicativeInverse():ComplexNumber = {
    val denominator = this.real * this.real + this.imaginary * this.imaginary;
    new ComplexNumber(this.real / denominator, -this.imaginary / denominator);
  }

  def complexConjugate():ComplexNumber = new ComplexNumber(this.real, this.imaginary * -1);

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

val ComplexAdditiveIdentity = new ComplexNumber(0,0);
val ComplexMultiplicativeIdentity = new ComplexNumber(1,0);



// Some really bad tests
println(s"${new ComplexNumber(5,-2)} == 5 - 2i");
println(s"${new ComplexNumber(5,-1)} == 5 - i");
println(s"${new ComplexNumber(0,1)} == i");
println(s"${new ComplexNumber(0,-1)} == -i");
println(s"${new ComplexNumber(5,2)} == 5 + 2i");
println(s"${new ComplexNumber(0,2)} == 2i");
println(s"${new ComplexNumber(0,0)} == 0");
println(s"${new ComplexNumber(2,0)} == 2");
println(s"${new ComplexNumber(0,-7)} == -7i");

println(s"${new ComplexNumber(5,2) + 2} == 7 + 2i");
println(s"${new ComplexNumber(0,2) / 2} == i");
println(s"${new ComplexNumber(0,0) - 1} == -1");
println(s"${new ComplexNumber(2,0) * 3} == 6");

val c1 = new ComplexNumber(5,-2);
val c2 = new ComplexNumber(-7,5);

println(s"${c1 + c2} == ${new ComplexNumber(-2,3)}");
println(s"${c1 + c1.additiveInverse} == ${ComplexAdditiveIdentity}");
println(s"${c1 * c1.multiplicativeInverse} == ${ComplexMultiplicativeIdentity}");
println(s"${c1 * (new ComplexNumber(5,2))} == 29");
println(s"${c1 - (new ComplexNumber(2,4))} == ${new ComplexNumber(3, -6)}");
println(s"${c1 / (new ComplexNumber(2,2))} == ${new ComplexNumber(.75, -1.75)}");

println(c1 + c2 == new ComplexNumber(-2,3));
println(c1 + c1.additiveInverse == ComplexAdditiveIdentity);
println(c1 * c1.multiplicativeInverse == ComplexMultiplicativeIdentity);
println(c1 * (new ComplexNumber(5,2)) == 29);
println(s"${c1 * (new ComplexNumber(5,2)) == 28} should be false");
println(c1 - (new ComplexNumber(2,4)) == new ComplexNumber(3, -6));
println(c1 / (new ComplexNumber(2,2)) == new ComplexNumber(.75, -1.75));