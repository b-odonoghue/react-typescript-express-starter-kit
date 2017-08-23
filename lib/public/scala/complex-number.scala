//import Numeric._

class ComplexNumber(var real:Double, var imaginary:Double) {
  def +(that:ComplexNumber):ComplexNumber = new ComplexNumber(this.real + that.real, this.imaginary + that.imaginary);
  def -(that:ComplexNumber):ComplexNumber = new ComplexNumber(this.real - that.real, this.imaginary - that.imaginary); // additiveInverse
  def *(that:ComplexNumber):ComplexNumber = new ComplexNumber(this.real * that.real - this.imaginary * that.imaginary,
      this.real * that.imaginary + this.imaginary * that.real);
  def /(that:ComplexNumber):ComplexNumber = this * that.multiplicativeInverse;

  def +(that:Double):ComplexNumber = new ComplexNumber(this.real + that, this.imaginary);
  def -(that:Double):ComplexNumber = new ComplexNumber(this.real - that, this.imaginary); // additiveInverse
  def *(that:Double):ComplexNumber = new ComplexNumber(this.real * that, this.imaginary * that);
  def /(that:Double):ComplexNumber = new ComplexNumber(this.real / that, this.imaginary / that); // multiplicativeInverse

  def additiveInverse():ComplexNumber = this * (new ComplexNumber(-1,0));

  def multiplicativeInverse():ComplexNumber = {
    val denominator = this.real * this.real + this.imaginary * this.imaginary;
    new ComplexNumber(this.real / denominator, -this.imaginary / denominator);
  }

  def complexConjugate():ComplexNumber = new ComplexNumber(this.real, this.imaginary * -1);

  // override def equals(that:Any):Boolean = {
  //   that match {                                 
  //     case _:ComplexNumber => this.real == that.real && this.imaginary == that.imaginary; // WHY DOESNT THIS COMPILE GRR
  //     case _:Byte => this.real == that && this.imaginary == 0;
  //     case _:Short => this.real == that && this.imaginary == 0;
  //     case _:Int => this.real == that && this.imaginary == 0;
  //     case _:Long => this.real == that && this.imaginary == 0;
  //     case _:Float => this.real == that && this.imaginary == 0;
  //     case _:Double => this.real == that && this.imaginary == 0;
  //     case _ => false;
  //   }
  // }

  // need to fix this function. scala prints out null terminating characters as a space to the console
  override def toString:String = {
    val realByItself:String = if (this.real == 0 && this.imaginary != 0) "" else this.real.toString;
    val space:Char = if (this.imaginary != 0 && this.real != 0) ' ' else 0;
    val operand:Char = if (this.imaginary == 0 || this.real == 0 && this.imaginary > 0) 0 else
          if (this.imaginary < 0) '-' else '+';
    val imaginaryByItself:String = if (this.imaginary == 0) "" else
          if (Math.abs(this.imaginary) == 1) "i" else s"${Math.abs(this.imaginary)}i";
    s"${realByItself}${space}${operand}${space}${imaginaryByItself}";
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