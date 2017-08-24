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
    new ComplexNumber(this.real / denominator, -this.imaginary / denominator);
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

case class ComplexVector(val components:List[ComplexNumber]) {
    def +(that:ComplexVector):ComplexVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot add vectors of different dimensions.");
        ComplexVector((this.components zip that.components).map(c => c._1 + c._2));
    }
    def -(that:ComplexVector):ComplexVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot subtract vectors of different dimensions.");
        ComplexVector((this.components zip that.components).map(c => c._1 - c._2));
    }

    def dot(that:ComplexVector):Double = {
        (this.components zip that.components).map(c => c._1 * c._2).reduceLeft(_ + _));
    }

    def cross(that:ComplexVector):ComplexVector = {
        if (this.dimension != 3 || that.dimension != 3) throw new Exception("Dimensions of both vectors must be 3.");
        return ComplexVector(List(this.components(1)*that.components(2)-this.components(2)*that.components(1),
        this.components(2)*that.components(0)-this.components(0)*that.components(2),
        this.components(0)*that.components(1)-this.components(1)*that.components(0)));
    }

    def complexConjugate():ComplexVector = ComplexVector(this.components.map(c => ComplexNumber(c.real, -c.imaginary)));

    def dimension():Integer = this.components.length;

    def magnitude():Double = Math.sqrt(this.dot(this.complexConjugate));

    override def toString:String = {
        return "(" + this.components.mkString(", ") + ")";
    }
}

val a0 = List[ComplexNumber]();
val a1 = List(ComplexNumber(1,2), ComplexNumber(3,1), ComplexNumber(5,2));
val a2 = List(ComplexNumber(1,2), ComplexNumber(3,1), ComplexNumber(5,2));
val a3 = List(ComplexNumber(2,4), ComplexNumber(6,2), ComplexNumber(10,4));
println(s"${ComplexVector(a0).dimension == 0}");
println(s"${(ComplexVector(a1)).dimension == 3}");
println(s"${(new ComplexVector(a1)) + (new ComplexVector(a2))} == ${new ComplexVector(a3)}");
println(s"${(new ComplexVector(a1)) + (new ComplexVector(a2)) - (new ComplexVector(a3))}");

println(s"${(ComplexVector(a1)) == (ComplexVector(a2))}");
println(s"${ComplexVector(a1) + ComplexVector(a2)} == ${ComplexVector(a3)}");
println(s"${ComplexVector(a1) + ComplexVector(a2) == ComplexVector(a3)}");
val a4 = List(ComplexNumber(1,0), ComplexNumber(1,0), ComplexNumber(1,0));
println(s"${ComplexVector(a4).magnitude} == ${Math.sqrt(3)}");
//println(s"cross product: ${ComplexVector(a0).cross(ComplexVector(a1))}");
println(s"cross product: ${ComplexVector(a4).cross(ComplexVector(a1))}");