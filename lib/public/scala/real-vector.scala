case class RealVector(val components:List[Double]) {
    def +(that:RealVector):RealVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot add vectors of different dimensions.");
        RealVector((this.components zip that.components).map(c => c._1 + c._2));
        //RealVector(List.tabulate(this.components.length) {i => (this.components(i) + that.components(i))});
    }
    def -(that:RealVector):RealVector = {
        if (this.dimension != that.dimension) throw new Exception("Cannot subtract vectors of different dimensions.");
        RealVector((this.components zip that.components).map(c => c._1 - c._2));
    }

    def dot(that:RealVector):Double = {
        (this.components zip that.components).map(c => c._1 * c._2).reduceLeft(_ + _);
    }

    def cross(that:RealVector):RealVector = {
        if (this.dimension != 3 || that.dimension != 3) {
            throw new Exception("Dimensions of both vectors must be 3.");
        }
        else {
            return RealVector(List(this.components(1)*that.components(2)-this.components(2)*that.components(1),
            this.components(2)*that.components(0)-this.components(0)*that.components(2),
            this.components(0)*that.components(1)-this.components(1)*that.components(0)));
        }
    }

    def complexConjugate():RealVector = this;

    def dimension():Integer = this.components.length;

    def magnitude():Double = Math.sqrt(this.dot(this.complexConjugate));

    override def toString:String = {
        return this.components.mkString(" ")
    }
}

var a0:List[Double] = List();
var a1:List[Double] = List(1, 3, 5);
var a2:List[Double] = List(1, 3, 5);
var a3:List[Double] = List(2, 6, 10);
println(s"${(new RealVector(a0)).dimension == 0}");
println(s"${(new RealVector(a1)).dimension == 3}");
println(s"${(new RealVector(a1)) + (new RealVector(a2))} == ${new RealVector(a3)}");
println(s"${(new RealVector(a1)) + (new RealVector(a2)) - (new RealVector(a3))}");

println(s"${(RealVector(a1)) == (RealVector(a2))}");
println(s"${RealVector(a1) + RealVector(a2)} == ${RealVector(a3)}");
println(s"${RealVector(a1) + RealVector(a2) == RealVector(a3)}");
var a4:List[Double] = List(1, 1, 1);
println(s"${RealVector(a4).magnitude} == ${Math.sqrt(3)}");
//println(s"cross product: ${RealVector(a0).cross(RealVector(a1))}");
println(s"cross product: ${RealVector(a4).cross(RealVector(a1))}");