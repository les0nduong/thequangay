class Rational(n: Int, d: Int){
    /*Primary constructor */
    if !(d > 0) then
        throw new IllegalArgumentException("The denominator is not positive.")
    private val g = gcd(n.abs, d.abs) // abs-Method returns the absolute value
    val numerator = n / g // reduced numerator
    val denominator = d / g // reduced denominator
    println("Created" + n.toString + "/" + d.toString)
    /** Returns the gcd of two passed Ints */
    def gcd(a: Int, b: Int): Int = {
        if b == 0 then a else gcd(b, a % b)
    }
    /** Alternative constructor for creating a rational from an interger */
    def this(n: Int) = this(n, 1)

    /** Adds a Rational to this Rational
     * 
     * @param that the Rational to add to this Rational
     * @return a new Rational representing the result of the addition
     */
    def add(that: Rational): Rational = {
        new Rational( // reduces for free
            this.numerator * that.denominator + that.numerator * this.denominator,
            this.denominator * that.denominator
        )
    }

    /** Adds an Int to this Rational
     * 
     * @param that the Int to add to this Rational
     * @return a new Rational representing the result of the addition
     */
    def add(that: Int): Rational = {
        new Rational(numerator + that * denominator, denominator)
    }
    
    /** Returns true if that Rational is less than this Rational
     * 
     * @param that the Rational for which it is tested whether it is less than this Rational
     */
    def lessThan(that: Rational): Boolean = {
        numerator * that.denominator < that.numerator * denominator
    }

    /** Returns the larger Rational of this and that
     * 
     * @param that the Rational to compare with this Rational
     */
    def max(that: Rational): Rational = {
        if this.lessThan(that) then that else this
    }
}