package CiaoJava;

/**
 * Prolog integer representation.
 * This implementation can only work with Prolog integers
 * that fit in Java <code>Integer</code> type. The <code>Long</code>
 * representation cannot be used due to the importance of integer
 * numbers for almost every class in the Java API.
 */
public class PLInteger extends PLTerm {
  /**
   * Value of the Prolog integer.
   */
  private Integer Value;

  /**
   * Integer constructor. Creates a new <code>PLInteger</code> object
   * with initial value given as argument.
   *
   * @param v Initial value of this <code>PLInteger</code> object.
   *        This argument must be the largest representation of an
   *        integral number to manage Prolog integers.
   */
  public PLInteger(int v) {

    Type = PLTerm.INTEGER;
    Value = new Integer(v);

  }

  /**
   * String representation. Returns the <code>String</code> representation
   * of this <code>PLInteger</code> object.
   *
   * @return The string representation of this <code>PLInteger</code> object.
   */
  public String toString() {

    return Value.toString();

    }

  /**
   * Gets the Java representation of this Prolog integer as an object.
   * The object returned must be a Java <code>Integer</code> object.
   *
   * @param i <code>PLInterpreter</code> object to interpret the
   *          this Prolog term (although is not needed in this
   *          method, this parameter is included here for compatibility with
   *          the abstract declaration in <code>PLTerm</code>).
   *
   * @return  An <code>Object</code> representing the Prolog integer.
   *          This object will be a Java <code>Integer</code> object.
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)Value;

    }

  /**
   * Gets the Java representation of this Prolog integer as an object.
   * The object returned will be a Java <code>Integer</code> object.
   *
   * @return  An <code>Object</code> representing the Prolog integer.
   *          This object will be a Java <code>Integer</code> object.
   */
  public Object javaRepr() {
      return (Object)Value;
  }

  /**
   * Gets the integer value of the <code>PLInteger</code> object.
   *
   * @return The primitive <code>int</code> value of this Prolog integer.
   */
  public int getValue() {

    return Value.intValue();

    }

  /**
   * Execution test on Prolog objects. Returns true if the
   * related Prolog term can be evaluated. Included here
   * for compatibility with the <code>PLTerm</code> class.
   *
   * @return Always <code>false</code>.
   */
  public boolean isRunnable() {

    return false;
  
  }

  /**
   * comparison between Prolog terms.
   *
   * @param t Prolog term to compare to.
   *
   * @return <code>true</code> if the <code>PLTerm</code>
   *         received as argument is equal to this <code>PLInteger</code>
   *         object.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (this.getValue() == ((PLInteger)t).getValue()))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLInteger</code> object.
   * Creates a new <code>PLInteger</code> object with the
   * integer value of this object.
   *
   * @return A new <code>PLInteger</code> with the value of this object.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLInteger(this.getValue()));

  }

  /**
   * Number of cells used in the internal Prolog representation.
   * This function is only needed for $fast format generation in
   * Prolog $fast_read version 'a'.
   * This will NOT work if the internal Prolog representation of
   * this integer needs a structure (large integer representation).
   */
  int numberOfCells() {
    return 0;
  }
}





