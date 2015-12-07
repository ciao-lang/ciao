package CiaoJava;

/**
 * This class represents the Prolog floats. Internally the Prolog floats are
 * stored as double, to facilitate the conversion in the Java side.
 */
public class PLFloat extends PLTerm {
  /**
   * Value of this Prolog float. Must be the largest representation
   * of a float point number in Java, so can be implemented Prolog floats.
   */
  private Double Value;

  /**
   * Creates a new <code>PLFloat</code> object
   * with the given float value.
   *
   * @param v <code>double</code> value that will contain the new object.
   */
  public PLFloat(double v) {

    Type = PLTerm.FLOAT;
    Value = new Double(v);

  }

  /**
   * String representation of a Prolog float. Uses the representation 
   * of the toString method of the Double Java class.
   *
   * @return The string representation of the Prolog float.
   */
  public String toString() {

    return Value.toString();

  }

  /**
   * Java representation of a Prolog float. Returns the Java 
   * <code>Double</code> object that contains the Prolog float.
   *
   * @param i is the PLInterpreter instance used to translate
   *          Prolog representations of Java objects. Although
   *          this method makes no use of this argument, is
   *          included to keep the declaration of this method
   *          as abstract in class <code>PLTerm</code>.
   *
   * @return an <code>Object</code> instance that contains a 
   *         <code>Double</code> object.
   *
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)Value;
  
  }

  /**
   * Java representation of a Prolog float. Returns the Java 
   * <code>Double</code> object that contains the Prolog float.
   *
   * @return an <code>Object</code> instance that contains a 
   *         <code>Double</code> object.
   *
   */
  public Object javaRepr() {
    return (Object)Value;
  }

  /**
   * Returns the value of this Prolog float as a Java <code>double</code>.
   *
   * @return The value of this Prolog <code>PLFloat</code> as a Java
   *         <code>double</code>.
   */
  public double getValue() {

    return Value.doubleValue();

  }
  
  /**
   * Execution test on Prolog objects. Returns true if the
   * related Prolog term can be evaluated. 
   * This method is included for compatibility with the
   * <code>PLTerm</code> class.
   *
   * @return Always <code>false</code>.
   */
  public boolean isRunnable() {

    return false;

  }

  /**
   * Comparison between Prolog terms. 
   *
   * @param t Prolog term to compare to.
   *
   * @return <code>true</code> if the Prolog term received as 
   *         argument is equal to this <code>PLFloat</code><
   *         <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (Value == ((PLFloat)t).Value))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLFloat</code> object.
   * Creates a new <code>PLFloat</code> object with the
   * value of this object.
   *
   * @return A copy of this <code>PLFloat</code> object.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLFloat(this.getValue()));

  }

  /**
   * Not implemented.
   */
  int numberOfCells() {
    // @@@@@ Debe retornar el numero correcto de celdas del heap.
    return 0;
  }
}



