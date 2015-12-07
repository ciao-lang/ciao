package CiaoJava;

/**
 * Prolog atom representation. This class is used to represent
 * Prolog atoms and empty lists (The empty list [] is represented
 * in Prolog and in the fast_read/fast_write format as an atom).
 * This class does not represent numeric atoms; use instead
 * PLInteger and PLFloat classes.
 */
public class PLAtom extends PLTerm {
  private String Value;

  /**
   * Atom constructor. Creates a new atom object with its name given
   * as argument.
   *
   * @param name Name of the Prolog atom.
   */
  public PLAtom(String name) {

    Type = PLTerm.ATOM;
    Value = name;

  }

  /**
   * String representation of a Prolog atom.
   *
   * @return The <code>String</code> representation of this atom.
   */
  public String toString() {

    return new String(Value);

  }

  /**
   * Gets the Java representation of the atom as an object.
   *
   * @param i <code>PLInterpreter</code> object representing a Prolog
   *          to Java interpreter. Although is not used in this class,
   *          is included to implement the same method of class
   *          <code>PLTerm</code>.
   *
   * @return a Java <code>Object</code> with the name of this Prolog atom.
   */
  public Object javaRepr(PLInterpreter i) {
    
    return (Object)Value;

  }

  /**
   * Gets the Java representation of the atom as an object (a String object).
   *
   * @return a Java <code>Object</code> with the name of this Prolog atom.
   */
  public Object javaRepr() {
    
    return (Object)Value;

  }

  /**
   * Gets the name of this atom as a string. Returns a copy of the
   * string name instead of the name itself.
   * 
   * @return A <code>String</code> with the name of this atom.
   */
  public String getName() {
    
    return new String(Value);

  }

  /**
   * Execution test on Prolog objects. Implements the abstract method
   * of class <code>PLTerm</code>.
   * 
   * @return true if the related Prolog term can be evaluated as a goal;
   *         false otherwise.
   */
  public boolean isRunnable() {

    return true;

  }

  /**
   * comparison between Prolog terms.
   *
   * @param term The <code>PLTerm</code> object to compare with this object.
   *
   * @return <code>true</code> if the term received as argument is similar to this
   *         atom: is a <code>PLAtom</code> object and has the same
   *         atom name;
   *         false otherwise.
   */
  public boolean equals(PLTerm term) {

    if ((Type == term.Type) && Value.equals(((PLAtom)term).Value))
      return true;
    else
      return false;

  }

  /**
   * Makes a full copy of this <code>PLAtom</code> object. Creates 
   * a new <code>PLAtom</code> object with the name equals to this
   * object name.
   *
   * @return The new object created.
   */
  public PLTerm copy() {
    
    return (PLTerm)(new PLAtom(this.getName()));

  }

  /**
   * Returns the number of cells used in the internal Prolog
   * representation. This function is only needed for $fast format generation in
   * Prolog $fast_read version 'a'.
   * 
   * @return The number of cells used in the internal Prolog representation.
   */
  int numberOfCells() {
    return 0;
  }

  /**
   * Launches a Prolog goal with no arguments.
   */
  void launchGoal(PLInterpreter i, PLConnection pl)
      throws PLGoalException {

      try {
	  PLGoal goal = new PLGoal(pl, this);
	  goal.query();
	  goal.execute();
      } catch (Exception e) {
	  // Not implemented. Must throw the exception on the
	  // Prolog side.
	  System.err.println("ERROR: Exception thrown while launching goal:" + e);
      }
  }

}
