package CiaoJava;

/**
 * Prolog variables representation. This class
 * represents a Prolog variable, to provide
 * a Prolog style of variable instancing.
 * This class uses the characteristic Java
 * representation of objects as references.
 * If the object referenced by a variable
 * changes, the variable content will change
 * too. This allows to provide a Prolog behaviour
 * of Java variables containing PLVariable
 * objects.
 */
public class PLVariable extends PLTerm {
  int VarNumber;
  PLTerm Binding = null;
  private final int SINGLE_REF = -1;
  private static int lastRef = -1;

  /**
   * Creates a new Prolog variable
   * and references it to a free variable number.
   */
  public PLVariable() {

    Type = PLTerm.VARIABLE;
    if (lastRef < 0) {
      VarNumber = 0;
      lastRef = 0;
    } else
      VarNumber = ++lastRef;

  }

  /**
   * Creates a new Prolog variable with a given
   * variable number.
   *
   * @param number variable number.
   */
  public PLVariable(int number) {

    Type = PLTerm.VARIABLE;
    VarNumber = number;

    if (lastRef < number)
      lastRef = number;

  }

  /**
   * free variable test. Returns true if this Prolog variable is
   * unbound.
   *
   * @return <code>true</code> if this Prolog variable is unbound<
   *         <code>false</code> if is bound to anothe Prolog term.
   */
  public boolean isFree() {

    return (Binding == null);

  }

  /**
   * Returns the binding of this Prolog variable. If this variable
   * is free, returns null.
   *
   * @return the current binding of this variable.
   */
  public PLTerm getBinding() {

    return Binding;

  }

  /**
   * Returns the internal variable number.
   *
   * @return The internal variable number.
   */
  int getNumber() {

    return VarNumber;

  }

  /**
   * Variable binding. Binds the Prolog variable represented
   * by this object to a Prolog term. If the variable is
   * already bound, the binding is replaced with this one.
   *
   * @param term Term to bind to.
   */
  public void bind(PLTerm term) {
    
    Binding = term;

  }

  /**
   * Variable unbinding. Uninstantiates this Prolog variable,
   * by setting <code>null</code> to the binding attribute.
   */
  public void unbind() {

    Binding = null;

  }

  /**
   * Returns the string representation of this Prolog variable.
   * If the variable is bound to a Prolog term, this term is
   * also represented enclosed between brackets.
   *
   * @return The string representation of this variable.
   */
  public String toString() {
    String ret;

    if (VarNumber == SINGLE_REF)
      ret = "_";
    else
      ret = "_" + String.valueOf(VarNumber);

    if (!isFree())
      ret = ret + "{" + Binding.toString() + "}";

    return ret;
  }

  /**
   * Java representation of a variable: just itself. 
   */
  public Object javaRepr(PLInterpreter i) {

    return (Object)this;

  }

  /**
   * Execution test on Prolog objects. This method
   * is included for implementing the <code>PLTerm</code>
   * abstract method.
   *
   * @return Always <code>false</code>.
   */
  public boolean isRunnable() {
  	return false;
  }

  /**
   * Strict comparison between Prolog terms. 
   *
   * @return <code>true</code> if the <code>PLTerm</code> object
   *         is exactly a <code>PLVariable</code> object and
   *         has the same variable number<
   *         <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (VarNumber == ((PLVariable)t).VarNumber))
      return true;
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLVariable</code> Prolog variable
   * object. Recursively clones the term bound by this variable, in order
   * to create a new <code>PLVariable</code> with no shared components.
   *
   * @return A <code>PLTerm</code> object with a copy of this variable
   *         and its contents.
   */
  public PLTerm copy() {

    PLVariable v = new PLVariable();
    if (!this.isFree()) {
      PLTerm content = this.Binding.copy();
      v.bind(content);
    }
    return (PLTerm)v;

  }

  /**
   * Term unification. Unifies this Prolog variable with the term
   * received as argument. If this is a free variable, binds it
   * to the term received as argument. If not, tries to unify
   * the binding with the term received.
   *
   * @param term Term to unify with.
   *
   * @return <code>true</code> if the unification is successful: false otherwise.
   */
  public boolean unify(PLTerm term) {

    if (Binding == null) {
      Binding = term;
      return true;
    }
    else
      return Binding.unify(term);

  }

  /**
   * Undo the unification made on this variable using as pattern
   * the term received as argument.
   *
   * @param term <code>PLTerm</code> object that contains the
   *             <code>PLVariable</code> object with the content
   *             to be stored on this variable.
   *
   */
  public void backtrack(PLTerm term) throws PLException {

    if (term.isVariable())
      this.bind(((PLVariable)term).getBinding());
    else
      throw new PLException("Object cannot be backtracked" + this.toString());

  }

  /**
   *
   */
  int numberOfCells() {
    return 0;
  }
}






