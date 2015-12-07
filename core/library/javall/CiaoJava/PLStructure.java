package CiaoJava;

/**
 * This class is used to represent in Java the
 * Prolog compound terms. This is the basic
 * construct to manage requests, answers and
 * goals in the Java side of this interface.
 */
public class PLStructure extends PLTerm {
  String Name;
  int Arity;
  PLTerm Args[];

  /**
   * Creates a new <code>PLStructure</code> object
   * with the functor, arity and arguments received as parameters.
   *
   * @param name  Functor name.
   * @param arity Structure arity.
   * @param arg   Array of Prolog arguments.
   *
   */
  public PLStructure(String name, int arity, PLTerm arg[]) {

    Type = PLTerm.STRUCTURE;
    Name = name;
    Arity = arity;
    Args = new PLTerm[arity];
    for (int i = 0; i < arity; i++)
      Args[i] = arg[i];

  }

  /**
   * Creates a new <code>PLStructure</code> object.
   * Creates the Prolog structure using the functor and
   * argument list received as parameters.
   *
   * @param name  Functor name.
   * @param arg   Array of Prolog arguments.
   *
   */
  public PLStructure(String name, PLTerm arg[]) {

    Type = PLTerm.STRUCTURE;
    Name = name;
    Arity = arg.length;
    Args = new PLTerm[Arity];
    for (int i = 0; i < Arity; i++)
      Args[i] = arg[i];

  }

  /**
   * String representation of a Prolog structure.
   *
   * @return a Java string that represents the contents of this
   *         Prolog structure.
   */
  public String toString() {

    String s = Name + "(";

    for (int i = 0; i < Arity-1; i++)
      s += Args[i].toString() + ", ";
    return s + Args[Arity-1].toString() + ")";

  }

  /**
   * Returns the functor name of this Prolog structure.
   *
   * @return the string that contains the functor of this
   *         Prolog structure.
   */
  public String getFunctor() {

    return Name;

  }

  /**
   * Returns the arity of this Prolog structure.
   *
   * @return the number of elements of this structure.
   */
  public int getArity() {

    return Arity;

  }

  /**
   * Returns an array of Prolog terms containing the arguments
   * of this structure.
   *
   * @return a Java array of <code>PLTerm</code> objects that
   *         contains the arguments of this structure.
   */
  public PLTerm[] getArgs() {

    return Args;

  }

  /**
   * Returns the argument number <code>argNumber</code>, received
   * as argument.
   *
   * @param argNumber Position of the argument to be returned.
   *
   * @return the Prolog term included in the position <code>argNumber</code>
   *         of the argument list.
   */
  public PLTerm getArg(int argNumber) {

    if (argNumber < Args.length)
      return Args[argNumber];
    else
      return null;

  }

  /**
   * Java representation of a structure. If this structure
   * refers to a Java object in the object table of the
   * interpreter received as argument, then this Java object
   * is returned. Otherwise, this <code>PLStructure</code>
   * object is returned itself.
   *
   * @param i <code>PLInterpreter</code> object used to
   *          build the Java representation.
   *
   * @return a Java object with the Java representation
   *         of this Prolog structure.
   */
  public Object javaRepr(PLInterpreter i) {

    if (Name.equals(JAVA_OBJECT) && Arity == 1)
      return i.getObject((Integer)Args[0].javaRepr(i));
    else if (PLInterpreter.isInterpretable(this))
      return (i.interpret(this)).javaRepr(i);
    //    else if (isJavaType())
    //      return getJavaObject();
    else
      return (Object)this;
  }

  /**
   * Execution test on Prolog objects. Returns true if the
   * related Prolog term can be evaluated.
   *
   * @return Always <code>true</code> (every Prolog structure
   *         can be used to represent a Prolog goal).
   */
  public boolean isRunnable() {
  	return true;
  }

  /** 
   * comparison between Prolog terms. 
   *
   * @param t Prolog term to be compared to.
   *
   * @return <code>true</code> if this structure is equal
   *         to the Prolog term received as argument;
   *         <code>false</code> otherwise.
   */
  public boolean equals(PLTerm t) {

    if ((Type == t.Type) && (Arity == ((PLStructure)t).Arity)) {
      for (int i = 0; i < Arity; i++)
        if (!Args[i].equals(((PLStructure)t).Args[i]))
          return false;
      return true;
    }
    else
      return false;
  }

  /**
   * Makes a full copy of this <code>PLStructure</code> Prolog structure
   * object. Recursively clones the arguments of this term.
   *
   * @return a <code>PLTerm</code> object that contains a full copy 
   *         of this Prolog structure; that is, no argument is shared
   *         between this object and the Prolog term returned.
   *
   */
  public PLTerm copy() {

    PLTerm argCopy[] = new PLTerm[this.Arity];
    for (int i = 0; i < this.Arity; i++)
      argCopy[i] = this.Args[i].copy();

    PLStructure s = new PLStructure(this.Name, this.Arity, argCopy);
    return (PLTerm)s;

  }
    
  /**
   * Term unification. Unifies this Prolog structure with the term
   * received as argument. This method overrides the one 
   * inherited from PLTerm.
   * 
   * <p><bold>Important:</bold> The unification is 'two sided':
   * the variables found in the term received as argument could
   * be bound in order to unify the complete terms. In the same
   * way, the variables found in this structure could be bound to
   * unify both terms.</p>
   *
   * @param term Term to unify with.
   * @return true if the unification is successful: false otherwise.
   */
  public boolean unify(PLTerm term) {

    if (term.isVariable()) {
      if (((PLVariable)term).isFree()) {
	((PLVariable)term).bind(this);
	return true;
      }
      else
	return this.unify(((PLVariable)term).getBinding());
    }
    else
      if ((Type == term.Type) && (Arity == ((PLStructure)term).Arity)) {
	PLStructure s = (PLStructure)term;
      
	for (int i = 0; i < Arity; i++)
	  if (!Args[i].unify(s.Args[i]))
	    return false;
	return true;
      }

    return false;
  }

  /* Undo the unification made on this Structure using as pattern
   * the term received as argument.
   *
   * @param term Prolog term used as pattern for the
   *             backtracking.
   */
  void backtrack(PLTerm term) throws PLException {

    if ((Type == term.Type) && (Arity == ((PLStructure)term).Arity)) {
      PLStructure s = (PLStructure)term;
      
      for (int i = 0; i < Arity; i++)
	this.Args[i].backtrack(s.Args[i]);
    }
    else
      throw new PLException("Object cannot be backtracked" + this.toString());

  }

  int numberOfCells() {
    int num = 2 * (Arity + 1);

    for (int i = 0; i < Arity; i++)
      num += Args[i].numberOfCells();
       
    return num;
  }

  /**
   * Goal launching. Evaluates this structure as a goal and sends
   * it to the eventStream to be launched by Prolog. Before launching, 
   * calculates the values of arguments with the interpreter given, if they
   * must be constructed by Java.
   *
   * @param interpreter <code>PLInterpreter</code> object used to interpret
   *                    the arguments of this structure before launching
   *                    to the Prolog process.
   * @param pl          <code>PLConnection</code> object that represents
   *                    the connection to the Prolog process.
   **/
  void launchGoal(PLInterpreter interpreter,
                         PLConnection pl) {

    PLTerm args[] = new PLTerm[Arity];
    for (int i = 0; i < Arity; i++)
	if (PLInterpreter.isInterpretable(Args[i]))
	    args[i] = interpreter.interpret(Args[i]);
	else
	    args[i] = Args[i];

    PLStructure strGoal = new PLStructure(Name, Arity, args);

    try {
	PLGoal goal = new PLGoal(pl, strGoal);
	goal.query();
	goal.execute();
	//	goal.nextSolution();
	//	goal.terminate();
    } catch (Exception e) {
	// Not implemented. Must throw the exception on the
	// Prolog side.
	System.err.println("ERROR: Exception thrown while launching goal:" + e);
    }
  }
}






