package CiaoJava;

import java.io.*;

/**
 * This class Represents a Prolog goal to be evaluated on a given
 * <tt>PLConnection</tt>. This class connects to the Prolog side 
 * of the interface to manage the evaluation of the goals launched
 * from the Java side.
 * Instances of this class should be generated with the <tt>launchGoal</tt>
 * method of the <tt>PLConnection</tt> class.
 */
public class PLGoal {
  static final String SOLUTION = "prolog_solution";

  private PLTerm originalGoal = null; /* Keeps a copy of the original
				       * goal term to rebind the
				       * variables included on every
				       * Prolog solution.
				       */
  private PLTerm actualGoal = null;   /* Stores the actual binding
				       * of the variables contained
				       * in the goal. Is used to
				       * unbind them with the original
				       * binding (see originalGoal).
				       */
  private PLConnection prologSpace = null;
  private PLTerm goalId = null;
  private int status = NOT_LAUNCHED;

  private static final String FAIL = "prolog_fail";
  private static final String IS_RUNNING = "prolog_is_running";
  private static final String LAUNCH_GOAL = "prolog_launch_query";
  private static final String NEXT_SOLUTION = "prolog_next_solution";
  private static final String TERMINATE_QUERY = "prolog_terminate_query";
  private static final String USE_MODULE = "prolog_use_module";
  private static final String EXECUTE = "prolog_execute";

  // Query status.
  private static final int NOT_LAUNCHED = 0;
  private static final int RUNNING = 1;
  private static final int TERMINATED = -1;
  private static final int FINISHED = -2;

  /**
   * Goal constructor. Creates a new goal on an already started Prolog
   * process, using the Prolog term represented with <code>term</code>.
   *
   * @param		term	Prolog term that represents the goal that
   *          			will be evaluated.
   * @exception         <code>PLException</code> if there is no
   *                    connection to Prolog. 
   */
  public PLGoal(PLTerm term) throws PLException {

    prologSpace = PLConnection.getPreviousConnection();
    if (prologSpace == null)
	throw new PLException("There is no started connection to Prolog");
    this.actualGoal = term;
    this.originalGoal = term.copy();

  }


  /**
   * Goal constructor. Creates a new goal on the <code>where</code> Prolog
   * process, using the Prolog term represented with <code>term</code>.
   *
   * @param		where	Prolog process on which the goal must
   *          			be evaluated.
   * @param		term	Prolog term that represents the goal that
   *          			will be evaluated. This term must be a
   *                            <code>PLStructure</code> object or a 
   *                            <code>PLAtom</code> object. 
   */
  public PLGoal(PLConnection where, PLTerm term) {

    prologSpace = where;
    this.actualGoal = term;
    this.originalGoal = term.copy();

  }

    /**
   * Goal constructor. Creates a new goal on a already started Prolog
   * process, using the Prolog term represented with <code>term</code>.
   *
   * @param		term	String containing the representation of a
   *                            well formed Prolog term that represents
   *                            the goal that will be evaluated.
   *          			This term must be a
   *                            <code>PLStructure</code> object or a 
   *                            <code>PLAtom</code> object. 
   *
   * @exception         <code>PLException</code> if there is no
   *                    connection to Prolog. 
   */
  public PLGoal(String term) 
    throws IOException, PLException {

    prologSpace = PLConnection.getPreviousConnection();
    if (prologSpace == null)
	throw new PLException("There is no started connection to Prolog");

    this.actualGoal = parseTerm(term);
    this.originalGoal = this.actualGoal.copy();

  }


  /**
   * Goal constructor. Creates a new goal on the <code>where</code> Prolog
   * process, using the Prolog term represented with <code>termString</code>
   * string. This string must be a well formed Prolog term; otherwise a
   * <code>PLException</code> will be thrown. This method connects to
   * Prolog to parse the string containing the goal.
   *
   * @param		where	Prolog process on which the goal must
   *          			be evaluated.
   * @param		term	String containing the representation of a
   *                            well formed Prolog term that represents
   *                            the goal that will be evaluated.
   *          			This term must be a
   *                            <code>PLStructure</code> object or a 
   *                            <code>PLAtom</code> object. 
   */
  public PLGoal(PLConnection where, String term) 
    throws IOException, PLException {

    prologSpace = where;

    this.actualGoal = parseTerm(term);
    this.originalGoal = this.actualGoal.copy();

  }

  /**
   * Goal query.
   * Evaluates on the <code>PLConnection</code> associated object the
   * goal represented by this object. To obtain the solutions of this
   * goal, the {@link #nextSolution()} method must be called, once for
   * each solution.
   *
   * @exception <code>IOException</code>, <code>PLException</code>
   *            if there is any problem 
   *            communicating with the Prolog process, or in the
   *            Prolog side (e.g., the predicate to be launched
   *            does not exist, or the goal has been launched yet).
   */
  public void query() throws IOException, PLException {

    if (status != NOT_LAUNCHED)
      throw new PLException("This query has been already launched.");

    if (actualGoal.isRunnable()) {

      PLTerm arg[] = {actualGoal};
      PLTerm result = null;
      synchronized (prologSpace) {
	  PLTerm javaId = getJavaId();
	  prologSpace.toPrologJP(javaId,new PLStructure(LAUNCH_GOAL,arg));
	  result = prologSpace.fromPrologJP(javaId);
      }

      if (result.isSolution() && ((PLStructure)result).getArg(0).isException())
	throw PLException.translateException(((PLStructure)result).getArg(0));
      
      if (result.isException())
	throw PLException.translateException(result);

      if (result.isPrologFail())
	throw new PLException("Fail returned creating query");

      if (result.isQueryId()) {
	goalId = ((PLStructure)result).getArg(0);
	status = RUNNING;
      }
      else
	throw new PLException("No Id received at query creation:" + result);
    }
    else
      throw new PLException("Invalid goal: " + actualGoal);
  }

  /**
   * Sends to Prolog process a request for the next query solution.
   * Returns a Prolog term that corresponds to the goal with the
   * Prolog variables unified with the solution. Later use of this
   * Prolog variable objects will refer the unification performed.
   * If there is no more solutions, all the variables of the goal
   * will be set to their original binding before calling this method.
   *
   * This method will wait until the solution requested be available.
   *
   * @return  the term that corresponds to the query, with the
   *          variables unified with the solution.
   * @return  <code>null</code> if there are no more solutions.
   *
   * @exception <code>IOException</code> if there are any error on 
   *            the sockets.
   * @exception <code>PLException</code> if there are any error on 
   *            the Prolog process. If the Prolog goal raises an
   *            exception, it is propagated through the interface, 
   *            and a <code>PLException</code> is raised in the
   *            user Java program.
   */
  public PLTerm nextSolution() throws IOException, PLException {

    PLTerm result = null;
    switch (status) {
    case NOT_LAUNCHED:
	throw new PLException("Query not launched");
    case TERMINATED:
	throw new PLException("Query has been already terminated.");
    case FINISHED:
	throw new PLException("Query is already finished.");
    }

    synchronized(this) {
	prologSpace.toPrologJP(goalId,new PLAtom(NEXT_SOLUTION));
	result = prologSpace.fromPrologJP(goalId);
    }

    if (result.isPrologFail()) {
	result = null;
	status = FINISHED;
    } else if (result.isSolution() && ((PLStructure)result).getArg(0).isException())
	throw PLException.translateException(((PLStructure)result).getArg(0));
    else if (result.isException())
	throw PLException.translateException(result);
    else {
	result = ((PLStructure)result).getArg(0);
	actualGoal.backtrack(originalGoal);
	actualGoal.unify(result);
    }
    return result;
  }

  /**
   * Sends to Prolog process a request for the execution of this
   * goal, and returns immediately. This method does not wait
   * until next solution in the Prolog side.
   *
   *
   * @return  <code>true</code> if the goal was launched successfully
   * @return  <code>false</code> if the goal was not launched due
   *          to any reason.
   *
   * @exception <code>IOException</code> if there are any error on 
   *            the sockets.
   * @exception <code>PLException</code> if there are any error on
   *            the Prolog process. If the Prolog goal raises an
   *            exception, it is propagated through the interface,
   *            and a <code>PLException</code> is raised in the
   *            user Java program.
   */
  public boolean execute() throws IOException, PLException {

    PLTerm result = null;
    switch (status) {
    case NOT_LAUNCHED:
	throw new PLException("Query not launched");
    case TERMINATED:
	throw new PLException("Query has been already terminated.");
    case FINISHED:
	throw new PLException("Query is already finished.");
    }

    synchronized(this) {
	prologSpace.toPrologJP(goalId,new PLAtom(EXECUTE));
	result = prologSpace.fromPrologJP(goalId);
    }

    if (result.isPrologFail())
	return false;
    else
	return true;
  }

  /**
   * Checks if Prolog is still running this query, or there are
   * solutions that have not been requested. This method must
   * be used after <code>nextSolution()</code> returns <code>null</code>
   * to ensure that there are no more solutions to request to
   * this goal.
   *
   * @return  true if this query is still running in the Prolog side,
   *          or is expecting <code>nextSolution()</code> requests.
   * @return  false if the goal has returned all solutions and is not
   *          running.
   *
   * @exception <code>IOException</code> if there are any error on
   *            the sockets.
   * @exception <code>PLException</code> if there are any error on
   *            the Prolog process. If the Prolog goal raises an
   *            exception, it is propagated through the interface,
   *            and a <code>PLException</code> is raised in the
   *            user Java program.
   */
  public boolean isStillRunning () throws PLException, IOException {
    PLTerm result = null;
    switch (status) {
    case NOT_LAUNCHED:
	throw new PLException("Query not launched");
    case TERMINATED:
	return false;
/*	throw new PLException("Query has been already terminated.");*/
    case FINISHED:
	return false;
/*	throw new PLException("Query is already finished.");*/
    }

    synchronized(this) {
	prologSpace.toPrologJP(goalId,new PLAtom(IS_RUNNING));
	result = prologSpace.fromPrologJP(goalId);
    }

    if (result.isPrologSuccess())
	return true;
    else if (result.isPrologFail()) 
	return false;
    else if (result.isSolution() && ((PLStructure)result).getArg(0).isException())
      throw PLException.translateException(((PLStructure)result).getArg(0));
    else if (result.isException())
      throw PLException.translateException(result);

    throw new PLException("Unexpected data returned from Prolog.");
  }

  /**
   * Terminates this Prolog goal execution.
   *
   * @exception IOException if there are any error on the socket 
   *            communication
   * @exception PLException if there are any error on the Prolog
   *            process
   *
   */
  public void terminate() throws IOException, PLException {
    
      switch(status) {
      case NOT_LAUNCHED:
	  throw new PLException("Query not launched");
      case TERMINATED:
	  throw new PLException("Query has been already terminated.");
/*    case FINISHED:*/
/*	  throw new PLException("Query is already finished.");*/
      }

      terminate_();
  }

  /**
   * This method loads a module in the Prolog process. This method
   * brings the possibility of loading Prolog modules dynamically.
   * The module to be loaded must be accesible to the Prolog server.
   *
   * @param module Prolog term that represents the name of the module
   *               to be loaded. Can be used the library(module) format.
   *
   * @exception IOException if there are any I/O error with the sockets 
   * @exception PLException if there are any error in the Prolog process
   */
  public void useModule(PLTerm module) throws IOException, PLException {
    
      PLTerm arg[] = {module};
      PLTerm command = new PLStructure(USE_MODULE, 1, arg);
      PLTerm result;
      synchronized(prologSpace) {
	  PLTerm javaId = getJavaId();
	  prologSpace.toPrologJP(javaId,command);
	  result = prologSpace.fromPrologJP(javaId);
      }

      if (result.isSolution() && ((PLStructure)result).getArg(0).isException())
	throw PLException.translateException(((PLStructure)result).getArg(0));
      else if (result.isException())
	throw PLException.translateException(result);
      else if (result.isPrologFail())
	throw new PLException("Fail returned using a Prolog module");
      else if (!result.isPrologSuccess())
	throw new PLException("No success returned using a Prolog module");
  }

  /**
   * 
   * This method loads a module in the Prolog process. This method
   * brings the possibility of loading Prolog modules dynamically.
   * The module to be loaded must be accesible to the Prolog server.
   *
   * @param module String that contains a Prolog term with the name of
   *               the module to be loaded. Can be used the
   *               library(module) format. The path must be accesible
   *               to the Prolog server.
   *
   * @exception IOException if there are any I/O error with the sockets 
   * @exception PLException if there are any error in the Prolog process
   *
   */
  public void useModule(String module) throws IOException, PLException {
      useModule(parseTerm(module));
  }

  /**
   * This private method implements the common tasks related to
   * goal termination.
   */
  private void terminate_() throws IOException, PLException {

    PLTerm result;

    synchronized(this) {
	prologSpace.toPrologJP(goalId,new PLAtom(TERMINATE_QUERY));
	result = prologSpace.fromPrologJP(goalId);
    }
    status = TERMINATED;

    if (result.isSolution() && ((PLStructure)result).getArg(0).isException())
      throw PLException.translateException(((PLStructure)result).getArg(0));
    else if (result.isException())
      throw PLException.translateException(result);
    else if (!result.isPrologSuccess())
      throw new PLException("Termination has not been accepted by the server");

  }

  /**
   * Destructor. Terminates the Prolog goal and finalizes itself.
   */
  protected void finalize() throws IOException, PLException, Throwable {
    
    if ((status != NOT_LAUNCHED) &&
	(status != TERMINATED) &&
	(status != FINISHED)) {
	terminate_();
    }
    super.finalize();
  }

  /**
   * This method uses the Prolog process to parse a Prolog term received
   * as a string.
   *
   *  @param     termString <code>String</code> object that represents
   *             a well formed Prolog term.
   *  @return    the <code>PLTerm</code> that represents this Prolog term.
   *  @exception <code>IOException</code> if the socket stream has been
   *             broken.
   *  @exception <code>PLException</code> if there is a problem parsing
   *             the term on the Prolog side.
   **/
  private PLTerm parseTerm(String termString) 
    throws IOException, PLException {

    PLVariable v = new PLVariable();
    PLStructure p = new PLStructure("prolog_parse", 
				    new PLTerm[] {new PLString(termString),v});

    PLGoal g = new PLGoal(prologSpace, p);

    g.query();
    PLTerm r = g.nextSolution();
    if (r == null)
      throw new PLException("null returned from Prolog socket");

    g.terminate();
    return v.getBinding();

  }

  /**
   * String representation of a Prolog goal.
   *
   * @return A <code>String</code> object representing
   *         this Prolog goal.
   */
  public String toString() {
    
      return "goal{" + this.actualGoal.toString() + "}";

  }

    /**
     * Gets an unique Id for specific commands to Prolog.
     */
    private PLTerm getJavaId() {
	PLTerm arg[] = {new PLInteger(this.hashCode())};
	return (PLTerm)new PLStructure("$javaId",arg);
    }
    
    /**
     * Gets connection that this goal uses to communicate to 
     * Prolog.
     *
     * @return A <code>PLConnection</code> object representing
     *         current connection to Prolog side.
     */
    public PLConnection getConnection() {
        return this.prologSpace;
    }

    
}



