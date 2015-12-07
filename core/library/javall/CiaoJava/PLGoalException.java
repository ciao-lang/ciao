package CiaoJava;

/**
 * This class implements the exceptions that can throw
 * when a goal is launched to be evaluated by the prolog
 * process.
 */
public class PLGoalException extends PLException {

  /**
   * Creates a new <code/>PLGoalException</code>
   * with no description.
   */
  public PLGoalException() {

    super();

  }

  /**
   * Creates a new <code/>PLGoalException</code>
   * with the description given as argument.
   *
   * @param s Description to include to the goal exception.
   */
  public PLGoalException(String s) {

    super(s);

  }

}
