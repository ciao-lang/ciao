import CiaoJava.*;
import java.awt.*;
import java.awt.event.*;

/**
 * ABSTRACT
 * Java to ciao interface example.
 * This program shows the java to ciao interface capabilities. Using the
 * N-queens problem shows, how to start a ciao process (so called prolog
 * space), and how a prolog goal can be launched, and the iteration used
 * to find all the solutions of a prolog goal.
 *
 * PROGRAM BEHAVIOUR
 * The program starts creating a window with an input box, several buttons
 * and a game board. The board is initially an 8x8 board with the first 
 * 8-queens solution.
 * With the input box can be selected the size of the board, from 4x4 to
 * 9x9. With the Ok button the board size is taken, and the program launches
 * the corresponding prolog query and requests the first solution.
 * Remaining solutions can be showed clicking at the Next Sol. button.
 * 
 * INTERNALS 
 * This program consists of one main class (queens) that extends the Frame 
 * class. This class handles the program window and its elements.
 * To work properly with the elements of the window, uses four inner classes,
 * three of which are used to handle button events, and one class is used to
 * represent one square of the board.
 *
 * The most important methods of the program are the ones regarding the
 * prolog interface tasks. There are three main tasks:
 *  - Prolog process creation: is made in main method at start up.
 *  - Query creation: at start up and when the user clicks on the OK button,
 *    a new query is launched and requested the first solution. This task is
 *    executed by the solveProblem method
 *  - Solutions iteration: when the user clicks on the Next Sol. button, is
 *    requested the following solution of the last query. This task is
 *    carried out by the nextSolution method.
 **/
public class queens extends Frame {

  // Constants
  private static final int INIT_DIM = 8;
  private static final int FRAME_WIDTH = 300;
  private static final int FRAME_HEIGHT = 350;

  // Components to be displayed.
  private Panel pnNorth = new Panel();
  private Panel pnBoard = new Panel();
  private TextField txtDimension = new TextField(Integer.toString(INIT_DIM));
  private Button btnOk = new Button("Ok");
  private Button btnNext = new Button("Next Sol.");
  private Button btnExit = new Button("Exit");

  // Prolog process.
  private static PLConnection plServer = null;

  // Following variables are used to iterate through the
  // solutions of the prolog goal.
  PLGoal currentGoal = null;
  PLVariable currentSolution = null;

  // Following private variables are used to speed up board refresh: when
  // the board must be repainted, if the board dimension has not be changed,
  // displaySolution only has to empty the previous solution and fill the
  // squares of the current solution.
  private char[] prevSolution = {};
  private int prevDim = 0;

  /**
   * Start method. Receives in <code>argv</code> the command line arguments.
   * This program only uses the first argument as the name of the prolog
   * process (in this example is called plserver).
   **/
  public static void main(String argv[]) {

    try {
	if (argv.length == 0)
	    plServer = new PLConnection();
	else
	    plServer = new PLConnection(argv);
    } catch (Exception e) {
      System.err.println("Problems starting java server: " + e);
      System.exit(1);
    }

    queens q = new queens();
    q.setSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
    q.setTitle("N-queens problem");
    q.show();
    
  }

  /**
   * Frame constructor. Creates the components that will be included on
   * the frame and its event handlers, and finds an initial solution to
   * the 8-queens problem.
   **/
  public queens() {

    pnBoard.setLayout(new GridLayout(INIT_DIM,INIT_DIM));

    pnNorth.setLayout(new GridLayout(1,4));
    pnNorth.add(txtDimension);
    pnNorth.add(btnOk);
    pnNorth.add(btnNext);
    pnNorth.add(btnExit);

    this.setLayout(new BorderLayout());
    this.add(pnNorth, BorderLayout.NORTH);
    this.add(pnBoard, BorderLayout.CENTER);

    btnOk.addActionListener(new btnOkAction(this));
    btnNext.addActionListener(new btnNextAction(this));
    btnExit.addActionListener(new btnExitAction());

    displaySolution(solveProblem(INIT_DIM),INIT_DIM);

  }

  /**
   * Creates a new query to solve the n-queens problem using the
   * <code>dim</code> parameter as argument of the new query.
   *
   * @param dim Size of the board.
   * @return the first solution of the n-queens query.
   **/
  private char[] solveProblem(int dim) {

    char res[] = new char[dim];

    try {
      //
      // Java representation of : queens:queens(dim,X)
      // where: dim - is the board dimensions.
      //        X   - is the prolog variable where the results are received.
      //
      currentSolution = new PLVariable();
      PLTerm t = new PLStructure(":", 
				 new PLTerm[] 
				 {new PLAtom("queens"),
				    new PLStructure
				    ("queens", 
				     new PLTerm[] {new PLInteger(dim),
						     currentSolution}
								 )});

      if (currentGoal != null) {
	currentGoal.terminate();
	currentGoal = null;
      }

      currentGoal = new PLGoal(plServer, t);
      currentGoal.useModule("queens");

      currentGoal.query();
      PLTerm r = null;
      do {
	  r = currentGoal.nextSolution();
      } while ((r == null) && currentGoal.isStillRunning());
      if (r != null) {
	res = ((PLString)currentSolution.getBinding()).getValue().toCharArray();
      }
      else {
	System.err.println("Error raised getting solution.");
	System.exit(1);
      }

    } catch (Exception e) {
      System.err.println("Error:"+e);
      System.exit(1);
    }

    return res;

  }

  /**
   * Gets the next solution of the previous n-queens problem.
   * Given the query stored at the currentGoal field, requests the
   * next solution of the problem. If there are no more solutions,
   * prints at standard error stream a message and returns the
   * last solution. If there is no query, prints the corresponding
   * message and returns the last solution requested.
   * If an exception on the prolog side is thrown, the program
   * is terminated with an error message.
   *
   * @return the next solution of the problem, if there is any;
   *         the last solution, if there are no more solutions, or
   *         the query has not been started (using solveProblem).
   */
  private char[] nextSolution() {

    char res[] = new char[prevDim];

    if (currentGoal !=null) {
      try {
	PLTerm r = currentGoal.nextSolution();
	if (r != null) {
	  res = ((PLString)currentSolution.getBinding()).getValue().toCharArray();
	}
	else {
	  System.err.println("There are no more solutions.");
	  currentGoal = null;
	  res = prevSolution;
	}

      } catch (Exception e) {
	System.err.println("Error:"+e);
	System.exit(1);
      }
    }
    else {
      System.err.println("There is no goal to solve.");
      res = prevSolution;
    }

    return res;

  }

  /**
   * Puts on the frame the solution received in parameter <code>solution</code>
   * , and redisplays the window.
   * To speed up the process, the last dimension and solution are stored in
   * global fields, and if the board dimension has not been changed, are
   * updated only those squares that change their contents.
   *
   * @param solution The char array with the solutions to the n-queens problem.
   * @param dim Size of the board.
   *
   **/
  private void displaySolution(char[] solution, int dim) {

    Color c;

    if (dim == this.prevDim) {
      for (int i = 0; i < dim; i++) {
	if (((i+prevSolution[i]-1) % 2) == 0)
	  c = Color.black;
	else
	  c = Color.white;
	pnBoard.remove(i*prevDim+prevSolution[i]-1);
	pnBoard.add(new Square(c, Square.EMPTY), i*prevDim+prevSolution[i]-1);
	if (((i+solution[i]-1) % 2) == 0)
	  c = Color.black;
	else
	  c = Color.white;
	pnBoard.remove(i*dim+solution[i]-1);
	pnBoard.add(new Square(c, Square.QUEEN), i*dim+solution[i]-1);
      }
    }
    else {
      pnBoard.removeAll();
      pnBoard.setLayout(new GridLayout(dim, dim));

      for (int i = 0; i < dim; i++)
	for (int j = 0; j < dim; j++) {
	  if (((i+j) % 2) == 0)
	    c = Color.black;
	  else
	    c = Color.white;

	  if (solution[i] == j+1)
	    pnBoard.add(new Square(c, Square.QUEEN));
	  else
	    pnBoard.add(new Square(c, Square.EMPTY));
	}
    }
    prevDim = dim;
    prevSolution = solution;

    this.show();

  }

  /**
   * Displays the solution to the n-queens problem received as argument,
   * using the previous board size.
   * Just calls to displaySolution(char[], int) with the previous board
   * size.
   *
   **/
  private void displaySolution(char[] solution) {

    displaySolution(solution, prevDim);

  }

  /**
   *-------------------------*
   *      INNER CLASSES      *
   *-------------------------*
   **/

  /**
   * Ok button handler.
   **/
  class btnOkAction implements ActionListener {
    private queens frame;

    /**
     * Constructor. Creates a new handler storing the
     * caller frame on the <code>frame</code> field
     **/
    public btnOkAction(queens q) {
      frame = q;
    }

    /**
     * Event handler method. Performs the action associated to the
     * Ok button.
     * Tests if the board size is between the limits, and shows the
     * first solution of the n-queens problem.
     *
     * @param e <code>ActionEvent</code> object that represents the
     *        event that started this method.
     **/
    public void actionPerformed(ActionEvent e) {

      int dim;

      try {
	dim = (Integer.valueOf(frame.txtDimension.getText())).intValue();
      } catch (NumberFormatException exc) {
	return;
      }

      if (dim < 4 || dim > 20) {
	System.err.println("N-queens can only be solved on boards of");
	System.err.println("size 4x4 to 20x20.");
      }
      else
	frame.displaySolution(frame.solveProblem(dim), dim);
    }
  }

  /**
   * Next Sol. button handler.
   **/
  class btnNextAction implements ActionListener {
    private queens frame;
    
    /**
     * Constructor. Creates a new handler storing the
     * caller frame on the <code>frame</code> field
     **/
    public btnNextAction(queens q) {
      frame = q;
    }
     
    /**
     * Event handler method. Performs the action associated to the
     * Next Sol. button.
     * Puts on the window the next solution to the n-queens problem.
     *
     * @param e <code>ActionEvent</code> object that represents the
     *        event that started this method.
     **/
    public void actionPerformed(ActionEvent e) {
      frame.displaySolution(frame.nextSolution());
    }
  }

  /**
   * Exit button handler.
   **/
  class btnExitAction implements ActionListener {
     
    /**
     * Event handler method. Terminates the java program.
     **/
    public void actionPerformed(ActionEvent ev) {
	try {
	    plServer.stop();
	    System.exit(0);
	} catch (Exception e) {
	    System.err.println("Problems stopping Prolog server: " + e);
	    System.exit(1);
	}
    }
  }

  /**
   * This class represents one square of the board.
   * Is used to display all the squares of the board
   * on the screen, with the appropiate color and 
   * with or without a queen inside.
   **/
  class Square extends Panel {

    public static final String QUEEN = "Q";
    public static final String EMPTY = "";
  
    public Square(Color c, String piece) {
      this.setBackground(c);
      if (c.equals(Color.black))
	this.setForeground(Color.white);
      else
	this.setForeground(Color.black);
      this.setFont(new Font("Symbol",Font.PLAIN,28));
      this.add(new Label(piece));
    }
  
  }
}

