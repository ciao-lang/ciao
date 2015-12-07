import CiaoJava.*;
import java.awt.*;
import java.awt.event.*;

/**
 * ABSTRACT
 * Java to ciao interface example.
 **/
public class example1 extends Frame {

  // Components to be displayed.
  private Label lblQuery = new Label();
  private Label lblResult1 = new Label();
  private Label lblResult2 = new Label();
  private Button btnExit = new Button("Exit");

  // Prolog process.
  private static PLConnection plServer = null;

  /**
   * Start method. 
   **/
  public static void main(String argv[]) {
    System.err.println("Starting connection");
    try {
	if (argv.length == 0)
	    plServer = new PLConnection();
	else
	    plServer = new PLConnection(argv);
    } catch (Exception e) {
      System.err.println("Problems starting java server: " + e);
      e.printStackTrace();
      System.exit(1);
    }
    System.err.println("Started connection");

    example1 q = new example1();
    q.show();
    q.call_prolog();
  }

  /**
   * Frame constructor. 
   **/
  public example1() {
      super("Example 1");
    btnExit.addActionListener(new btnExitAction());
    setLayout(new GridLayout(4,1));
    add(lblQuery);
    add(lblResult1);
    add(lblResult2);
    add(btnExit);
    setSize(300,300);
  }

  void call_prolog() {
    PLTerm[] list = {new PLAtom("a"), new PLAtom("b")};
    PLList plList = null;

    try {
	plList = new PLList(list);
    } catch (PLException e) {}

    PLVariable plX = new PLVariable();
    PLVariable plY = new PLVariable();
    PLTerm[] args = {plX, plY, plList};
    PLStructure strGoal = new PLStructure("append", args);
    PLGoal goal = new PLGoal(plServer,strGoal);
    try {
      goal.useModule("library(lists)"); // for lists:append/3
      lblQuery.setText("Query: " + strGoal);
      goal.query();
      goal.nextSolution();
      lblResult1.setText("Solution 1: plX = " + plX + "; plY = " + plY);
      goal.nextSolution();
      lblResult2.setText("Solution 2: plX = " + plX + "; plY = " + plY);
    } catch (Exception e) {
      System.err.println("Problems launching goal: " + e);
      System.exit(1);
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
}

