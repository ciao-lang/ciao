/*---------------------------------------------------------------
 * example0.java
 * Java to ciao interface example with no graphical interface. 
 */
import CiaoJava.*;
// import java.awt.*;
// import java.awt.event.*;

public class example0 {

    // Prolog process.
    private static PLConnection plServer = null;

    public static void main(String argv[]) {
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
	    System.out.println("Query: " + strGoal);
	    goal.query();
	    
	    while (goal.nextSolution() != null)
		System.out.println("Solution: " + strGoal);
	    System.out.println("There are no more solutions");
	} catch (Exception e) {
	    System.err.println("Problems launching goal: " + e);
	    System.exit(1);
	}

	try {
	    plServer.stop();
	    System.exit(0);
	} catch (Exception e) {
	    System.err.println("Problems stopping Prolog server: " + e);
	    System.exit(1);
	}

    }
}
/*---------------------------------------------------------------*/

