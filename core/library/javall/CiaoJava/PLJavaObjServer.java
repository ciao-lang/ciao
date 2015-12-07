package CiaoJava;

/**
 * This class implements the Java object server,
 * implemented as a different thread that 
 * reads Prolog requests, interprets them,
 * and do the proper actions.
 * 
 * This class is internal to the interface and 
 * should not be used by user programs.
 */
class PLJavaObjServer extends Thread {

    private PLConnection pl = null;

    /**
     * Creates a new Java object server. Starts the
     * internal object server thread.
     *
     * @param pl PLConnection object to perform the
     *           communication to Prolog.
     */
    public PLJavaObjServer(PLConnection pl) {
	//	super("PLJavaObjServer");
	this.pl = pl;
	this.start();
    }

    /**
     * Thread code. Runs the Java object server itself.
     * in the constructor.
     */
    public void run() {
	PLInterpreter i = pl.getInterpreter();
	PLStructure cmd;

	try {
	    do {
		PLTerm t = pl.fromPrologPJ();
		cmd = (PLStructure)t;
		PLTerm response = i.interpret(cmd.getArg(1));
		if (!response.equals(PLTerm.terminate))
		    pl.toPrologPJ(cmd.getArg(0),response);
	    } while (!cmd.getArg(1).equals(PLTerm.terminate));
	    pl.joinSocketHandlers(); // waits for termination of all 
	                             // but myself.
	    pl.closeSocketStreams();
	} catch (Exception e) {
	    System.err.println("PLJavaObjServer error: " + e);
	    e.printStackTrace();
	    System.exit(1);
	}
	if (PLConnection.debugging)
	    System.err.println("Terminating PLJavaObjServer");
    }
}
