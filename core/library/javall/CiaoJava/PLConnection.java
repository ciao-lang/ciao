package CiaoJava;

import java.io.*;
import java.net.*;

// import org.apache.commons.logging.Log;
// import org.apache.commons.logging.LogFactory;

/**
 * Class for managing communication to Prolog.
 * Starts and handles a connection to a Prolog process via sockets.
 * The PLConnection can be used in two ways, using the
 * <code>CiaoJava</code> interface as a Java
 * object server (using {@link #PLConnection()}),
 * or as a connection to a Prolog query server 
 * (using {@link CiaoJava.PLConnection#PLConnection(java.lang.String)}).
 * Working with a Prolog server using the Java side as a
 * client, the Prolog goals can be launched using 
 * {@link #query(CiaoJava.PLTerm)}
 * method with a <code>PLTerm</code> object representing a goal
 * (<code>PLAtoms</code> and <code>PLStructures</code>)
 * or creating and using {@link CiaoJava.PLGoal} objects.
 */
public class PLConnection {

    /**
   * Private fields.
   */
    private Process plProc = null;
    private PLInterpreter plInterpreter = null;
    static boolean debugging = false;

    private static PLConnection previousConnection = null;

    private BufferedReader pjIn;
    private PrintWriter pjOut;
    private BufferedReader jpIn;
    private PrintWriter jpOut;

    private ServerSocket ss;
    private Socket pjSocket;
    private Socket jpSocket;
    private PLSocketReader pjReader;
    private PLSocketWriter pjWriter;
    private PLMultithreadSocketReader jpReader;
    private PLSocketWriter jpWriter;
    private PLJavaObjServer jServer;

    private static final PLTerm PJ_SYNC = new PLAtom("data");
    private static final PLTerm JP_SYNC = new PLAtom("event");
    private static final PLTerm ID_INTERFACE = new PLInteger(0);
    
    // final Log LOG = LogFactory.getLog(PLConnection.class);

    /**
     * Creates a new <code>PLConnection</code> object, establishing
     * a new socket server and listening Prolog connections on a
     * free port.
     * The port number on which it listens is printed out on the 
     * standard output stream.
     * Once a Prolog process connects to it, starts the internal 
     * threads to manage the communication.
     */
    public PLConnection() throws PLException, IOException {
	ss = new ServerSocket(0);
	start();
	previousConnection = this;
    }

    /**
     * Creates a new <code>PLConnection</code> object that executes the
     * Prolog server, and starts it.
     *
     * @param    where     The command-line to start the Prolog process.
     *                     This constructor forces that the connection be
     *                     established to the newly created Prolog
     *                     process, instead of waiting for any Prolog
     *                     client that wants to connect to it.
     *                     This constructor allows an array of strings
     *                     for command-line arguments.
     */
    public PLConnection(String[] where) throws PLException, IOException {
	ss = new ServerSocket(0);
	start(where);
	previousConnection = this;
    }

    /**
     * Creates a new <code>PLConnection</code> object that executes the
     * Prolog server, and starts it.
     *
     * @param    where     The command-line to start the Prolog process.
     *                     This constructor forces that the connection be
     *                     established to the newly created Prolog
     *                     process, instead of waiting for any Prolog
     *                     client that wants to connect to it.
     */
    public PLConnection(String where) throws PLException, IOException {
	ss = new ServerSocket(0);
	start(where);
	previousConnection = this;
    }

    /**
     * Creates a new PLConnection object, given an existing socket
     * server. This constructor is only useful when creating a
     * Java server that accept concurrent connections from 
     * different Prolog processes.
     * 
     * Once a Prolog process connects to it, starts the internal threads
     * to manage the communication.
     * 
     * @param ss <code>ServerSocket</code> object representing a new
     *           Prolog/Java server socket object.
     */
    public PLConnection(ServerSocket ss) throws PLException, IOException {
	this.ss = ss;
	start();
	previousConnection = this;
    }

    /**
     * Creates a new <code>PLConnection</code> object that executes the
     * Prolog server, and starts it. The connection port between Java side 
     * and Prolog side is fixed to the corresponding argument.
     *
     * @param    port      Port number to be used for the internal 
     *                     communication to Prolog side.
     *
     * @param    where     The command-line to start the Prolog process.
     *                     This constructor forces that the connection be
     *                     established to the newly created Prolog
     *                     process, instead of waiting for any Prolog
     *                     client that wants to connect to it.
     *                     This constructor allows an array of strings
     *                     for command-line arguments.
     */
    public PLConnection(int port, String[] where) throws PLException, IOException {
	this.ss = new ServerSocket(port,2);
	start(where);
	previousConnection = this;
    }

    /**
     * Creates a new <code>PLConnection</code> object that connects
     * to a Prolog server at host and port given as argument.
     *
     * @param    host      host name where the Prolog side is 
     *                     waiting for Java connection.
     * @param    port      Port number to be used for the internal 
     *                     communication to Prolog side.
     */
    public PLConnection(String host,int port) throws PLException, IOException {
	start(host,port);
	previousConnection = this;
    }


    /**
     * Starts a PLConnection to use the Java/Prolog
     * bidirectional interface. Starts the Prolog server
     * process and connects to it creating the sockets, and
     * starts the internal threads needed for interface communication.
     *
     * @param where command used to start the Prolog server process.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    private void start(String where) throws IOException, PLException {
	Runtime rt = Runtime.getRuntime();

	plProc = rt.exec(where); // runs plServer
	plProcHasNotDied(true);
	
	OutputStream pipeOut = plProc.getOutputStream();
	PrintStream out = new PrintStream(pipeOut);
	plInterpreter = new PLInterpreter(this);

	// port number output.
	int port = ss.getLocalPort();
	out.println(port + ".");
	out.flush();
// 	out.close();

	bindSockets(out);
    }

    /**
     * Starts a PLConnection to use the Java/Prolog
     * bidirectional interface. Starts the Prolog server
     * process and connects to it creating the sockets, and
     * starts the internal threads needed for interface communication.
     *
     * @param where command used to start the Prolog server process,
     *              including optional arguments.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    private void start(String[] where) throws IOException, PLException {
	Runtime rt = Runtime.getRuntime();

	plProc = rt.exec(where);
	plProcHasNotDied(true);
	
	OutputStream pipeOut = plProc.getOutputStream();
	PrintStream out = new PrintStream(pipeOut);
	plInterpreter = new PLInterpreter(this);

	// port number output.
	int port = ss.getLocalPort();
	out.println(port + ".");
	out.flush();
	out.close();

	bindSockets(out);
    }

    /**
     * Starts the PLConnection for the Prolog-to-Java
     * interface, connecting to an already executing Prolog server,
     * listening at port given as argument.
     *
     * @param  port           port number to which the Prolog
     *                        server is waiting for Java connection.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    public void start(String host, int port) throws IOException, PLException {
	plInterpreter = new PLInterpreter(this);

	connectSockets(host,port);
    }

    /**
     * Starts the PLConnection for the Prolog-to-Java
     * interface: waits for a Prolog connection.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    public void start() throws IOException, PLException {
	plInterpreter = new PLInterpreter(this);

	// port number output.
	int port = ss.getLocalPort();
	System.out.println(port + ".");
	System.out.flush();
	bindSockets(System.out);
    }
    
    /**
     * Tests if the pl Process has died.
     * In this case the java process hangs indefinitely, a not expected behavior.
     * ifDiedThrowException: Switches between throw exception or return true when died.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    private Boolean plProcHasNotDied (Boolean ifDiedThrowException) throws PLException {
    	// LOG.info("Testing if plProc has died. ");
    	Boolean hasDied;
    	try {
    		// LOG.info("Waiting 1 sec for plServer to load.");
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			// LOG.info("Interruption received when waiting for plServer to load.");
			e.printStackTrace();
		}
    	try{
    		plProc.exitValue(); // asks for plServer return value
    		// LOG.info("plProc has died. ERROR. ");
    		if (ifDiedThrowException) {
    			throw new PLException("ERROR: plServer process has died.");
    		}
    		hasDied = true;
    	}
    	catch (IllegalThreadStateException ex) { // if it is still running, all is ok.
    		// LOG.info("plProc has NOT died. OK. ");
    		hasDied= false;
    	}
    	return hasDied;
    }

    /**
     * Gets the Prolog Interpreter object used to interpret 
     * Prolog terms received from the Prolog side of the interface.
     */
    public PLInterpreter getInterpreter() {
	return plInterpreter;
    }

    /**
     * This private method binds and synchronizes the sockets 
     * for communication with the Prolog process.
     */
    private void bindSockets(PrintStream out) 
	throws IOException, PLException {
    
	// Open prolog-to-java socket.
	//**
	if (PLConnection.debugging)
	    System.err.println("Accepting connection...");
	pjSocket = ss.accept();
	if (PLConnection.debugging)
	    System.err.println("Connection accepted");
	pjIn = new BufferedReader(new InputStreamReader(pjSocket.getInputStream()));
	pjOut = new PrintWriter(pjSocket.getOutputStream());


	// Creating handler threads for
	// Prolog-to-Java communication.
	pjWriter = new PLSocketWriter(pjOut);
	pjReader = new PLSocketReader(pjIn, pjWriter);

	// Synchronizing prolog-to-java socket.
	PLTerm pjSync = fromPrologPJ();
	//if (!pjSync.equals(PJ_SYNC)) throw...
	toPrologPJ(ID_INTERFACE,PJ_SYNC);

	// Starting java-to-prolog socket.
	// TODO: both readers should use the same socket (?).
	//       (if this is impossible, both readers should
	//       use different port numbers.
	if (PLConnection.debugging)
	    System.err.println("Accepting connection...");
	jpSocket = ss.accept();
	if (PLConnection.debugging)
	    System.err.println("Connection accepted");
	jpIn = new BufferedReader(new InputStreamReader(jpSocket.getInputStream()));
	jpOut = new PrintWriter(jpSocket.getOutputStream());

	// Creating handler threads for
	// Java-to-Prolog communication.
	jpWriter = new PLSocketWriter(jpOut);
	jpReader = new PLMultithreadSocketReader(jpIn,jpWriter);

	// Synchronizing java-to-prolog socket.
	PLTerm jpSync = fromPrologJP(ID_INTERFACE);
	//if (!jpSync.equals(JP_SYNC)) throw...
	toPrologJP(ID_INTERFACE,JP_SYNC);

	// Creating Java object server.
	jServer = new PLJavaObjServer(this);
    }

    /**
     * This private method creates, connects, and synchronizes the
     * sockets for communication with the Prolog server.
     */
    private void connectSockets(String host,int port) 
	throws IOException, PLException {
    
	// Open prolog-to-java socket.
	//**
	if (PLConnection.debugging)
	    System.err.println("requesting connection...");
	pjSocket = new Socket(host,port);
	if (PLConnection.debugging)
	    System.err.println("Connection accepted");
	pjIn = new BufferedReader(new InputStreamReader(pjSocket.getInputStream()));
	pjOut = new PrintWriter(pjSocket.getOutputStream());


	// Creating handler threads for
	// Prolog-to-Java communication.
	pjWriter = new PLSocketWriter(pjOut);
	pjReader = new PLSocketReader(pjIn, pjWriter);

	// Synchronizing prolog-to-java socket.
	toPrologPJ(ID_INTERFACE,PJ_SYNC);
	//if (!pjSync.equals(PJ_SYNC)) throw...
	PLTerm pjSync = fromPrologPJ();

	// Starting java-to-prolog socket.
	// TODO: both readers should use the same socket (?).
	//       (if this is impossible, both readers should
	//       use different port numbers.
	if (PLConnection.debugging)
	    System.err.println("Requesting connection...");
	jpSocket = new Socket(host,port);
	if (PLConnection.debugging)
	    System.err.println("Connection accepted");
	jpIn = new BufferedReader(new InputStreamReader(jpSocket.getInputStream()));
	jpOut = new PrintWriter(jpSocket.getOutputStream());

	// Creating handler threads for
	// Java-to-Prolog communication.
	jpWriter = new PLSocketWriter(jpOut);
	jpReader = new PLMultithreadSocketReader(jpIn,jpWriter);

	// Synchronizing java-to-prolog socket.
	toPrologJP(ID_INTERFACE,JP_SYNC);
	//if (!jpSync.equals(JP_SYNC)) throw...
	PLTerm jpSync = fromPrologJP(ID_INTERFACE);

	// Creating Java object server.
	jServer = new PLJavaObjServer(this);
    }

    /**
     * Goal launching. Evaluates the term received as a query and
     * sends it to Prolog for evaluation.
     *
     * @param	term	Prolog term that will be evaluated as a Prolog
     *                  goal.
     *
     * @return The <code>PLGoal</code> object created to manage the goal.
     *
     * @exception IOException if there are I/O problems.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    public PLGoal query(PLTerm term) throws PLException, IOException {

	PLGoal goal = new PLGoal(this,term);
	goal.query();
	return goal;

    }

    /**
     * Low level Java-to-Prolog communication. This method sends 
     * Prolog terms from Java to Prolog. Transforms the term in
     * a serialized form (using the Prolog format) and sends the
     * result to Prolog through the prolog-to-java socket.
     *
     * @param term is an object representing a Prolog term.
     *
     */
    void toPrologPJ(PLTerm id, PLTerm term) {

	PLTerm[] arg = {id, term};
	PLStructure msg = new PLStructure("pj", arg);
	pjWriter.write(msg);
    }

    /**
     * Low level Java-to-Prolog  communication. This method sends
     * Prolog terms to Prolog through the Java-to-Prolog socket.
     *
     * @param term is an object representing a Prolog term.
     */
    void toPrologJP(PLTerm id, PLTerm term) {

	PLTerm[] arg = {id, term};
	PLStructure msg = new PLStructure("jp", arg);
	jpWriter.write(msg);

    }

    /**
     * Prolog-to-Java communication. This method listens at the Prolog
     * socket to receive results from the Prolog process as terms.
     *
     * @return Prolog term received from the prolog-to-java socket.
     *
     * @exception IOException if the socket stream has been broken.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    PLTerm fromPrologPJ() throws PLException {

	PLTerm msg = pjReader.read();
	return msg;

    }

    /**
     * Prolog-to-Java communication. This method listens at the 
     * Prolog-to-Java socket to receive requests from the Prolog
     * process as terms.
     *
     * @param id Message identifier (identifier given by Prolog
     *           when creating a new goal, or ID_INTERFACE if it
     *           is a message directed to the interface itself).
     *
     * @return Prolog term received from the Prolog-to-Java socket.
     *
     * @exception IOException if the socket stream has been broken.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    PLTerm fromPrologJP(PLTerm id) throws PLException {

	PLTerm msg = jpReader.read(id);
	return msg;

    }

    /**
     * Closes the communication to the Prolog side and terminates the
     * Prolog process.
     * 
     * @exception IOException if the socket stream has been broken.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     * @deprecated This method is deprecated. Use <code>stop</code> 
     *             method instead.
     */
    public void close() throws InterruptedException, IOException, PLException {

	stop();

    }

    /**
     * Stops the interface. Stops the internal threads and closes
     * the streams related to this interface.
     *
     * @exception IOException if the socket stream has been broken.
     * @exception PLException if there are problems regarding the Prolog
     *                        process.
     */
    public void stop() throws InterruptedException, IOException, PLException {

	toPrologJP(ID_INTERFACE,PLTerm.terminate);
	toPrologPJ(ID_INTERFACE,PLTerm.terminate);
	join(); // this method also closes socket streams.

    }

    /**
     * Closes interface sockets and related streams.
     *
     * @exception IOException if the socket stream has been broken.
     */
    protected void closeSocketStreams() throws InterruptedException, 
                                               IOException { 
	pjIn.close();
	pjOut.close();
	jpIn.close();
	jpOut.close();

	pjSocket.close();
	jpSocket.close();
	if (ss != null)
	    ss.close();

    }

    /**
     * Waits until all the internal threads terminate.
     */
    public void join() throws InterruptedException, IOException {

	joinSocketHandlers();
	jServer.join();
	//       	Thread.sleep(1000,0);
	closeSocketStreams();

    }

    /**
     * Waits until socket handling threads terminate.
     */
    protected void joinSocketHandlers() throws InterruptedException {

	jpReader.join();
	jpWriter.join();
	pjReader.join();
	pjWriter.join();
    }

    /**
     * Returns the last started connection to a Prolog process.
     */
    public static PLConnection getPreviousConnection() {
	return previousConnection;
    }

    public Process getPrologProcess() {
	return plProc;
    }

}
