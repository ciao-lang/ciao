package CiaoJava;

import java.io.*;
import java.util.*;

/**
 * Class for handling the output streams of the interface sockets.
 * This class is internal to the interface and 
 * should not be used by user programs.
 *
 * This class handles the (possible) concurrent requests for
 * writing to a socket input stream. It contains a data structure
 * to store the messages queued to be sent to Prolog.
 * 
 */
class PLSocketWriter extends Thread {

    /*
     * Internal constants.
     */
    private static final int STARTING_CAPACITY = 10;
    private static final int INCREMENT = 10;

    /**
     * Stream to which the handler writes Prolog commands.
     */
    private PrintWriter out;

    /**
     * Queue of messages posted by other Java threads, waiting to be 
     * sent to the Prolog side.
     */
    private Vector msgQueue;

    /**
     * Handler constructor. Starts a new handler thread writing 
     * messages to the <code>PrintWriter</code> object given as
     * argument.
     *
     * @param out Stream on which this handler writes the messages.
     */
    public PLSocketWriter(PrintWriter out) {
	//	super("PLSocketWriter");
	this.out = out;
	msgQueue = new Vector(STARTING_CAPACITY, INCREMENT);
	this.start();
    }

    /**
     * Thread code. Gets messages from the message queue and sends
     * them to the Prolog side.
     */
    public void run() {
	try {
	    PLStructure s;
	    do {
		s = (PLStructure)getMsg(); // gets the message and
		                                       // removes it from
                                                       // the queue.
		if (PLConnection.debugging)
		    System.err.println("PLSocketWriter:" + s);
		out.print(s.fastWrite());
		out.flush();
	    } while (!s.getArg(1).equals(PLTerm.terminate));
	} catch (Exception e) {
	    // No exception can be thrown here.
	    System.err.println("PLSocketWriter error: " + e);
	    System.exit(1);
	}
	if (PLConnection.debugging)
	    System.err.println("Terminating PLSocketWriter");
    }

    /**
     * Writes a Prolog message to the output stream. This method
     * puts that message to the message queue, and then the internal
     * handler thread gets it and sends it to the Prolog side.
     * This handler uses concurrent threads to avoid race conditions
     * when using the stream.
     *
     * The msg received as argument must be a correct message structure
     * for the specific communication to Prolog: if the communication
     * stream is the Prolog-to-Java socket, must be a PLStructure whose
     * functor is "pj", and with two arguments, identifier and message;
     * if the communication stream is the Java-to-Prolog socket, must
     * be a PLStructure whose functor is "jp" and with the same two
     * arguments.
     *
     */
    public synchronized void write(PLTerm msg) {
	if (msgQueue.isEmpty()) {
	    msgQueue.add(msg);
	    notify();
	} else
	    msgQueue.add(msg);
    }

    /**
     * Gets a message from the msgQueue and removes it from that structure.
     */
    private synchronized PLTerm getMsg() {
	if (msgQueue.isEmpty())
	    try {
		wait();
	    } catch (Exception e) {
		// No exception can be thrown here.
		System.err.println("PLSocketWriter error: " + e);
		System.exit(1);
	    }

	PLTerm msg = (PLTerm)msgQueue.firstElement();
	msgQueue.remove(0);
	return msg; 
    }

    /**
     * Clears the internal data structures.
     */
    private synchronized void clear() {
	msgQueue.clear();
    }
}
