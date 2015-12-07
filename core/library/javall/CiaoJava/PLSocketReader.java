package CiaoJava;

import java.io.*;
import java.util.*;

/**
 * Class for handling the input streams of the interface sockets.
 * This class is internal to the interface and 
 * should not be used by user programs.
 *
 * This class handles the requests for reading from a socket input
 * stream, but does not allow concurrent requests from several
 * threads (this is just for Prolog-to-Java communication; 
 * Java-to-Prolog multithreaded communication is supported by
 * <code>PLMultithreadSocketReader</code> class. It
 * contains a data queue to store the messages not requested from
 * the Java side of the interface.
 */
class PLSocketReader extends Thread {

    /*
     * Internal constants.
     */
    private static final int STARTING_CAPACITY = 10;
    private static final int INCREMENT = 10;

    /**
     * Stream from which the handler reads Prolog commands.
     */
    private BufferedReader in;

    /**
     * Socket writer object to be notified when termination is received.
     */
    private PLSocketWriter writer;

    /**
     * List of messages received but with no requests waiting for them.
     */
    private Vector msgQueue;

    /**
     * Handler constructor. Starts a new handler thread reading at the
     * <code>BufferedReader</code> stream received as argument.
     *
     * @param in Stream on which this reader receives messages.
     */
    public PLSocketReader(BufferedReader in, PLSocketWriter writer) {
	//	super("PLSocketReader");
	this.in = in;
	this.writer = writer;
	msgQueue = new Vector(STARTING_CAPACITY,INCREMENT);
	this.start();
    }

    /**
     * Thread code. Reads and processes messages from the stream given
     * in the constructor.
     */
    public void run() {
	PLStructure s = null;
	do {
	    try {
		s = (PLStructure)PLTerm.fastRead(in);
	    } catch (Exception e) {
		System.err.println("PLSocketReader: Socket broken");
		System.exit(1);
	    }
	    try {
		if (PLConnection.debugging)
		    System.err.println("PLSocketReader: " + s);
		addMsg(s);
	    } catch (Exception e) {
		System.err.println("PLSocketReader error: " + e);
		System.exit(1);
	    }
	} while (!s.getArg(1).equals(PLTerm.terminate));
	writer.write(s);
	if (PLConnection.debugging)
	    System.err.println("Terminating PLSocketReader");
    }

    /**
     * Reads a Prolog term from the input stream. This method
     * locks the caller thread until the requested term is
     * received. Internally, looks at the message queue to check
     * if that term has been already received, and then waits for a
     * term received with the id given as argument if the term
     * has not been received.
     *
     */
    public synchronized PLTerm read() throws PLException {
	PLTerm msg = null;
	if (msgQueue.isEmpty())
	    try {
		wait();
	    } catch(InterruptedException e) {
		throw new PLException("PLSocketReader error: " + e);
	    }
	try {
	    msg = (PLTerm)msgQueue.firstElement();
	} catch(NoSuchElementException e) {
	    // This error only occurs when the reader thread is stopped
	    // while waiting next msg at msgQueue.
	    throw new PLException("PLSocketReader error: Socket reader thread stopped");
	}
	msgQueue.remove(0);
	return msg;
    }

    /**
     * Adds a message to msgQueue.
     */
    private synchronized void addMsg(PLTerm msg) {
	if (msgQueue.isEmpty()) {
	    msgQueue.add(msg);
	    notify();
	} else
	    msgQueue.add(msg);
    }

    /**
     * Clears all the messages stored on the buffer.
     */
    private synchronized void clear() {
	msgQueue.clear();
    }
}
