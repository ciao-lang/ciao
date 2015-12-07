package CiaoJava;

import java.io.*;
import java.util.*;

/**
 * Class for handling the input streams of the interface sockets.
 * This class is internal to the interface and 
 * should not be used by user programs.
 *
 * This class handles the (possible) concurrent requests for
 * reading from a socket input stream. It contains a data structure
 * to store the messages not requested from other Java threads,
 * and another data structure for subscribing threads to be notified
 * when a message to them is received. Those threads are locked when
 * trying to read a message that has not been received.
 * 
 * Important: only is allowed a thread waiting for a message id, so
 * calls to read must be externally synchronized (specially the calls
 * with <code>PLConnection.ID_INTERFACE</code> message identifier).
 */
class PLMultithreadSocketReader extends Thread {

    /*
     * Internal constants.
     */
    private static final int STARTING_CAPACITY = 10;
    private static final int INCREMENT = 10;
    private static final float FACTOR = 0.75f;

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
     * This data structure is composed as a hashtable whose keys are
     * the message identifiers, and whose values are vectors of
     * messages. An element is removed from the hashtable when all
     * of its messages are read.
     */
    private Hashtable msgTable;

    /**
     * List of request objects waiting for messages. It is a TreeMap
     * data structure where the keys and the values contain an Integer
     * object with the message identifier. The thread that is waiting
     * messages for an identifier will wait on this Integer object;
     * when a message is received with that identifier, the socket
     * reader will notify the thread waiting for it.
     */
    private Hashtable rqsTable;

    /**
     * Handler constructor. Starts a new handler thread reading at the
     * <code>BufferedReader</code> stream received as argument.
     *
     * @param in Stream on which this reader receives messages.
     */
    public PLMultithreadSocketReader(BufferedReader in, 
				     PLSocketWriter writer) {
	//	super("PLMultithreadSocketReader");
	this.in = in;
	this.writer = writer;
	msgTable = new Hashtable(STARTING_CAPACITY, FACTOR);
	rqsTable = new Hashtable(STARTING_CAPACITY, FACTOR);
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
	    } catch (Exception e){
		System.err.println("PLMultithreadSocketReader: Socket broken");
		System.exit(1);
	    }
	    try {
		if (PLConnection.debugging)
		    System.err.println("PLMultithreadSocketReader:" + s);
		Object jId = s.getArg(0).hashKey();
                if (rqsTable.get(jId) != null) {
		    synchronized(rqsTable.get(jId)) {
			addMsg(jId,s.getArg(1));
			// If there is a request waiting to this message,
			// the requester is notified.
			rqsTable.get(jId).notify();
		    }
		}
		else
		    addMsg(jId,s.getArg(1));
	    } catch (Exception e) {
		System.err.println("PLMultithreadSocketReader error: " + e);
		System.exit(1);
	    }
	} while (!s.getArg(1).equals(PLTerm.terminate));
	writer.write(s);
	if (PLConnection.debugging)
	    System.err.println("Terminating PLMultithreadSocketReader");
    }

    /**
     * Reads a Prolog term from the input stream. This method
     * locks the caller thread until the requested term is
     * received. Internally, looks at the message queue to check
     * if that term has been already received, or waits for a
     * term received with the id given as argument.
     *
     * @param id Prolog identifier of the goal/request that
     *           is expecting that term. This argument must be
     *           of type <code>PLTerm</code> in order to allow
     *           thread synchronization if the caller thread
     *           has to wait until the requested term is 
     *           received.
     */
    public PLTerm read(PLTerm id) throws PLException {
	PLTerm msg = null;
	Object jId = id.hashKey();
	msg = getMsg(jId); // gets and removes the msg from the list.
	if (msg == null) {
	    // Requested term has not been yet received. caller
	    // thread must wait on its id until the message is received.
	    if (!rqsTable.containsKey(jId)) {
		rqsTable.put(jId,jId);
		synchronized(rqsTable.get(jId)) {
		    try {
			rqsTable.get(jId).wait();
		    } catch (InterruptedException e) {
			throw new PLException("PLMultithreadSocketReader:"+ e);
		    }
		    rqsTable.remove(jId);
		}
		msg = getMsg(jId); // gets the message and removes
                                   // it from the list.
		if (msg == null) 
		    throw new PLException("PLMultithreadSocketReader: message received from Prolog but not found in Java.");
	    } else
		throw new PLException("PLMultithreadSocketReader: message id is being used by another thread");
	}
	return msg;
    }

    /**
     * Adds a message to the msgTable data structure. This method is not
     * synchronized because the only caller thread will be the handler 
     * of this stream (and it will be unique).
     */
    private void addMsg(Object id,PLTerm t) {
	Vector msgList = (Vector)msgTable.get(id);
	if (msgList == null) {
	    msgList = new Vector(STARTING_CAPACITY,INCREMENT);
	    msgList.add(t);
	    msgTable.put(id,msgList);
	} else
	    msgList.add(t);
    }

    /**
     * Gets a message from the msgTable data structure and removes
     * it from that structure.
     */
    private synchronized PLTerm getMsg(Object id) {
	Vector msgList = (Vector)msgTable.get(id);
	if (msgList == null) 
	    return null;
	else {
	    if (!msgList.isEmpty()) {
		PLTerm msg = (PLTerm)msgList.firstElement();
		msgList.remove(0);
		if (msgList.isEmpty())
		    msgTable.remove(id);
		return msg;
	    } else {
		// This should not occur.
		msgTable.remove(id);
		return null;
	    }
	}
    }

    /**
     * Clears the internal data structures.
     */
    private synchronized void clear() {
	msgTable.clear();
	rqsTable.clear();
    }
}

