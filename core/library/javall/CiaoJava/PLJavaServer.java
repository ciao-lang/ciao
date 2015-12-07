package CiaoJava;

import java.net.*;

/**
 * This class implements the starting point of the server
 * of Java objects to Prolog. Starts a socket server
 * listening on a specific port 
 * performs the basic server loop. Includes the
 * <code>main</code> method to be executed at
 * server start up.
 */
public class PLJavaServer {

    /**
     * Start up method. Starts the sockets and 
     * prepares the server to receive Prolog requests.
     * If there is any error creating sockets terminates
     * the Java process.
     */
    public static void main(String argv[]) {

	int port = 0;
	int max = 2; //only one Prolog process can communicate.
	boolean server = false;

	// Checking command line arguments
	int n = 0;
	while (argv.length > n) {
	    if (argv[n].equals("-server")) {
		server = true;
		n++;
	    } else if (argv[n].equals("-port")) {
		n++;
		if (argv.length > n) {
		    try {
			port = Integer.parseInt(argv[n]);
		    } catch(NumberFormatException e) {
			usage();
			System.exit(1);
		    }
		    n++;
		}
	    } else if (argv[n].equals("-max")) {
		n++;
		if (argv.length > n) {
		    try {
			max = Integer.parseInt(argv[n]) * 2;
		    } catch(NumberFormatException e) {
			usage();
			System.exit(1);
		    }
		    n++;
		}
	    } else if (argv[n].equals("-help")) {
		usage();
		System.exit(0);
	    } else {
		usage();
		System.exit(1);
	    }
	}

	try {
	    ServerSocket ss = new ServerSocket(port, max);

	    if (server) {
		System.err.println("Java Server started.");
		while (true) {
		    PLConnection pl = new PLConnection(ss);
		}
	    } else {
		PLConnection pl = new PLConnection(ss);
		pl.join();
	    }
 	    ss.close();
	} catch (Exception e) {
	    System.err.println("Problems starting java server: " + e);
	    System.exit(1);
	}
	if (PLConnection.debugging)
	    System.err.println("Terminating PLJavaServer");
	System.exit(0);
    }

    private static void usage() {

	System.err.println("Options: -port N   Port number on which accept connections");
	System.err.println("         -max  M   Maximum number of concurrent connections");
	System.err.println("         -help     This help");
    }

}

