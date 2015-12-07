package CiaoJava;

import java.io.*;
import java.net.*;

/**
 *
 * This class creates a server socket and keeps it listening at
 * socket port #5000 (or number given in constructor).
 *
 */
public class PLServerSocket {
  private ServerSocket sock = null;
  private int port = 5000;
  private final int MAXPORT = 5010;

  /**
   * Server socket creation.
   * Creates a new server socket at port 5000 or
   * higher, and prints the port # at the standard output.
   * When the openSocket() method be called, the object will be
   * listening at this port number for connections.
   */
  public PLServerSocket() throws IOException {
    boolean done = false;

    while (!done) {
      try {
        sock = new ServerSocket(port);
        done = true;
      }
      catch(IOException e) {
        if (++port > MAXPORT) {
          System.err.println("Error creating ServerSocket: " + e);
          throw e;
        }
      }
    }
  }

  /**
   * Accepts a client request for this socket port.
   *
   * @return A <code>Socket</code> object that represents the
   *         connection accepted.
   **/
  public Socket openSocket() throws IOException {
    
    Socket s = sock.accept();
    System.err.println("Connection from " + s.getInetAddress() +
                       ":" + s.getPort());
    return s;

  }

  /**
   * Returns the <code>BufferedReader</code> input stream for this
   * socket connection. If the socket isn't opened, returns <code>null</code>.
   *
   * @param s <code>Socket</code> object from which the stream reader
   *          is to be obtained.
   *
   * @return  the <code>BufferedReader</code> object that represents
   *          the reader of the socket received as argument.
   **/
  protected static BufferedReader getReader(Socket s) throws IOException {
    return new BufferedReader(new InputStreamReader(s.getInputStream()));
  }

  /**
   * Returns the PrintWriter output stream for this
   * socket connection. If the socket isn't opened, returns null.
   *
   * @param s <code>Socket</code> object from which the stream writer
   *          is to be obtained.
   *
   * @return  the <code>PrintWriter</code> object that represents
   *          the writer of the socket received as argument.
   **/
  protected static PrintWriter getWriter(Socket s) throws IOException {
    return new PrintWriter(s.getOutputStream());
  }

  /**
   * Returns de port number on which the <code>PLServerSocket</code> object
   * is listening.
   *
   * @return the port number on which the <code>PLServerSocket</code>
   *         object is listening.
   */
  public int getPort() {
    return port;
  }

}






