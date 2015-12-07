package CiaoJava;

/**
 * This class implements the Java representation of a Prolog string.
 * Although in Prolog strings are represented as lists of integers,
 * internally a more eficient implementation is made. The Java side
 * of this interface has implemented the string representation
 * accordingly to the byte representation given by the serialization
 * mechanism.
 */
public class PLString extends PLTerm {
    private String Value;

    /**
     * Given a Java string, creates a Java <code>PLString</code>
     * object.
     *
     * @param s String that will contain the Prolog string.
     */
    public PLString(String s) {

	Type = PLTerm.STRING;
	Value = s;

    }

    /**
     * String representation of a Java PLString object.
     *
     * @return a Java string with a copy of the Prolog string. 
     */
    public String toString() {

	return new String(Value);

    }

    /**
     * Gets the value of a Prolog string object.
     *
     * @return the value of this Prolog string object.
     */
    public String getValue() {

	return Value;

    }

    /**
     * Java representation of a <code>PLString</code>. Creates a copy
     * of the value of this <code>PLString</code> and returns it.
     *
     * @param i <code>PLInterpreter</code> object used to 
     *          obtain the Java representation. Included
     *          here only for compatibility purposes.
     *
     * @return a Java object representation of this Prolog string.
     */
    public Object javaRepr(PLInterpreter i) {

	return new String(Value);

    }

    /**
     * Java representation of a <code>PLString</code>. Creates a copy
     * of the value of this <code>PLString</code> and returns it.
     *
     * @return a Java object representation of this Prolog string.
     */
    public Object javaRepr() {
	return new String(Value);
    }

    /**
     * Execution test on Prolog objects. Returns true if the
     * related Prolog term can be evaluated. Included here
     * only for compatibility purposes.
     *
     * @return Always <code>false</code>.
     */
    public boolean isRunnable() {

	return false;

    }

    /**
     * Compares this <code>PLString</code> object with the <code>PLTerm</code> given as
     * argument. Implementation of the method inherited from PLTerm.
     *
     * @param t Prolog term to be compared to.
     *
     * @return <code>true</code> if this Prolog string is equal
     *         to the term received as argument;
     *         <code>false</code> otherwise.
     */
    public boolean equals(PLTerm t) {

	if (Type == t.Type && Value.equals(((PLString)t).getValue()))
	    return true;
	else
	    return false;
    }

    /**
     * Makes a full copy of this <code>PLString</code>
     * object.
     *
     * @return a copy of this Prolog term.
     */
    public PLTerm copy() {

	return (PLTerm)(new PLString(Value));

    }
    

    /**
     * Returns the number of characters of this <code>PLString</code>.
     *
     * @return The number of characters of this Prolog string.
     */
    public int length() {

	return Value.length();

    }

    /**
     * Returns the number of cells needed to represent
     * this PLString in the Prolog heap. Only used for
     * $fast_read format version 'a'.
     *
     * @return 0 (no cells used).
     */
    int numberOfCells() {

	return 0;

    }

    /*jcf 27.01.03*/
    /** Converts this Prolog string into a Prolog list 
     * of ASCII codes of this string.
     *
     * @return the list of ASCII codes of this Prolog string. 
     */
    public PLList toPLList() throws PLException {

	PLInteger[] il = new PLInteger[Value.length()];
	int i;
	
	for(i = 0; i < Value.length(); i++)
	    il[i] = new PLInteger(Value.charAt(i));
      
	return new PLList(il);

    }
    /**/
}

