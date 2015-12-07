package CiaoJava;

import java.util.Vector;

/**
 * Java representation of a Prolog list. Contains a representation
 * of a Prolog list using the same head/tail structure of the Prolog lists
 * The last element of the list must be an empty list.
 * The empty list must be referred as the <code>PLTerm.nil</code> static
 * field, due to the serialization mechanism of Prolog terms, that
 * represents the empty list as an atom, not a list.
 */
public class PLList extends PLTerm {
    private PLTerm Head;
    private PLTerm Tail;
    private final int START_CAPACITY = 16;
    private final int INCREMENT = 16;

    /**
     * Given a head and a tail, creates a Java PLList object.
     * The tail term must be nil or another <code>PLList</code>.
     *
     * @param h First element of the list. Can be any Prolog term.
     * @param t Rest of the list. Must be nil (if the list contains
     *          just one element), or another <code>PLList</code> object.
     */
    public PLList(PLTerm h, PLTerm t) throws PLException {
	Type = PLTerm.LIST;
	if ((t.equals(PLTerm.nil)) || (t.Type == Type)) {
	    Head = h;
	    Tail = t;
	}
	else {
	    throw new PLException("Error: wrong tail type (" + t.Type + ")");
	}
    }

    /**
     * Given a Java list, creates a Java PLList object
     * containing the objects included in the Java list.
     * Is important to realize that the array argument must contain
     * at least one element. An empty list is implemented in the
     * Java representation as an atom with name "[]".
     *
     * @param list Java list that contains the elements that must be
     *             included in the Prolog list.
     */
    public PLList(PLTerm list[]) throws PLException {
	Type = PLTerm.LIST;

	if (list.length > 1) {
	    PLTerm tail[] = new PLTerm[list.length-1];
	    System.arraycopy(list, 1, tail, 0, list.length-1);
	    Head = list[0];
	    Tail = new PLList(tail);
	}
	else if (list.length == 1) {
	    Head = list[0];
	    Tail = PLTerm.nil;
	}
	else
	    throw new PLException("Nil cannot be of type PLList");
    }

    /**
     * Given a Java string, creates a Java PLList object
     * containing the characters included in the Java string.
     * Is important to realize that the string argument must contain
     * at least one character. An empty list is implemented in the
     * Java representation as an atom with name "[]".
     *
     * @param s Java string that contains the characters that must be
     *          included in the Prolog list as elements.
     */
    public PLList(String s) throws PLException {
	Type = PLTerm.LIST;

	if (s.length() > 1) {
	    Head = new PLInteger(s.getBytes()[0]);
	    Tail = new PLList(s.substring(1));
	}
	else if (s.length() == 1) {
	    Head = new PLInteger(s.getBytes()[0]);
	    Tail = PLTerm.nil;
	}
	else
	    throw new PLException("Nil cannot be of type PLList");
    }

    /**
     * String representation of a Java PLList object.
     *
     * @return The string representation of the Prolog list.
     */
    public String toString() {
	PLList t;
	String s = "[";

	t = this;
	while (t.getTail().Type == Type) {
	    s = s + t.getHead().toString() + ", ";
	    t = (PLList)t.getTail();
	}
    
	return s + t.getHead().toString() + "]";
    }

    /**
     * Java representation of a PLList. The Java representation
     * of a Prolog list is an object array.
     *
     * @param i <code>PLInterpreter</code> object used to do
     *          the interpretation. Included here only for
     *          compatibility purposes with the <code>PLTerm</code>
     *          abstract class.
     *
     * @return  a Java object that represents the Prolog list.
     *          This Java representation is built by an
     *          <code>Object</code> array.
     */
    public Object javaRepr(PLInterpreter i) {
	Vector v = new Vector(START_CAPACITY, INCREMENT);
	Object[] a;
	PLTerm t;

	t = this;
	do {
	    v.addElement(((PLList)t).getHead().javaRepr(i));
	    t = ((PLList)t).getTail();
	} while (t.Type == Type);
	a = new Object[v.size()];
	v.copyInto(a);
	return (Object)a;
    }

    /**
     * Execution test on Prolog objects. Returns true if the
     * related Prolog term can be evaluated. Implements the
     * abstract method declared in the <code>PLTerm</code> class.
     *
     * @return always <code>false</code>. 
     */
    public boolean isRunnable() {
  	return false;
    }

    /**
     * Gets the head of a PLList object.
     *
     * @return the first element of the Prolog list.
     */
    public PLTerm getHead() {
	return Head;
    }

    /**
     * Gets the tail of a PLList object. The object returned may be
     * nil or another list.
     *
     * @return the Prolog list result of removing the first element
     *         of this list.
     */
    public PLTerm getTail() {
	return Tail;
    }

    /**
     * Sets the tail of a PLList object, removing the previous tail.
     * Important: this method does not conform Prolog list handling
     * and must be used very carefully.
     *
     * @param l <code>PLList</code> or <code>PLTerm.nil</code> object
     *          that represents the new tail.
     */
    private void setTail(PLTerm l) throws PLException {
	if (l.isList() || l.isNil())
	    Tail = l;
	else
	    throw new PLException("The tail of a list must be a PLList or nil");
    }

    /**
     * Adds a term as the tail of a PLList object.
     *
     * @param term <code>PLTerm</code> object to be appended at the 
     *             end of this list.
     */
    public void add(PLTerm term) throws PLException {
	PLList mytail = this;

	while (mytail.getTail().isList())
	    mytail = (PLList)mytail.getTail();

	mytail.setTail(new PLList(term,PLTerm.nil));
    }

    /**
     * Appends the list given as argument as the tail of this PLList.
     *
     * @param tail <code>PLList</code> object or 
     *             <code>PLTerm.nil</code> to be appended at the  
     *             end of this list.
     */
    public void append(PLTerm tail) throws PLException {
	PLList mytail = this;

	if (tail.isNil() || tail.isList()) {
	    while (mytail.getTail().isList())
		mytail = (PLList)mytail.getTail();

	    mytail.setTail(tail);
	} else
	    throw new PLException("The tail of a list must be a PLList or nil");
    }

    /**
     * Compares the PLList object with the PLTerm given as
     * argument. Implementation of the method inherited from
     * PLTerm.
     *
     * @param t Prolog term to be compared to.
     *
     * @return  <code>true</code> if this term is equal to
     *          the term received as argument;
     *          <code>false</code> otherwise.
     */
    public boolean equals(PLTerm t) {

	if (Type == t.Type) {
	    if (Head.equals(((PLList)t).Head) && Tail.equals(((PLList)t).Tail))
		return true;
	    else
		return false;
	}
	else
	    return false;
    }

    /**
     * Makes a full copy of this <code>PLList</code> Prolog list
     * object. Recursively clones the elements of this term, in order to
     * return a separated copy of all the elements included
     * in this list.
     *
     * @return a <code>PLTerm</code> object that is a full
     *         copy of this list. All the elements of this list
     *         are copied in turn.
     */
    public PLTerm copy() {

	PLTerm head = this.getHead().copy();
	PLTerm tail = this.getTail().copy();
	PLList l = null;

	try {
	    l = new PLList(head, tail);
	} catch (PLException e) {
	    // Exception not handled: tail allways is a valid tail
	}
	return (PLTerm)l;

    }
    
    /**
     * Term unification. Unifies this Prolog list with the term
     * received as argument. This method overrides the one 
     * inherited from PLTerm.
     * 
     * <p><bold>Important:</bold> The unification is 'two sided':
     * the variables found in the term received as argument could
     * be bound in order to unify the complete terms. In the same
     * way, the variables found in this list could be bound to
     * unify both terms.</p>
     *
     * @param term Term to unify with.
     * @return     <code>true</code> if the unification is successful;
     *             <code>false</code> otherwise.
     */
    public boolean unify(PLTerm term) {

	if (term.isVariable()) {
	    if (((PLVariable)term).isFree()) {
		((PLVariable)term).bind(this);
		return true;
	    }
	    else
		return this.unify(((PLVariable)term).getBinding());
	}
	else if (Type == term.Type) {
	    PLList l = (PLList)term;
	    return (Head.unify(l.getHead()) && Tail.unify(l.getTail()));
	}

	return false;

    }

    /* Undo the unification made on this list using as pattern
     * the term received as argument.
     *
     * @param term Prolog term to be used as pattern for 
     *             backtracking.
     */
    void backtrack(PLTerm term) throws PLException {

	if (Type == term.Type) {
	    PLList l = (PLList)term;

	    this.getHead().backtrack(l.getHead());
	    this.getTail().backtrack(l.getTail());
	}
	else
	    throw new PLException("Object cannot be backtracked: " + this.toString());

    }

    /**
     * Returns the number of elements of this <code>PLList</code>.
     *
     * @return The number of elements of this Prolog list.
     */
    public int length() {
	PLList t;
	int len = 1;

	t = this;
	while (t.getTail().Type == Type) {
	    len++;
	    t = (PLList)t.getTail();
	}
    
	return len;
    }

    /**
     * Returns the number of cells needed to represent
     * this PLList in the Prolog heap. Only used to
     * build the fast_write representation in 'a' version.
     *
     * @return the number of cells needed.
     */
    int numberOfCells() {
	PLList t;
	int num = 2;

	t = this;
	while (t.getTail().Type == Type) {
	    num += t.getHead().numberOfCells() + 1;
	    t = (PLList)t.getTail();
	}
       
	return num;
    }

    /**
     * Returns the representation of this Prolog list
     * as a Prolog string, if possible.
     *
     * @return the string representation of this Prolog list. 
     */
    PLString toPLString() throws PLException {
	PLTerm t;
	String s = "";
	char c;

	t = this;
	while (t.Type == PLTerm.LIST) {
	    PLTerm head = ((PLList)t).getHead();
	    if (head.isInteger() &&
		((PLInteger)head).getValue() < 256) {
		c = (char)((PLInteger)head).getValue();
		s += c;
		t = ((PLList)t).getTail();
	    }
	}
    
	return new PLString(s);
    }
}

