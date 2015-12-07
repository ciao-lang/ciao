:- module(dictionary_tree, [
	create_dictionaries/1,
	is_dictionaries/1,
	get_definition_dictionary/2,
	get_prototype_dictionary/2,
	dictionary_insert/5,
	dictionary_lookup/5,
	merge_tree/2
	],
	[assertions,isomodes,regtypes,iso]).

:- doc(author, "G@..{o}ran Smedb@..{a}ck").

%:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(library(provrml/internal_types)).

:- set_prolog_flag(multi_arity_warnings, off).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(module,"This module offers a dynamic tree structured dictionary
                   a bit combined with predicates that gives it the useability
                   to be the dictionary for the parser.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_dictionaries(-Dictionary)
   :: dictionary
    # "Returns a dictionary. A general name was used if the user would like
       to  change the code to include more dictionaries.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dictionaries(dic(Proto)) :-
	create_tree(Proto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_dictionaries(?Dictionary)
   :: dictionary
    # "Is the argument a dictionary is solved by this predicate.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_dictionaries(dic(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_tree(-Tree)
	:: tree
        # "Creates an empty tree structure used as the base structure for a 
           dictionary.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tree(tree(_Key,_Value,_LeftTree,_RightTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_tree(+Element,-Tree)
	:: element * tree
        # "From an element construct a tree structure using the values 
           of the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tree(Element,tree(Key,Leaf, _Left, _Right)) :-
	get_element_key(Element,Key),
	create_leaf(Element,Leaf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- true prop element(Element) + regtype.
element(element(_,_,_)).

:- true prop leaf(Leaf) + regtype.
leaf(leaf(_,_,_,_)).

:- pred create_leaf(+Element,-Leaf)
	:: element * leaf
        # "Creates a leaf structure from a given element. The leaf is the 
           information post in the tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_leaf(element(Key,Type,Dic),leaf(Key,Type,Dic,_MoreLeafs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_definition_dictionary(+Dictionary,-Tree)
   :: dictionary * tree
    # "Returns the definition dictionary (for the moment there is only one
       dictionary), which is a tree representation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_definition_dictionary(dic(D),D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_prototype_dictionary(+Dictionary,-Tree)
   :: dictionary * tree
    # "Returns the prototype dictionary (for the moment there is only one
       dictionary), which is a tree representation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_prototype_dictionary(dic(P),P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_left_tree(+Tree,-Left_tree)
	:: tree * tree
        # "The predicate will return the left search tree of the input tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_left_tree(tree(_Key,_Value,Left,_Right),Left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_right_tree(+Tree,-Right_tree)
	:: tree * tree
        # "The predicate will return the left search tree of the input tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_right_tree(tree(_Key,_Value,_Left,Right),Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_tree_key(+Tree, -Key)
	:: tree * atm
        # "The predicate vill return the key value at the current position.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tree_key(tree(Key,_Value,_Left,_Right),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_tree_leaf(+Tree,-Leaf)
	:: tree * leaf
        # "The predicate returns the leaf value at the position, 
           that is the information that is set with the current key value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tree_leaf(tree(_Key,Leaf,_Left,_Right),Leaf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_information(+Leaf,-Info)
	:: leaf * term
        # "The predicate will return the information that is within the
           leaf. The information is what the user has placed as information
           and can be any type of term.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_information(leaf(_Key, _Type, Info,_MoreLeafs),Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_type(+Leaf,-Type)
	:: leaf * atm
        # "From a leaf structure will return the type, a user elected 
           template to identify different types of information inserts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_type(leaf(_Key,Type,_Info,_MoreLeafs),Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_rest(+Leaf,-Rest_of_leafs)
	:: leaf * leaf
        # "The leaf for a given key will contain not only one value, 
           but can contain more values with the same key but of different 
           properties. These extra values will be placed dynamically in the 
           open body of the leaf. So from a given leaf will return the 
           possible descendents with the same key value. If no leaf is bound 
           a variable is returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_rest(leaf(_Key,_Type,_Info,MoreLeafs),MoreLeafs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_key(+Leaf,-Key)
	:: leaf * atm
        # "From a leaf structure will return the key value for the leaf.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_key(leaf(Key,_Type,_Info,_More),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_last_leaf(+Leaf,-Last_leaf)
	:: leaf * leaf
        # "From a leaf returns the last leaf with the same key value.
           If it is the last leaf then the input will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_last_leaf(Leaf,Last_leaf) :-
	(  nonvar(Leaf)
	-> get_leaf_rest(Leaf,Rest),
	   ( nonvar(Rest)
	   ->get_last_leaf(Rest,Last_leaf)
	   ; Last_leaf = Rest
	   )
	;  Last_leaf = Leaf
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_next_leaf(+Leaf, -Next_leaf) 
	:: leaf * term
        # "The predicate will return the next leaf with the same key value.
           If the are no more leafs the atom last will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_next_leaf(Leaf,Next) :-
	get_leaf_rest(Leaf,Rest),
	(var(Rest)
	->
	Next = last
	;
	Next = Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred element_not_on_branch(+Element,+Leaf)
	:: element * leaf
        # "The predicate will check if the element not exists in the leaf
	   descendents, that is as the elements with the same key value. You
           are allowed to insert post that differs a bit, but we do not want 
           duplicates.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

element_not_on_branch(E,Leaf) :-
	( compare_element_leaf(E,Leaf)
	-> fail
	; get_next_leaf(Leaf,Next),
	  ( Next = last
	  ->true
	  ; element_not_on_branch(Next,E)
	  )
	).
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_element_leaf(+Element,+Leaf)
	:: element * leaf
        # "The predicate will compare the element values against those
           of a leaf.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_element_leaf(Element,Leaf) :-
	get_leaf_key(Leaf,Key),
	get_element_key(Element,Key),
	get_leaf_type(Leaf,Type),
	get_element_type(Element,Type),
	get_leaf_information(Leaf,Info),
	get_element_dic(Element,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_tree(+Tree) 
	:: tree
        # "The argument is a tree structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_tree(tree(K,V,_,_)) :-
	nonvar(K),
	nonvar(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_leaf(+Leaf)
	:: leaf
        # "The predicate is a leaf structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_leaf(Leaf) :-
	nonvar(Leaf),
	Leaf = leaf(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred got_left_tree(+Tree)
	:: tree
        # "The tree have a left tree instance.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
got_left_tree(Tree) :-
	is_tree(Tree),
	get_left_tree(Tree,B),
	nonvar(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred got_right_tree(+Tree)
	:: tree
        # "The tree have a right tree instance.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

got_right_tree(Tree) :-
	is_tree(Tree),
	get_right_tree(Tree,B),
	nonvar(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_key(+Element,-Key)
	:: element * atm
        # "From an element structure will return the search key for the 
           element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_key(element(Key,_Type,_Dic),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_type(+Element,-Type)
	:: element * atm
        # "Returns the element type for the given element, user specified.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_type(element(_Key,Type,_Dic),Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_dic(+Element,-Dictionary)
   :: element * dictionary
   # "Returns the ditionary type for the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_dic(element(_Key,_Type,Dic),Dic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-  pred choose_branch(+Key,+KeyPlaced,-Direction)
    :: atm * atm * atm
    # "The predicate will return the direction for the new key to be placed
       that is to the 'left' or to the 'right'. These two atoms are the 
       returned values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_branch(Key,Value,left) :-
	compare_key_lt(Key,Value),
	!.

choose_branch(Key,Value,right) :-
	compare_key_gt(Key,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred select_branch(+Direction,+Tree,-Tree)
   :: atm * tree * tree
   #  "The predicate will return the branch, the following sub tree
       for the given direction, that is 'left' or 'right'.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_branch(left,Tree,Left) :-
	get_left_tree(Tree,Left).

select_branch(right,Tree,Right) :-
	get_right_tree(Tree,Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred change_direction(+Key,+Tree,-Tree)
   :: atm * tree * tree
   # "The predicate will return the appropiate sub tree for the given 
      key value. The branch will be the right or the left subtree of the 
      given tree. Equal key value in the tree should be checked before 
      because this predicate only handle different key values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_direction(Key,Tree,Branch) :-
	get_tree_key(Tree,Tree_key),
	choose_branch(Key,Tree_key,Branch_choise),
	select_branch(Branch_choise,Tree,Branch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_key_eq(+Name,+Name)
   :: atm * atm
   # "The predicate will compare two names and will be true if they
      are equal.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_key_eq(K0,K1) :-
	compare_list_eq(K0,K1).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_key_leq(+Name,+Name)
   :: atm * atm
   # "The predicate will compare two names and will be true if they
      are equal or if the first name has a lower value according to the 
      ascii value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_key_leq(K0,K1) :-
	compare_list_leq(K0,K1).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_key_lt(+Name,+Name)
   :: atm * atm
   # "The predicate will compare two names and will be true if the
      first name has a lower value according to its ascii value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_key_lt(K0,K1) :-
	compare_list_lt(K0,K1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_key_gt(+Name,+Name)
   :: atm * atm
   # "The predicate will compare two names and will be true if the
      first name has a greater ascii value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_key_gt(K0,K1) :-
	compare_list_gt(K0,K1).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_seek(+Key,+Type,+Post_in,+Dictionary,-Post_out)
	:: atm * atm * term * tree * term
        # "This predicate is used to find the right place for a post
           and return the appropriate leaf structure to use for the 
           insert place.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_seek(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	get_last_leaf(Leaf,Last),
	get_leaf_information(Last,Info).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_seek(+Element,+Dictionary,-Post_out)
	:: element * tree * term
        # "This predicate is used to find the right place for an
           element and return the appropiate leaf structure to use 
           for the insert place. The returned value is the information
           in the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_seek(element(Key,Type,Info),Tree,Leaf) :-
	climb_tree(element(Key,Type,Info), Tree, Leaf).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_lookup(+Key,?Type,?Field,+Dictionary,-Info)
   :: atm * atm * term * dictionary * atm
   # "The predicate will search for the Key and return Info;defined or
       undefined accordingly. If defined the fields will be filled as well.
       The predicate do not insert the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_lookup(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_lookup(element(Name,Type,Field),Dictionary,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_lookup(+Element,+Tree,-Info)
   :: element * tree * term
   # "The predicate will search for the element and return Info;defined or
       undefined accordingly. If defined the fields will be filled as well.
       The predicate do not insert the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_lookup(E,Tree,Info) :-
	scan_tree(E,Tree,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_insert(+Key,+Type,+Field,+Dictionary,?Info)
   :: atm * atm * term * tree * atm
   # "The predicate will search for the place for the Key and return Info,
       if the element inserted had a post before (same key value) multiple
       else new. The dictionary is dynamic and do not need output because 
       of using unbinded variables.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_insert(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	(Leaf == new
	->
	Info = new
	; ( element_not_on_branch(element(Name,Type,Field),Leaf)
	  ->get_leaf_rest(Leaf, Rest_of_leafs),
	    get_last_leaf(Rest_of_leafs,Last_leaf),
	    create_leaf(element(Name,Type,Field),Last_leaf),
	    Info = multiple
	  ; true
	  ) 
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_insert(+Element,+Dictionary,-Info)
   :: element * tree * atm
   # "The predicate will search for the place for the right place
      for the element and return Info,
       if the element inserted had a post before (same key value) multiple
       else new. The dictionary is dynamic and do not need output because 
       of using unbinded variables.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_insert(element(Name,Type,Field),Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	(Leaf == new
	->
	Info = new
	; ( element_not_on_branch(element(Name,Type,Field),Leaf)
	  -> get_leaf_rest(Leaf, Rest_of_leafs),
	     get_last_leaf(Rest_of_leafs,Last_leaf),
	     create_leaf(element(Name,Type,Field),Last_leaf),
	     Info = multiple
	  ;  true
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred climb_tree(+Element,+Dictionary,-Post_out)
	:: element * tree * term
        # "This predicate is used to find the right place for an
           element and return the appropiate leaf structure to use 
           for the insert place. The returned value is the information
           in the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

climb_tree(Element,Tree,Leaf) :-
	get_element_key(Element,Element_key),
	get_tree_key(Tree,Tree_key),
	compare_key_eq(Element_key,Tree_key),
	!,
	get_tree_leaf(Tree,Leaf).

%Traversing
climb_tree(Element,Tree,Leaf) :-
	get_element_key(Element,Key),
	change_direction(Key,Tree,Branch),
	climb_tree(Element,Branch,Leaf).


%Inserting a new leaf.
climb_tree(Element,Tree,new) :-
	create_tree(Element,Tree),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred scan_tree(+Element,+Tree,-Info)
   :: element * tree * term
   # "The predicate will search for an element in the given tree
      and return the information, defined or undefined.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan_tree(Element,Tree,Info) :-
	get_element_key(Element,Element_key),
	get_tree_key(Tree,Tree_key),
	compare_key_eq(Element_key,Tree_key),
	!,
	get_tree_leaf(Tree,Leaf),
	get_leaf_type(Leaf,Leaf_type),
	get_element_type(Element,Element_type),
	(Leaf_type == Element_type
	->
	get_element_dic(Element,Dic),
	get_leaf_information(Leaf,Dic),
	Info = defined
	;
	scan_correct_leaf(Element,Leaf,Info)).

%Traversing to find the post
scan_tree(Element,Tree,Info) :-
	get_element_key(Element,Key),
	change_direction(Key,Tree,Branch),
	scan_tree(Element,Branch,Info).

%The post could not be found
scan_correct_leaf(Element,Leaf,Info) :-
	get_next_leaf(Leaf,Next),
	(Next == last
	->
	Info = undefined
	;
	get_leaf_type(Leaf,Leaf_type),
	get_element_type(Element,Element_type),
	(Leaf_type == Element_type
	->
	get_element_dic(Element,Dic),
	get_leaf_information(Leaf,Dic),
	Info = defined
	;
	scan_correct_leaf(Element,Leaf,Info))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred merge_tree(+Tree,+Tree)
   :: tree * tree
   # "The predicate can be used for adding a tree dictionary to another
      one (the second). It will remove equal posts but posts with a slight 
      difference will be inserted. The resulting tree will be the second
      tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_tree(Tree0,Tree1) :-
	get_all_leafs(Tree0,Leafs),
	make_elements_of_leafs(Leafs,Elements),
	insert_elements(Elements,Tree1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred gather_leafs(+Leaf,-Leafs)
   :: leaf * list(leaf)
   # "The predicate will return a list of all the leafs in a leaf. That 
      is all the values in a leaf, they have the same keyvalue but different
      types.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gather_leafs(Leaf,Leafs) :-
	is_leaf(Leaf),
	get_next_leaf(Leaf,Rest),
	(  Rest == last
	-> Leafs = [Leaf]
	;  gather_leafs(Rest,More),
	   Leafs = [Leaf|More]
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_all_leafs(+Tree, -ListOfLeafs)
   :: tree * list(leaf)
   # "This predicate will collect all the leafs in the dictionary tree
      to a list.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_leafs(Tree,Leafs) :-
	is_tree(Tree),
	get_tree_leaf(Tree,TreeLeaf),
	gather_leafs(TreeLeaf,TreeLeafs),
	(  got_left_tree(Tree)
	-> get_left_tree(Tree,Left),
	   get_all_leafs(Left,LeftLeafs)
	;  LeftLeafs = []
	),
	(  got_right_tree(Tree)
	-> get_right_tree(Tree,Right),
	   get_all_leafs(Right,RightLeafs)
	;  RightLeafs = []
	),
	append(TreeLeafs,LeftLeafs,L0),
	append(L0,RightLeafs,Leafs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred make_elements_of_leafs(+ListOfLeafs,-ListOfElements)
   :: list(leaf) * list(element)
   # "The predicate changes the leaf structures to element structures.
      This is because we want to have them when inserting.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_elements_of_leafs([],[]).
make_elements_of_leafs([Leaf|Rest_of_leafs],[Element|Rest_of_elements]) :-
	create_leaf(Element,Leaf),
	make_elements_of_leafs(Rest_of_leafs,Rest_of_elements).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred insert_elements(+ListOfElements,+Tree)
   :: list(element) * tree
   # "The predicate will insert all the elements in the list in the
      tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_elements([],_Tree).
insert_elements([E|Rest],Tree) :-
	dictionary_insert(E,Tree,_Info),
	insert_elements(Rest,Tree).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%These predicates are not used.

compare_number_eq(K0,K1) :-
	number(K0),
	number(K1),
	k0 == K1.

compare_number_leq(K0,K1) :-
	number(K0),
	number(K1),
	K0 =< K1.

compare_number_lt(K0,K1) :-
	number(K0),
	number(K1),
	K0 < K1.

compare_number_gt(K0,K1) :-
	number(K0),
	number(K1),
	K0 > K1.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_list_eq(+Name,+Name)
   :: atm * atm
   # "The two names are equal.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_list_eq(K0,K1) :-
	K0 == K1.
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_list_leq(+Name,+Name)
   :: atm * atm
   # "The predicate will check that the names not are variables
      and then change the atom to a list of ascii and then compare.
      Is true if the first name is less or equal to the second.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_list_leq(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_leq(K0_list,K1_list).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_list_lt(+Name,+Name)
   :: atm * atm
   # "The predicate will check that the names not are variables
      and then change the atom to a list of ascii and then compare.
      Is true if the first name is less then the second.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_list_lt(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_lt(K0_list,K1_list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_list_gt(+Name,+Name)
   :: atm * atm
   # "The predicate will check that the names not are variables
      and then change the atom to a list of ascii and then compare.
      Is true if the first name is greater then the second.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_list_gt(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_gt(K0_list,K1_list).
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_eq(+List,+List)
   :: list(num) * list(num)
   # "The predicate will check so that the lists contain the same code 
      values.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_eq([],[]).
list_eq([S|R0],[S|R1]) :-
	list_eq(R0,R1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_leq(+List,+List)
   :: list(num) * list(num)
   # "The predicate will check so that the lists contain the same code 
      values or that the first one has lower value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_leq(L0,L1) :-
	list_gt(L1,L0).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_lt(+List,+List)
   :: list(num) * list(num)
   # "The predicate will check so that the first list contain
      a lower value then the second.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_lt([],[_]).
list_lt([S0|_],[S1|_]) :-
	S0 < S1.

list_lt([S|R0],[S|R1]) :-
	list_lt(R0,R1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred list_gt(+List,+List)
   :: list(num) * list(num)
   # "The predicate will check so that the second list contain 
      a higher value then the first one.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_gt([_],[]).
list_gt([S0|_],[S1|_]) :-
	S0 >= S1.

list_gt([S|R0],[S|R1]) :-
	list_gt(R0,R1).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred test(+Tree,+Tree)
   :: tree * tree
   # "The predicate is a check for the merge predicate.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(A0,B0) :-
	create_tree(A0),
	create_tree(B0),
	dictionary_insert(a,typa,a,A0,_),
	dictionary_insert(b,typa,b,A0,_),
	dictionary_insert(b,typa,bb,A0,_),
	dictionary_insert(a,typa,a,A0,_),
	dictionary_insert(c,typa,a,A0,_),
	dictionary_insert(c,typc,a,A0,_),
	dictionary_insert(d,typd,a,A0,_),

	dictionary_insert(a,typa,a,B0,_),
	dictionary_insert(b,typa,b,B0,_),
	dictionary_insert(b,typa,bb,B0,_),
	dictionary_insert(a,typa,a,B0,_),
	dictionary_insert(c,typa,a,B0,_),
	dictionary_insert(e,type,e,B0,_),

	merge_tree(A0,B0).
*/
