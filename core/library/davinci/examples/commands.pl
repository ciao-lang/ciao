/*
This file contains some examples for commands which can be sent to daVinci
by using the application in this directory. First you have to connect the 
application to daVinci (?- davinci.), then you can type the commands as an
argument of davinci_put/1. Alternatively you can start a toplevel loop with
?- topd. and type the commands at your terminal. In this case you can type
|: in. to get the output from daVinci printed at your terminal, and ^D to
finish. A goal of davinci_get/1 will also return the output from daVinci.

The commands in this file have been obtained after those in an example file
in daVinci1.4's distribution (by Michael Froehlich, 1994-12-01).
*/

%% Send a new graph in daVinci's term representation format.	 	%%
new_term([l(string('Object A'),
	     n(string('anything'),
		[a(string('OBJECT'),string('Object A'))],
		[e(string('anything'),
		    [],
		    l(string('Object C'),
		       n(string('anything'),
			  [a(string('OBJECT'),string('Object C'))],
			  [e(string('anything'),
			      [],
			      l(string('Object E'),
				 n(string('anything'),
				    [a(string('OBJECT'),string('Object E'))],
				    [e(string('anything'),
				     [],
				     r(string('Object D')))
				    ]
				  )
			       )
			    )
			  ]
			)
		     )
		  ),
		 e(string('anything'),
		    [],
		    l(string('Object B'),
		       n(string('anything'),
		          [a(string('OBJECT'),string('Object B'))],
			  [e(string('anything'),
			      [],
			      l(string('Object D'),
			         n(string('anything'),
				    [a(string('OBJECT'),string('Object D'))],
				    []
				  )
			       )
			    )
			  ]
			)
		     )
		  )
		]
	      )
	   ),
	  l(string('Object F'),
	     n(string('anything'),
	        [a(string('OBJECT'),string('Object F'))],
		[e(string('anything'),[],r(string('Object B')))]
	      )
	   )
	 ]
	).

%% This will create the same graph as above 	%%
new_term([l(string('Object A'),n(string(''),
	                         [a(string('OBJECT'),string('A'))],
				 [e(string(''),[],r(string('Object C'))),
				  e(string(''),[],r(string('Object B')))])),
	  l(string('Object C'),n(string(''),
	                         [a(string('OBJECT'),string('C'))],
				 [e(string(''),[],r(string('Object E')))])),
	  l(string('Object E'),n(string(''),
	                         [a(string('OBJECT'),string('E'))],
				 [e(string(''),[],r(string('Object D')))])),
	  l(string('Object B'),n(string(''),
	                         [a(string('OBJECT'),string('B'))],
				 [e(string(''),[],r(string('Object D')))])),
	  l(string('Object D'),n(string(''),
	                         [a(string('OBJECT'),string('D'))],
				 [])),
	  l(string('Object F'),n(string(''),
	                         [a(string('OBJECT'),string('F'))],
				 [e(string(''),[],r(string('Object B')))]))
	 ]).

%% Send a new graph in Prolog's term representation format (the graph   %%
%% representation of SICStus3's library(ugraphs) is used).	 	%%
%% This will create the same graph as above, since it is exactly what  	%%
%% it gets translated into. 						%%
%% This is NOT a daVinci command.
davinci_ugraph(['A'-['B','C'],'B'-['D'],'C'-['E'],'D'-[],'E'-['D'],'F'-['B']]).

%% Send a labeled graph in Prolog's term representation format (the     %%
%% graph representation of SICStus3's library(wgraphs) is used).	%%
%% This will create the same graph as above, but showing labels in   	%%
%% the edges. 						                %%
%% This is NOT a daVinci command.
davinci_lgraph(['A'-['B'-[from,a,to,b],'C'-ac],'B'-['D'-bd],'C'-['E'-ce],
	        'D'-[],'E'-['D'-ed],'F'-['B'-fb]]).

%% Create an application specific menu in daVinci. Don't do that 	%%
%% twice with the same menu identifiers! 			       	%%
create_menu(submenu_entry(string('applmenu'),string('Application'),
	[menu_entry(string('applmenu A'),string('Command A')),
	 menu_entry(string('applmenu B'),string('Command B')),
	 blank,
	 submenu_entry(string('applmenu_sub'),string('Submenu'),
	   [menu_entry(string('applmenu C'),string('Command C')),
	    menu_entry(string('applmenu D'),string('Command D'))
	   ])
	])).

%% Activate some menu entries in this menu tree (but create it first  	%%
%% with the command above!). 						%%
activate_menu_entries([string('applmenu'),string('applmenu A'),
		       string('applmenu_sub'),string('applmenu C'),
		       string('applmenu D')]).

%% Start a user dialog (a text input). 				%%
%% You can also issue this command with davinci_ask/2.          %%
question_string(string('A question'),
	        [string('Please enter'),string('some text:')],
	        string('Delete this!')).

%% Start a user dialog (a choice).				%%
%% You can also issue this command with davinci_ask/2.          %%
question_boolean(string('Another question'),
		 [string('How do you do?')],
	         string(':->'),
		 string(':-{'),
		 true).

%% Start a user dialog (a confirmation). 			%%
%% This command will simply not work with davinci_put/1, use    %%
%% davinci_ask/2 instead.					%%
confirmer(string('Please confirm'),
	  [string('You have to press'),string('the button')],
	  string('Yes, this one')).

%% Report a message in the left footer. 				%%
show_message(string('Here is a message for you!')).

%% Report a status in the right footer. 				%%
show_status(string('Here is a status')).

%% Load a graph in term representation from a file. Please use a 	%%
%% valid graph filename! 					  	%%
load_graph(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/graph_example.daVinci')).

%% Load a graph in term representation from a file. Please use a 	%%
%% valid graph filename! The graph is automatically placed afterwards.	%%
load_graph_placed(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/graph_example.daVinci')).

%% Load a status of daVinci from a file. Please use a valid status 	%%
%% filename! 			       					%%
load_status(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/xerox_star.status')).

%% Save the current graph in a file (in term representation format). 	%%
save_graph(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/xerox_star_new.daVinci'))

%% Save the status of daVinci in a file. 				%%
save_status(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/xerox_star_new.status'))

%% Save the current graph in a file (in PostScript format). 		%%
save_postscript(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/xerox_star_new.ps'))

%% Set the scale of the visualization to 15%. 				%%
set_scale(15).

%% Start the layout algorithm (edge crossing minimization) for the 	%%
%% whole graph. 					       		%%
place_all_nodes.

%% Start the layout algorithm (edge crossing minimization) only for  	%%
%% the visible subgraph (i.e. all nodes visible in the window). 	%%
place_visible_nodes.

%% Shrink the width of the graph if this is possible. 			%%
compact_graph.

%% Select one or more nodes in the displayed graph. Do this after   	%%
%% loading status file 'xerox_star.status' (see load_status above). 	%%
%% IMPORTANT: The identifiers used in the examples are the labels of 	%%
%% the node in the term representation, not the text of the nodes!	%%
select_nodes_labels([string('Sketchpad'),string('Alto')]).

%% Hide (i.e. collapse) the subgraphs of all currently selected  	%%
%% nodes. The subgraph is shown again if it was hidden before. 	%%
hide_or_show_subgraph.

%% Hide the subgraphs for the nodes in the list. Do this after   	%%
%% loading status file 'xerox_star.status' (see load_status above).	%%
hide_subgraphs([string('Memex')]).

%% Show the subgraphs for the nodes in the list. Do this after   	%%
%% loading status file 'xerox_star.status' (see load_status above). 	%%
show_subgraphs([string('Memex')]).

%% Show all hidden subgraphs. %%
restore_subgraphs.

%% Start the encapsulation operation for all selected nodes, i.e. all	%%
%% the incomming and outgoing edges are hidden (or shown if they were 	%%
%% hidden before).	            					%%
hide_or_show_edges.

%% Show all hidden edges. 						%%
restore_edges.

%% Set the accuracy of the phase one of the crossing minimization 	%%
%% algorithm to the specified value (must be between 1 and 20).  	%%
%% Values greater than 5 are not very useful.                    	%%
set_accuracy_phase_1(3).

%% Set the accuracy of the phase two of the crossing minimization 	%%
%% algorithm to the specified value (must be between 1 and 40).   	%%
set_accuracy_phase_2(8)

%% Set the font size to the specified value (must be one of the 	%%
%% following  values: 6,8,10,12,14,18,24,34).   			%%
set_font_size(34).

%% Set the gap width (minimal distance between two nodes) to the  	%%
%% specified value (between 4 and 300).				%%
set_gap_width(20).

%% Set the gap height (minimal distance between two layers) to the  	%%
%% specified value (between 4 and 300).				%%
set_gap_height(40).

%% Set the gap for multiple edges to the specified value (must be 	%%
%% between 2 and 20). Send command					%%
load_graph(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/multiple_edges.daVinci')).
%% first to see the effect of this command.			   	%%
set_multiple_edge_gap(20).

%% Set the radius for self refering edges to the specified value 	%%
%% (must be between 8 and 30). Send 				  	%%
load_graph(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/self_refering_edges.daVinci')).
%% first to see the effect of this command.			  	%%
set_self_refering_edge_radius(30).

%% Set the title of the visualization window. 				%%
set_window_title(string('My Title')).

%% Set the size of the visualization window. 				%%
set_window_size(400,300).

%% Set the position of the visualization window. 			%%
set_window_position(100,100).

%% Open daVinci, i.e. deiconify it, if it was iconified before. 	%%
open_window.

%% Close daVinci, i.e. iconify it. 					%%
close_window.

%% Lock daVinci, i. e. disable user input. 				%%
deactivation.

%% Unlock daVinci, i. e. enable user input again. 			%%
activation.

%% Quit daVinci.							%%
%% Even if you send this message, you should also use davinci_quit/0,	%%
%% so that the state of the interface is resumed.
quit.

%% !!!Here are the commands for the new features of daVinci V1.4.1!!!	%%

%% First, you have to send the following command to load a graph.	%%
load_status(string('/home/bueno/daVinci-1.4.2/daVinci_V1.4_solaris_1/example_graphs/xerox_star.status')).

%% Change the color of one node without redrawing the whole graph.	%%
%% Only one of the 16 supported color values of daVinci can be used. 	%%
%% They can be viewed with the tool daVincicolors (in the tools/ 	%%
%% directory).								%%
change_node_color(string('Memex'),string('red')).

%% Center a node in the visualization window. Be sure that graph	%%
%% xerox_star.status is loaded (see example above).			%%
focus_node(string('Interleaf')).

%% Do the same with animation (i.e. use a smooth scrolling). The 	%%
%% number (30) is the scrolling speed in pixel per step.		%%
focus_node_animated(30,string('Pygmalion')).
