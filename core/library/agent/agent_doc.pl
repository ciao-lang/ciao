
:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Agents").
:- doc(author,"Francisco Bueno").

% Has to go here or wrong title is generated (MH)
:- include(agent_ops).

:- doc(bug,"Currently, the agent has to be compiled explicitely as an
     active module. The same protocol than in the agent source code must be
     used for this. Automatic compilation is not working.").
:- doc(bug,"It seems that there are running-ahead problems with threads
     that prevent to correctly publish agent addresses, sometimes.").

:- doc(module,"An agent is an active module which has a main execution
     thread. Simultaneously (i.e., in concurrent execution with the main thread), 
     the agent receives messages from other agents, which trigger the
     execution of a predicate by the name of the message.
     Messages can also be sent to other agents, by calling the predicate by
     the name of the message in the context of the receiver agent (see
     @tt{::/2} below).
     Agents are identified by name. The name of an agent is usually the name of 
     its (main) file, but this depends on the protocol used (see
     @tt{protocol/1} below).

     A simple agent that sends inform messages and at the same time receives
     them from other agents, answering back ok, will look like:
@begin{verbatim}
:- agent(simple,[inform/2,ok/1]).
:- protocol('actmods/filebased').

agent :-
    repeat,
    display('Agent id:message?- '), read(Agent:Mess),
    Agent::inform(Mess),
    fail.

inform(Agent,Mess):-
    display(Agent), display(' has sent: ', display(Mess), nl,
    Agent::ok.

ok(_Agent).
@end{verbatim}
").
:- doc(usage,"
@begin{verbatim}
:- agent(AgentName,[Message|...]).
@end{verbatim}
for the main file of the agent.
@begin{verbatim}
:- use_module(library(agent/agent_call)).
@end{verbatim}
for the rest of modules of the agent that need to send messages.
").

:- pred agent
    # "(user defined) is the main thread of the agent.".

:- doc(protocol/1,"A protocol is formed by a pair of modules which allow
    to locate connection addresses of agents. By convention, the names of these
    modules have a common prefix, which makes reference to the protocol, and
    have suffixes '_locate' and '_publish'. The 'publish' part of the protocol 
    must define a multifile predicate @code{save_addr_actmod/1} and the 'locate' 
    part export a predicate @code{module_address/2}. The first one publishes an
    agent address; the second one locates the address of an agent. Together, 
    both make it possible for agents to send and receive messages. All agents 
    in a multi-agent system must therefore use the same protocol. Upon compilation,
    they will then be (automatically) instrumented as active modules under the
    corresponding rendezvous method.").
:- decl protocol(Protocol)
    # "@var{Protocol} is the prefix to a library path where an active module
       rendezvous protocol can be found.".

:- multifile save_addr_actmod/1.
:- pred save_addr_actmod(Address)
    # "(protocol defined) publishes the agent's @var{Address}.".

:- doc(doinclude,module_address/2).
:- pred module_address(Agent,Address)
    # "(protocol defined) gives the @var{Address} of @var{Agent}.".

:- doc(doinclude,:: /2).
:- doc(doinclude,self/1).

:- doc(appendix,"This package is intended as a sample of how to program
    agents in Ciao, based on active modules. It probably lacks 
    many features that an agent might need. In particular, it lacks
    language-independence: it is thought for multi-agent systems where all
    agents are programmed in Ciao.

    You are welcome to add any feature that you may be missing!

@subsection{Platforms}

A platform is an active module which holds connection addresses of agents in
a multi-agent system. A protocol is provided which enables the use of platforms:
@tt{agent/platformbased}. A suitable platform must be up and running when
agents which run under this protocol are started up. The host id and port number 
(IP address/socket number)
of the platform must then be given as arguments to the agents executables. 
The protocol also allows to give an agent name to the agent upon start-up.
A module suitable for a platform can be found in 
@file{library(actmods/examples/webbased_server/webbased_server)}.
").

