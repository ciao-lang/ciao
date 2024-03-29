:- use_package([assertions,regtypes]).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).
:- doc(nodoc,regtypes).

:- doc(title, "Active modules").
:- doc(subtitle, "A concurrency model with high-level distributed execution").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Daniel Cabeza (original version before 1.16)").
:- doc(author,"Jose F. Morales (revised version)").

:- doc(stability, devel).

:- doc(module,"

@begin{note}
This version corresponds to a revised model for active modules. See
Ciao version 1.15 for the original first design as described in
@cite{ciao-dis-impl-parimp-www}.
@end{note}

An @index{active module} is an ordinary module whose instances (copies
sharing the same code but different state or data) have computational
resources attached (e.g., computation steps shared in a fair fashion).

Active modules provide a high-level model of concurrency suitable for
@concept{distributed execution} (and @concept{inter-process
communication}) that is similar to @index{active object}s and
@index{actor}s. Note that Ciao also offers lower-level primitives for
concurrency and network-based communication.

@section{Concurrency model and semantics}

Each @index{active module instance} is internally composed of a local
mailbox for queries (a queue of messages) and a query handler loop.
Each active module instance is identified by a unique name (which can
be provided or created automatically).

Calls to exported predicates of an active module are enqueued in the
@concept{mailbox}. The query handler loop is a @bf{deterministic} loop that
executes queries sequentially, sending back the results to the caller
program (another active module instance) if needed. The composition of
the (possibly) multiple answers from the callee and the caller is
given be the @em{query protocols} defined below.

@begin{note} 
Query requests at the handler loop do not fail or leave
choicepoints. When required, non-deterministic behaviour must be
captured on the answer and treated on the callee by the @em{query
protocols}. 
@end{note}

@subsection{Query protocols}

There exist several query protocols depending on the expected answers
of a predicate. 

@begin{itemize}
@item @em{all solutions}: all answers to each query are precomputed on the
  callee side and sent to the caller. Backtracking is supported by
  enumeration on the caller side. @bf{Note}: calls to active module
  predicates with an infinite number of solutions will obviously not
  terminate with this query method.

@item @em{cast}: no answer is required from the callee (equivalent to
  @em{message passing} in distributed computation). It is useful when
  the query performs side-effects or it sends back the answers to the
  caller active module through another cast (a-la continuation-based
  programming).

@item (experimental) @em{answers with suspensions}: the callee may
  return a suspended computation and continue the execution on the
  caller site.  Currently predicates must be declared as
  @tt{suspendable}. The main focus of the current functionality is the
  implementation of @tt{REST}ful applications via the (experimental)
  HTTP interface.  @bf{Note}: support is limited, recommended only for
  deterministic computations.
@end{itemize}

Further details on the semantics and concurrency model:
@begin{itemize}
@item The cost of calls depends on the size of the messages (arguments,
  results, and the target @em{location})
@item Deadlocks may happen due to the \"message processing lock\"
  (e.g., A calls B, B calls A). Use @em{cast} instead.
@end{itemize}

@begin{note}
@begin{itemize}
@item Query protocols will be changed or extended in the future,
  specially to optimize cost for particular cases.
@item Suspendable predicates rely on the experimental @tt{fibers}
  package. This may change in the future.
@item Or-suspensions for lazily asking for more solutions are not
  currently implemented (they are in development).
@end{itemize}
@end{note}

@subsection{Side-effects}

All communication between active module instances should (in
principle) happen through message passing. Instances should not share
any global data. Sharing via dynamic/data predicates (or other global
mechanisms) is seen as an @em{impure} side-effect w.r.t. this model and
must be used with care (e.g., @em{caching}, hand-made
optimizations, etc.).

@subsection{Distributed}

In a distributed setting active module instances may run on separate
@em{node}s that can interchange messages through the network. See
@lib{actmod_dist} for more details about the distribution protocol and
how it can be extended.

@section{Using active modules}

Using active modules requires the use of the @lib{actmod} package:
@begin{verbatim}
:- module(...,...,[actmod]).
@end{verbatim}

This turns the current module into an active module and enables all
the directives and features required to use other active modules.

Predicates exported by an active module can be accessed by other
active modules using the @decl{use_module/3} declaration with the
@tt{active} option (see below).

Note that the process of @em{using} an active module does not involve
transferring any code, but rather setting up things so that calls in
the module using the active module are executed as remote procedure
calls to the active module.

@section{Running active modules}

For spawning active module instances (dynamic creation) see
@lib{actmod_process}.

Active modules may implement a @tt{main/1} predicate, if they want to
receive command-line arguments or use the directive @tt{:- dist_node} to
include a default @tt{main/1} for distributed nodes. See
@lib{actmod_dist} for more details.

@section{Examples}

The following command:

@begin{verbatim}
ciaoc simple_server.pl
@end{verbatim}

@noindent
compiles the simple server example that comes with the distribution
(in the @tt{actmod/example} directory). The
@tt{simple_client_with_main} example (in the same directory) can be
compiled as usual:

@begin{verbatim}
ciaoc simple_client_with_main
@end{verbatim}

Now, if the server is running when the client is executed it will
connect with the server to access the predicate(s) that it imports
from it.

An even simpler client @file{simple_client.pl} can be loaded into the
top level and its predicates called as usual (and they will connect
with the server if it is running).
").

% ---------------------------------------------------------------------------

:- use_module(engine(stream_basic), [sourcename/1]).

:- decl use_module(ModSpec,Imports,Opts) : 
    sourcename * list(predname) * list(import_opt)
   # "Import from @var{ModSpec} the predicates in @var{Imports} with
      options @var{Opts}. If @var{Imports} is a free variable, all
      predicates are imported.".

:- regtype import_opt/1.
:- doc(import_opt(Opt), "Options for @pred{use_module/3}:
   @begin{itemize}
   @item @tt{active}: import as an active module (which adds a
     dependency to the module interface, not its code; it allows
     @pred{actmod_spawn/3} and static named instances).
   @item @tt{reg_protocol(RegProtocol)}: specify default name server protocol.
   @item @tt{libexec}: use @tt{libexec} as spawning option by default.
   @item @tt{binexec(Name)}: use @tt{binexec(Name)} as spawning option by default.
   @end{itemize}").

import_opt(active).
import_opt(reg_protocol(_)).
import_opt(libexec).
import_opt(binexec(_)).

% ---------------------------------------------------------------------------

% (High priority)

:- doc(bug, "Current support for multiple instances from the same
   module is limited and not fully implemented. The default is a
   single instace per OS process, with the module name as active
   module instance name.").

:- doc(bug, "Make sure that active module address is re-lookup on
   broken connections, etc.").

% TODO:T253 add `static` as new option for use_module/3:
%    a single active module instance is reachable with the same
%    name as the module

% TODO:T253 explain .stub.pl: full code is not linked, only a stub
%    (see implementation, requires a .stub.pl file).
:- doc(bug, "Some features for active modules require a @tt{.stub.pl}
   file, containing only the module interface (exported predicate with
   their assertions). This file must be created manually. In future
   versions it will not be needed.").
:- doc(bug, "Option @tt{active} allow arbitrary values in
   @var{ModSpec} and unrestricted interfaces (similar to the
   @tt{import/2} declaration). Use with care.").

% TODO:T253 complete and document 'suspendable'

% (Normal/low priority)

:- doc(bug, "Include some means for security in the distribution
   protocol.").
% E.g., http://erlang.org/doc/apps/ssl/ssl_distribution.html

:- doc(bug, "Document how to run as Unix services (e.g., fixed and
   known machine and port number, start automatically, restart on
   errors).").

