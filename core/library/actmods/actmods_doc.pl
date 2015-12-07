:- use_package(assertions).

:- doc(nodoc,assertions).

:- doc(title,"Active modules (high-level distributed execution)").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Daniel Cabeza").

:- doc(bug,"The package provides no means for security: the accessing
	application must take care of this (?).").

:- doc(bug,"It can happen that there is a unique process for an 
	active module serving calls from several different simultaneous
	executions of the same application (or even different applications).
        In this case, there might be
	unwanted interactions (e.g., if the active module has state).").

:- doc(bug,"Applications may fail if the name server or an active module
	is restarted during execution of the application (since they might
	restart at a different port than the one cached by the application).").

:- doc(bug,"One may want name servers to reside at a fixed and known
	machine and port number (this is known as a @em{service} and is
	defined in @tt{/etc/services} in a Unix machine).").

:- doc(module,"

Active modules @cite{ciao-dis-impl-parimp-www} provide a high-level
model of @concept{inter-process communication} and
@concept{distributed execution} (note that this is also possible using
Ciao's communication and concurrency primitives, such as sockets,
concurrent predicates, etc., but at a lower level of abstraction).  An
@index{active module} (or an @index{active object}) is an ordinary
module to which computational resources are attached, and which
resides at a given location on the network.  Compiling an active
module produces an executable which, when running, acts as a
@em{server} for a number of predicates: the predicates exported by the
module. Predicates exported by an active module can be accessed by a
program on the network by simply ``using'' the module, which then
imports such ``remote predicates.''  The process of ``using'' an
active module does not involve transferring any code, but rather
setting up things so that calls in the module using the active module
are executed as remote procedure calls to the active module. This
occurs in the same way independently of whether the active module and
the using module are in the same machine or in different machines
across the network.

Except for having to compile it in a special way (see below), an active
module is identical from the programmer point of view to an ordinary
module. A program using an active module imports it and uses it in the
same way as any other module, except that it uses
``@decl{use_active_module}'' rather than ``@decl{use_module}'' (see
below). Also, an active module has an address (network address) which
must be known in order to use it.  In order to use an active module it
is necessary to know its address: different ``protocols'' are provided for
this purpose (see below).
@footnote{It is also possible to provide active modules via a WWW
address.  However, we find it more straightforward to simply use
socket addresses. In any case, this is generally hidden inside the
access method and can be thus made transparent to the user.}

From the implementation point of view, active modules are essentially
daemons: executables which are started as independent processes at the
operating system level.  Communication with active modules is
implemented using sockets (thus, the address of an active module is an
IP socket address in a particular machine).  Requests to execute goals
in the module are sent through the socket by remote programs.  When
such a request arrives, the process running the active module takes it
and executes it, returning through the socket the computed
answers. These results are then taken and used by the remote
processes. Backtracking over such remote calls works as usual and
transparently.  The only limitation (this may change in the future,
but it is currently done for efficiency reasons) is that all
alternative answers are precomputed (and cached) upon the first call
to an active module and thus @em{an active module should not export a
predicate which has an infinite number of answers}.

  The first thing to do is to select a method whereby the client(s)
  (the module(s) that will use the active module) can find out in
  which machine/port (IP address/socket number) the server (i.e., the
  active module) will be listening once started, i.e., a ``protocol''
  to communicate with the active module.  The easiest way to
  do this is to make use of the rendezvous methods which are provided in the
  Ciao distribution in the @tt{library/actmods} directory; currently,
  @tt{tmpbased...}, @tt{filebased...}, @tt{webbased...}, and
  @tt{platformbased...}. 

  The first one is based on saving the IP address and socket number of the
  server in a file in a predefined directory (generally @tt{/tmp}, but
  this can be changed by changing @tt{tmpbased_common.pl}).

  The second one is similar but saves the info in the directory in
  which the server is started (as @em{<module_name>}@tt{.addr}), or in the
  directory that a @tt{.addr} file, if it exists, specifies. The
  clients must be started in the same directory (or have access to a
  file @tt{.addr} specifying the same directory). However, they can be
  started in different machines, provided this directory is shared
  (e.g., by NFS or Samba), or the file can be moved to an appropriate
  directory on a different machine --provided the full path is the same.

  The third one is based on a @concept{name server} for active modules.
  When an active module is started, it communicates its address to the
  name server. When the client of the active module wants to communicate with
  it, it asks the name server the active module address. This is all done
  transparently to the user.
  The name server must be running when the active module is started (and,
  of course, when the application using it is executed). The location
  of the name server for an application must be specified in an application
  file named @tt{webbased_common.pl} (see Section 3.1 below).

  The fourth one is also based on a name server, but the address of the
  name server is given as a parameter to the active modules when started.

  The rendezvous methods (or protocols) are encoded in two modules: a first
  one, called @tt{...publish.pl}, is used by the server to publish its
  info. The second one, called @tt{...locate.pl}, is used by the
  client(s) to locate the server info. For efficiency, the client
  methods maintain a cache of addresses, so that the server
  information only needs to be read from the file system the first
  time the active module is accessed.

  Active modules are compiled using the @tt{-a} option of the Ciao
  compiler (this can also be done from the interactive top-level shell
  using @pred{make_actmod/2}). For example, issuing the following
  command:

  @begin{verbatim}
  ciaoc -a 'actmods/filebased_publish' simple_server
  @end{verbatim}

  compiles the simple server example that comes with the distribution
  (in the @tt{actmods/example} directory). The 
  @tt{simple_client_with_main} example (in the same directory) can be
  compiled as usual:

  @begin{verbatim}
  ciaoc simple_client_with_main
  @end{verbatim}

  Note that the client uses the @tt{actmods} package, specifies the 
  rendezvous method by importing @tt{library(actmods/filebased_locate)},
  and explicitely imports the ``remote'' predicates (@em{implicit imports
  will not work}). Each module using the @tt{actmods} package @em{should
  only use one of the rendezvous methods}.

  Now, if the server is running (e.g., @tt{simple_server &} in Unix or
  double-clicking on it in Win32) when the client is executed it will
  connect with the server to access the predicate(s) that it imports
  from it.

  A simpler even client @file{simple_client.pl} can be loaded into the
  top level and its predicates called as usual (and they will connect
  with the server if it is running).

  @section{Active modules as agents}

  It is rather easy to turn Ciao active modules into agents for some kind
  of applications. The directory @tt{examples/agents} contains a
  (hopefully) self-explanatory example.

").

:- decl use_active_module(AModule,Imports) : sourcename * list(predname)

        # "Specifies that this code imports from the @em{active
          module} defined in @var{AModule} the predicates in
          @var{Imports}.  The imported predicates must be exported by
          the active module. ".

:- doc(appendix,"The protocols @tt{webbased} and @tt{platformbased}
  are described in this section with a bit more detail.

  @section{Active module name servers (webbased protocol)}

  An application using a name server for active modules must have a file
  named @tt{webbased_common.pl} that specifies where the name server
  resides. It must have the @tt{URL} and the path which corresponds to
  that @tt{URL} in the file system of the server machine (the one that 
  hosts the @tt{URL}) of the file that will hold the name server address.

  The current distribution provides a file @tt{webbased_common.pl} that
  can be used (after proper setting of its contents) for a server of
  active modules for a whole installation. Alternatively, particular 
  servers for each application can be set up (see below).

  The current distribution also provides a module that can be used as
  name server by any application. It is in
  file @file{examples/webbased_server/webbased_server.pl}.

  To set up a name server edit @tt{webbased_common.pl} to change its
  contents appropriately as described above (@tt{URL} and corresponding
  complete file path). Then recompile this library module:
  @begin{verbatim}
    ciaoc -c webbased_common
  @end{verbatim}
  The name server has to be compiled as an active module itself:
  @begin{verbatim}
    ciaoc -a actmods/webserver_publish webbased_server
  @end{verbatim}
  It has to be started in the server machine
  before the application and its active modules are compiled.

  Alternatively, you can copy @tt{webbased_common.pl} and use it to set
  up name servers for particular applications. Currently, this is a bit
  complicated. You have to ensure that the name server, the application
  program, and all its active modules are compiled and executed with
  the same @tt{webbased_common.pl} module. One way to do this is to
  create a subdirectory @tt{actmods} under the directory of your application,
  copy @tt{webbased_common.pl} to it, modify it, and then compile the
  name server, the application program, and its active modules using a
  library path that guarantees that your @tt{actmods} directory is located
  by the compiler before the standard Ciao library. The same applies for
  when running all of them if the library loading is dynamic.

  One way to do the above is using the @tt{-u} compiler option. Assume the
  following file:
  @begin{verbatim}
     :- module(paths,[],[]).
     :- multifile library_directory/1.
     :- dynamic library_directory/1.
     :- initialization(asserta_fact(
	library_directory('/root/path/to/my/particular/application') )).
  @end{verbatim}
  then you have file @tt{webbased_common.pl} in a subdirectory @tt{actmods}
  of the above cited path. You have to compile the name server, the active
  modules, and the rest of the application with:
  @begin{verbatim}
    ciaoc -u paths -s ...
  @end{verbatim}
  to use your particular @tt{webbased_common.pl} and to make executables
  statically link libraries. If they are dynamic, then you have to provide
  for the above library_directory path to be set up upon execution. This
  can be done, for example, by including module @tt{paths} into your
  executables.

  Addresses of active modules are saved by the name server in a subdirectory
  @tt{webbased_db} of the directory where you start it
  ---see @tt{examples/webbased_server/webbased_db/webbased_server}).
  This allows to restart the server 
  right away if it dies (since it saves its state).
  This directory should be cleaned up regularly
  of addresses of active modules which are no more active. To do this, stop
  the server ---by killing it (its pid is in @tt{PATH/FILE}), and restart it
  after cleaning up the files in the above mentioned directory.

  @section{Platforms (platformbased protocol)}

  This protocol is also based on a name server. There are, however, two
  differences with the above one: the name server address and the active
  modules names are dynamic. On the one hand, the name server address
  (IP address/socket number) is given to the active modules when they are 
  started up. This might be convenient when using the same name server
  executable for different applications starting up a different name server
  process for each application. On the other hand, the name assigned to a given
  active module can also be given as a parameter to the active module
  when it is started up. This makes it easier to maintain a local name
  space for particular applications (e.g., two modules with the same name 
  can be used as active modules in the same application).

  The code of a name server for the previous section protocol can also be
  used for this protocol (e.g.,
  file @file{examples/webbased_server/webbased_server.pl}).
").
