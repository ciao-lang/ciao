# Notes on Ciao active modules

This is an improved version of Ciao active modules, which includes
new features:

 - distribution nodes (a runtime running on a OS process) may run many
   active module instances

 - better support for active module instances (each of them can be
   regarded as actor-based process with an implicit handler
   dispatching exported module predicates)

 - support for lightweight concurrency (even without OS threads)

--jfmc

# HOW TO TEST IT?

  See `actmod` directory at the `testsuite` bundle.

- tests/ directory

  It contains some development scripts to test different actmod
  features. Some of the tests require running from a terminal emulator
  able to run 'tmux' (on macOS install with 'brew install tmux').

# (OLDER NOTES)

-> In filebased_common, change the directory in the fact common_directory/1
   to point to a directory readable system wide (typically in NFS).

-> In webbased_common, change the fact server_public_address/2 to an
   address reachable by httpd. See the manual.
