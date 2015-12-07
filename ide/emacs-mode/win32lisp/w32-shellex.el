;;; w32-shellex.el --- Windows 95/NT Explorer Shell-based program/file launching

;; Author: Theodore A. Jump (tjump@cais.com)
;; Maintainer: taj / http://www.i21.com/~tjump
;; Keywords: NTemacs

(defconst w32-shellex-version "$VER: 1.6.1 (Fri, Dec 11 1998, 07:10 EST)"
  "*w32-print.el version information
")

;  1.6.1 12/11/98 Minor fixups and byte-compiling cleanups
;  1.6 4/25/98 Renamed to be in line with other w32 packages
;              Minor fixups for better compatability and reasonable defaults.
;              Added w32-shellex-version value.
;              Added code from <cstacy@pilgrim.com> which provided better
;              interactive support. Changed slightly from his implementation
;              to support use of the minibuffer completion routines for
;              interactively selecting a file to 'shellex'.
;  1.5 4/19/98 Some tweaks to better work with directory names with spaces
;              contained within them. Better handle being handed a list of
;              objects from dired.
;  1.3 2/20/98 Changed to use the 'START' command as part of Win95/WinNT
;              rather than the external program shellex.exe which is no
;              longer needed, but still provided just in case.
;  1.2 2/5/98 Fix to shell-execute-file provided by Walt Beuhring incorporated
;  1.4 2/27/98 Fix for VM provided by Chris McMahan <cmcmahan@Teknowledge.COM>
;  1.1 2/4/98 Fixed process startup so that it does not hang on Win95
;  1.0 2/3/98 Initial release

;; ORIGINAL CODE by: Caleb Deupree

;;{{{ GPL

;; This file is NOT part of GNU Emacs, but is for USE with NTemacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file GPL.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;}}}

;;{{{ Commentary

;; This file attempts to make it easy to use file associations in the MS Windows
;; Explorer easy. The original work for this was done by Caleb Deupree and I
;; just took it a bit further.
;;
;; At the end of this elisp file you will find two thing, the source to the
;; shellex.exe module, and the contents of a filetype.reg file that I have
;; created. I personally had a lot of difficulty starting up Netscape
;; Communicator via the Explorer (not just via this package but also in general)
;; and have through grain pains come up with a configuration that works clean
;; for me under both NT (5b1) and '95 (OSR2).  I make no guarantees that it'll
;; work for you and I would definately advise saving your current settings
;; before using mine, but here they are if you want them.

;;}}}

;;{{{ Installation

;; (optional)Put SHELLEX.EXE some place where NTemacs can find it, and put this
;; file (w32-shellex.el) some place in your load path.

;; Usage:
;;
;; Put either of these two lines in your .emacs:
;;
;;    (load "w32-shellex")
;;    (require 'w32-shellex)
;;
;; If you do nothing else, this will create two global key mappings:
;;
;; ^C X F   --> calls "shell-execute-file"
;; ^C X B   --> calls "shell-execute-buffer"
;;
;; and will "enhance" dired-mode to include
;;
;; w32-shellex.el also inserts a hook that works with dired.el such that when
;; you're in a dired buffer you may press ^X^F (which normally calls find-file)
;; to call shell-execute-file.  I find this convenient, I hope you do. You may
;; disable this by setting w32-shellex-no-dired-hook to 't' before loading.
;;
;; Lastly, the browse-url "browse-url-browser-function" is set to
;; shell-execute-object so that WWW, FTP, etc., will work as specified in your
;; system registry. You may disable this by setting w32-shellex-no-browse-url to
;; 't' before loading. The VM and GNUS vars are also configured for you
;; automatically in line with how you've specified the browse-url var.
;;

;;}}}

;;{{{ shellex.exe source

(defvar w32-shellex-dot-exe-source
  "
#include <stdio.h>
#include <windows.h>
#include <shellapi.h>

/* Don't forget to link with shell32.lib */

int __cdecl main (int argc, char **argv)
{
   if (argc == 2)
   {
      return (int) ShellExecute(0, 0, argv[1], 0, 0, SW_NORMAL);
   }
   else
   {
      return 0;
   }
}
")

;;}}}

;;{{{ shellex.reg source

;;--------------------------------------------------------------------
;; source for shellex.reg follows:
;;--------------------------------------------------------------------
;;
;; IMPORTANT NOTE
;;
;; The following assumes that your copy of Netscape Communicator is
;; installed such that the full path netscape.exe is as follows:
;;
;; D:\Program Files\Netscape\Communicator\Program\Netscape.exe
;;
;; You *will* want to fix this for your own system before using this
;; file.
;;
;; The first line of shellex.reg is the one containing "REGEDIT4" and
;; is required for it to be recognized properly for loading.
;;
;; To use, save the contents of this section in a separate file and
;; remove all the elisp comment tags for each line, then double-click
;; within Explorer, and I would suggest then rebooting your machine.
;;

; REGEDIT4
;
; [HKEY_CLASSES_ROOT\file]
; @="URL:File Protocol"
;
; [HKEY_CLASSES_ROOT\file\DefaultIcon]
; @="D:\\Progra~1\\Netscape\\Commun~1\\Program\\Netscape.exe,8"
;
; [HKEY_CLASSES_ROOT\file\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\file\shell\open]
;
; [HKEY_CLASSES_ROOT\file\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\file]
; @="URL:File Protocol"
;
; [HKEY_CLASSES_ROOT\file\DefaultIcon]
; @="D:\\Progra~1\\Netscape\\Commun~1\\Program\\Netscape.exe,8"
;
; [HKEY_CLASSES_ROOT\file\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\file\shell\open]
;
; [HKEY_CLASSES_ROOT\file\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\file\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\.htm]
; @="htmlfile"
; "Content Type"="text/html"
;
; [HKEY_CLASSES_ROOT\.htm\ShellNew]
; "FileName"="netscape.html"
;
; [HKEY_CLASSES_ROOT\.html]
; @="htmlfile"
; "Content Type"="text/html"
;
; [HKEY_CLASSES_ROOT\.html\ShellNew]
; "FileName"="netscape.html"
;
; [HKEY_CLASSES_ROOT\ftp]
; @="URL:File Transfer Protocol"
;
; [HKEY_CLASSES_ROOT\ftp\DefaultIcon]
; @="D:\\Progra~1\\Netscape\\Commun~1\\Program\\Netscape.exe,8"
;
; [HKEY_CLASSES_ROOT\ftp\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\ftp\shell\open]
;
; [HKEY_CLASSES_ROOT\ftp\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\ftp\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\ftp\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\ftp\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\gopher]
; @="URL:Gopher Protocol"
;
; [HKEY_CLASSES_ROOT\gopher\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,4"
;
; [HKEY_CLASSES_ROOT\gopher\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\gopher\shell\open]
;
; [HKEY_CLASSES_ROOT\gopher\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\gopher\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\gopher\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\gopher\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\htmlfile]
; @="HTML Document"
;
; [HKEY_CLASSES_ROOT\htmlfile\DefaultIcon]
; @="D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe,2"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell]
; @="open"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\edit]
; "EditFlags"=hex:01,00,00,00
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\edit\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" -edit \"%1\""
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\open]
; @="&Open"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\open\ddeexec]
; @="\"%1\",,0x0"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\Open_in_Same_Window]
; @="Open in Same Window"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\Open_in_Same_Window\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\Program\\Netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\Open_in_Same_Window\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\Open_in_Same_Window\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\htmlfile\shell\Open_in_Same_Window\ddeexec\topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\http]
; @="URL:HyperText Transfer Protocol"
;
; [HKEY_CLASSES_ROOT\http\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,6"
;
; [HKEY_CLASSES_ROOT\http\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\http\shell\open]
; "EditFlags"=hex:01,00,00,00
;
; [HKEY_CLASSES_ROOT\http\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\http\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\http\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\http\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\https]
; @="URL:HyperText Transfer Protocol with Privacy"
;
; [HKEY_CLASSES_ROOT\https\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,6"
;
; [HKEY_CLASSES_ROOT\https\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\https\shell\open]
; "EditFlags"=hex:01,00,00,00
;
; [HKEY_CLASSES_ROOT\https\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\https\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
; "NoActivateHandler"=""
;
; [HKEY_CLASSES_ROOT\https\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\https\shell\open\ddeexec\Topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\mailto]
; @="URL:MailTo Protocol"
; "EditFlags"=hex:02,00,01,00
; "URL Protocol"=""
;
; [HKEY_CLASSES_ROOT\mailto\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,11"
;
; [HKEY_CLASSES_ROOT\mailto\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\mailto\shell\open]
; "EditFlags"=hex:01,00,00,00
;
; [HKEY_CLASSES_ROOT\mailto\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\mailto\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
;
; [HKEY_CLASSES_ROOT\mailto\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\mailto\shell\open\ddeexec\topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\news]
; @="URL:News Protocol"
;
; [HKEY_CLASSES_ROOT\news\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,10"
;
; [HKEY_CLASSES_ROOT\news\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\news\shell\open]
;
; [HKEY_CLASSES_ROOT\news\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\news\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
;
; [HKEY_CLASSES_ROOT\news\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\news\shell\open\ddeexec\topic]
; @="WWW_OpenURL"
;
; [HKEY_CLASSES_ROOT\nntp]
; @="URL:NNTP Protocol"
;
; [HKEY_CLASSES_ROOT\nntp\DefaultIcon]
; @="d:\\progra~1\\netscape\\commun~1\\program\\netscape.exe,10"
;
; [HKEY_CLASSES_ROOT\nntp\shell]
; @=""
;
; [HKEY_CLASSES_ROOT\nntp\shell\open]
;
; [HKEY_CLASSES_ROOT\nntp\shell\open\command]
; @="\"D:\\Progra~1\\Netscape\\Commun~1\program\\netscape.exe\" %1"
;
; [HKEY_CLASSES_ROOT\nntp\shell\open\ddeexec]
; @="\"%1\",,0xFFFFFFFF"
;
; [HKEY_CLASSES_ROOT\nntp\shell\open\ddeexec\Application]
; @="NSShell"
;
; [HKEY_CLASSES_ROOT\nntp\shell\open\ddeexec\topic]
; @="WWW_OpenURL"Tee

;;}}}

;;{{{ Prologue

(if (not (eq system-type 'windows-nt))
	(error "w32-shellex.el: Only supports Windows95/98 or Windows NT"))

(eval-when-compile
  (require 'dired)
  (require 'browse-url))

;;}}}

;;{{{ Module control vars

(defvar w32-shellex-use-start-cmd (and (getenv "OS") (string-equal (getenv"OS") "Windows_NT"))
  "When t, uses the \"START\" command rather than the external shellex.exe.
While this author can see no reason why one would need to use the external program
instead of START by setting this variable to nil you can force this behavior.

The one proven exception to this seems to be Windows95, where directly executing START
does not seem to generate the required affect.
"
  )

(defvar w32-shellex-no-dired-hook nil
  "When t, specifies to NOT hook shellex.el into the dired keymap"
  )

(defvar w32-shellex-no-browse-url-hook nil
  "When t, specifies to NOT configure browse-url-browser-function to use shellex.el."
  )

(defvar w32-shellex-no-vm-hook nil
  "When t, specifies to NOT configure vm-url-browser to use shellex.el."
  )

(defvar w32-shellex-execute-helper "shellex.exe"
  "Program to call to initiate Shell Execution of a URL or file on Win95/WinNT"
  )

;;}}}

;;{{{ Keymapping support

; hookup the standard keystrokes

(eval-after-load
 "dired"
 '(cond ((not (eq w32-shellex-no-dired-hook t))
		 (define-key dired-mode-map "j" 'w32-shellex-dired-on-objects)
		 (define-key dired-mode-map "\C-x\C-f" 'w32-shellex-on-file)
		 ))
 )

(eval-after-load
 "browse-url"
 '(cond ((not (eq w32-shellex-no-browse-url-hook t))
		 (setq-default browse-url-browser-function 'w32-shellex-on-object)
		 ))
 )

(eval-after-load
 "vm"
 '(cond ((not (eq w32-shellex-no-vm-hook t))
		 (setq-default vm-url-browser 'w32-shellex-on-object)
		 ))
 )

;;}}}

;;{{{ Functional code

(defun w32-shellex-unexpand-file-name (path)
  "Replace unix-style directory separator character with dos/windows one.

This function found originally in print-NT.el by (), and provided to him
by Marc P. Kwiatkowski <marc@mpath.com>.

This function can be used as a mirror to the NTemacs standard function
\"expand-file-name\".
"
  (interactive "sPath: ")
  (if (memq system-type '(windows-nt))
      (mapconcat '(lambda(x) (char-to-string (if (eq x ?/) ?\\ x))) path nil)
    path)
  )

(defun w32-shellex-on-object (object &optional new-window)
  "Invoke the shell-execute-helper program to call ShellExecute
and launch or re-direct a file, program, or URL.

Note: If you're having problems launching things, e.g.: you get an error
message like \"Searching for program: no such file or directory, start\"
you may wish to set shellexe-use-start-cmd to nil to instruct this function
to use the external shellex.exe helper code.

Note2: Under windows-nt the START command is issued via shell-command, and
under Windows95 the START command is issued via call-process. The difference is
owing to the fact that it just works a bit differently between the two OS. For
this automatic detection to function properly, the OS variable is checked for
and compared against \"Windows_NT\" - which is set automatically by Windows NT
for you but is not found by default under Windows95.
"
  (interactive "sTarget: ")
  (let ((ori_shell (getenv "SHELL"))
		(ori_sfn shell-file-name))
	(setenv "SHELL" (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
	(setq-default shell-file-name "cmdproxy.exe")
	(if (or (eq w32-shellex-use-start-cmd nil)
			(and (getenv "OS") (string-equal (getenv"OS") "Windows_NT")))
		(call-process w32-shellex-execute-helper nil 0 nil (w32-shellex-unexpand-file-name object))
	  (call-process "start" nil 0 nil (format "\"%s\"" (w32-shellex-unexpand-file-name object)))
	  )
	(setenv "SHELL" ori_shell)
	(setq-default shell-file-name ori_sfn)
	)
  )

(defun w32-shellex-on-buffer (&optional buffer)
  "Ask Explorer to view the current, or specified buffer, by association."
  (interactive)
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
	(if (null buffer-file-name)
		(message "shellex: Can't browse, buffer has no file name associated.")
	  (progn
		(if (and (not (null browse-url-save-file)) (eq browse-url-save-file t))
			(save-buffer))
		(message (format "shellex: %s ..." buffer-file-name))
		(w32-shellex-on-object buffer-file-name)
		)
	  ) ; end if
	) ; end save-excursion
  ) ; end defun

(defun w32-shellex-on-file (&optional arg)
  "Ask Explorer to open the specified file, by association.

Revised code and better interactive support provided by <cstacy@pilgrim.com>
"
  (interactive "fShellEx File: ")
  (let (file)
	(cond ((stringp arg)
		   (setq file arg)
		   )
		  ((and (not (stringp arg)) (boundp 'dired-directory))
		   (setq file dired-directory) ; getting target from dired
		   (save-excursion
			 (dired-move-to-filename)
			 (let ((start (point)))
			   (dired-move-to-end-of-filename)
			   (setq file (concat (dired-current-directory) (buffer-substring start (point))))
			   )
			 )
		   )
		  )
	(message "ShellExing %S" file)
	(w32-shellex-on-object (expand-file-name file))
	)
  )

(defun w32-shellex-dired-on-objects ()
  "Invoke the Explorer on all dired-marked entries"
  (interactive)
  ;; The dired-do-shell-command function in direc-aux.el changed
  ;; from version 19 to 20.  So handle the two cases separately.
  ;; Note that this is the command bound to "!" key in the dired mode.
  (let ((dmf-list (dired-get-marked-files t current-prefix-arg))
		(index 0))
	(while (< index (length dmf-list))
	  (w32-shellex-on-file (nth index dmf-list))
	  (setq index (1+ index))
	  )
	)
  )

; makes sure we're set to autoload browse-url at need
;
(autoload 'browse-url-at-point "browse-url" "" t)
(autoload 'browse-url-of-buffer "browse-url" "" t)
(autoload 'browse-url-of-file "browse-url" "" t)
(autoload 'browse-url-of-dired-file "browse-url" "" t)

(provide 'w32-shellex)

;;}}}
