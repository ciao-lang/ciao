;;; winnt.el --- Steves startup file for GNU Epacs on Windows NT>
;;
;;  Startup file for Windows NT.
;;
;;;;;



;;
;; Auto load section.
;;  This allows users to run commands, without the necessary lisp files being loaded.
;; and can save some time.

;;  Format + Read Unix Man pages
(autoload 'woman "woman"
 "Decode and browse a Unix man page." t)
(autoload 'woman-find-file "woman"
 "Find, decode and browse a specific Unix man-page file." t)
;;  Initial value of woman-path.
(setq woman-path (concat (expand-file-name (getenv "HOME")) "/Man_Pages/" ))

;; Allow the browse url package to find my copy of netscape.
(setq browse-url-netscape-program
      "D:/PROGRA~1/NETSCAPE/COMMUN~1/PROGRAM/netscape.exe")



;; Requires section.
;;  The things included here are loaded when emacs starts.  Removing some
;; of them will result in a faster emacs startup time.
;;

;; Some windows specific modes
(require 'generic)


;; Start setup for printibg on Win32, Emacs 20.3.1
(if (and  (>= emacs-major-version 20)
	  (or (eq window-system 'w32) (eq window-system 'win32)))
    (progn (setq printer-name "//peking/lp")
	   (setq ps-printer-name "//peking/lp")))
;; End setup for printing on Win32, Emacs 20.3.1

;;; 
;;
;; GNU Server
;;  Allows files to be presented in an already-running instance of emacs
;; by being associated with gnuclientw.exe.
;;
(require 'gnuserv)
;; Make files appear in the current frame, rather than a new one.
(setq gnuserv-frame (selected-frame))
(gnuserv-start)


;; Allow Emacs to interact with windows 95 / NT
(require 'w32-shellex)
(setq w32-shellex-execute-helper(concat (expand-file-name (getenv "HOME")) "/Binaries/shellex.exe") )



(setq ftp-program (concat (expand-file-name (getenv "HOME")) "/Binaries/ftp.exe"))

;;  Setup ange-ftp, making sure that this is portable!
(setq ange-ftp-ftp-program-name
      (concat (expand-file-name (getenv "windir")) "/System32/ftp.exe"))
(setq ange-ftp-tmp-name-template
      (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
(setq ange-ftp-gateway-tmp-name-template
      (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
;;
;; Now make sure that files written on a server are written in Unix
;; format.
(require 'advice)
(defadvice ange-ftp-real-write-region (around fix-ange-ftp-real-write-region first activate)
  (let ((coding-system-for-write 'no-conversion))
    ad-do-it
    ))
;;;

;; General settings for emacs.
;;

;;  Allow normally disabled functions to be enabled.
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



;; Setup e-mail stuff
(setq user-full-name "Steve Kemp")
(setq user-mail-address "skx@tardis.ed.ac.uk")
(setq smtpmail-default-smtp-server "butterfly.epc.co.uk")
(setq send-mail-function 'smtpmail-send-it)
(load-library "smtpmail")


;; Set up my news server.
;;  This is a public access one, that should be okay for most people.

;;; Specify which backends select new messages and groups.
(setq
 gnus-select-method             '(nntp "news.lockergnome.com")
 )



;;; SUBJECT window display and auto-selection parameters.
(setq
 gnus-large-newsgroup           250
 gnus-auto-select-first         nil
 gnus-auto-select-next          'skip-empty-groups
 gnus-thread-indent-level       4       ; was 2, new gnus is 4 default
 gnus-xmas-force-redisplay      nil     ; expensive to do over slip
 gnus-summary-line-format       "%U%R%z%I%(%[%4L: %-20,20n%]%) %s\n"
 )


;;
;;  Setup fonts
;;
(if window-system
    (progn
      (add-hook 'shell-mode-hook 'turn-on-font-lock)
      (add-hook 'find-file-hooks 'turn-on-font-lock)
      (font-lock-mode 1)
))


;; Bind key "C-x v" to open up view mode in the current buffer.
(global-set-key "\C-xv" 'view-mode)

;; Bind key f3 to the various search forward functions.
(global-set-key [f3] 'isearch-forward)
(define-key esc-map [f3] 'isearch-forward-regexp)
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)

;;  Add some file extensions to ingnore when completing filenames
(setq completion-ignored-extensions
      (cons ".class" completion-ignored-extensions))
(setq completion-ignored-extensions
      (cons ".exe" completion-ignored-extensions))
(setq completion-ignored-extensions
      (cons ".obj" completion-ignored-extensions))


;; Don't attempt to convert the encoding system in use in the buffer.
;; or something.. ;)
(setq default-buffer-file-coding-system 'no-conversion)

;; Hightlight parenthesis, etc.
;; Custom settings.
(custom-set-variables
 '(show-paren-mode t nil (paren))
 '(auto-insert t)
 '(auto-insert-mode t nil (autoinsert))
 '(ispell-highlight t))
(custom-set-faces)



;;
;;  Map the left windows button to meta
;;
(setq w32-alt-is-meta t)
(setq w32-pass-alt-to-system nil) 
(setq w32-pass-lwindow-to-system nil)   
(setq w32-lwindow-modifier 'meta)      ; lwindow acts as hyper
(setq w32-rwindow-modifier t)          ; rwindow is ignored
(setq w32-apps-modifier nil)        

;; From: Kim F. Storm 
;; I use the following little trick to make the numeric keypad keys
;; work as prefix numeric keys inside emacs (the decimal point 
;; works like a negative argument):
;; So to insert 100 x'es enter 100 on the keypad followed by x
;; or to kill the previous 10 characters enter kp-.10 C-d
(define-key function-key-map [kp-0] [27 ?0])
(define-key function-key-map [kp-1] [27 ?1])
(define-key function-key-map [kp-2] [27 ?2])
(define-key function-key-map [kp-3] [27 ?3])
(define-key function-key-map [kp-4] [27 ?4])
(define-key function-key-map [kp-5] [27 ?5])
(define-key function-key-map [kp-6] [27 ?6])
(define-key function-key-map [kp-7] [27 ?7])
(define-key function-key-map [kp-8] [27 ?8])
(define-key function-key-map [kp-9] [27 ?9])
(define-key function-key-map [kp-decimal] [27 ?-])

;;
;; Fix for igrep running under 20.3.1
;;
(defvar grep-null-device null-device)

(autoload (function igrep) "igrep"
 "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload (function igrep-find) "igrep"
 "*Run `grep` via `find`..." t)
(autoload (function dired-do-igrep) "igrep"
 "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload (function dired-do-igrep-find) "igrep"
 "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload (function grep) "igrep"
 "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
(autoload (function egrep) "igrep"
 "*Run `egrep`..." t)
(autoload (function fgrep) "igrep"
 "*Run `fgrep`..." t)
(autoload (function agrep) "igrep"
 "*Run `agrep`..." t)
(autoload (function grep-find) "igrep"
 "*Run `grep` via `find`..." t)
(autoload (function egrep-find) "igrep"
 "*Run `egrep` via `find`..." t)
(autoload (function fgrep-find) "igrep"
 "*Run `fgrep` via `find`..." t)
(autoload (function agrep-find) "igrep"
 "*Run `agrep` via `find`..." t)
(autoload (function dired-do-grep) "igrep"
 "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload (function dired-do-grep-find) "igrep"
 "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)


;; Stop asking for confirmation in dired
(setq dired-no-confirm
  '(byte-compile chgrp chmod chown compress copy delete hardlink load
    move print shell symlink uncompress))

;; Change the some actions for query-replace
(define-key query-replace-map [return] 'act)
(define-key query-replace-map "\C-m" 'act)

;;
;;
;;;;;


;; ispell4:;;
;;(setq flyspell-multi-language-p nil)


;;
;;  Start flyspell mode for text files
;;
;;(add-hook 'text-mode-hook 'flyspell-mode)

;;
;; This section manages to use a telnet inside a buffer,  Its not a wonderful
;; experience all the time, as there is no terminal emulation, but for command
;; line work its great.
;;
(require 'telnet)

;; This is the telnet binary
(setq telnet-program (concat (expand-file-name (getenv "HOME")) "/Binaries/telnet.exe"))

(defun telnet (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*telnet-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (name (concat "telnet-" (comint-arguments host 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 process)
    (cond ((string-equal system-type "windows-nt")
      (setq telnet-new-line "\n")))
    (if (and buffer (get-buffer-process buffer))
	(pop-to-buffer (concat "*" name "*"))
      (pop-to-buffer (make-comint name telnet-program nil host))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      (accept-process-output process)
      (telnet-mode)
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count))))


;; Latex Viewer
(setq tex-dvi-view-command "D:\\Latex\\miktex\\bin\\yap.exe")
(setq tex-run-command "D:\\Latex\\miktex\\bin\\latex.exe")

;;
;;;;;



;;;;;
;;
;; Section 4.
;;  C / C++ Mode customization
;;

;;
;;  Add function index to menu bars.


			

(require 'cc-styles)

(defconst my-indentation-style
  '(
    (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     ;; the following preserves Javadoc starter lines
     (c-hanging-comment-starter-p . nil)
     (c-offsets-alist
      .
      ((inline-open . 0)
       ;; Changed from 0 to 2
       (topmost-intro-cont    . 2)
       (statement-block-intro . +)
       (knr-argdecl-intro     . 5)
       (substatement-open     . 0)
       (label                 . 0)
       (statement-case-open   . +)
       (statement-cont        . 2)
       (arglist-intro  . c-lineup-arglist-intro-after-paren)
       (arglist-close  . c-lineup-arglist)
       (access-label   . 0)
       (inher-cont     . c-lineup-java-inher)
       (func-decl-cont . c-lineup-java-throws)
       ))
     ) "PERSONAL" )

(defun my-c-mode-common-hook ()
       ;; add my personal style and set it for the current buffer
       (c-add-style "PERSONAL"  my-indentation-style t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)







;;
;;;;;;



;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weblint.el;;
;; An emacs command for running Neil Bowers' weblint on the current emacs
;; buffer.  Weblint is an excellent HTML syntax checker that will catch
;; all sorts of subtle and not-so-subtle problems in your HTML code.;;
;; INSTALLATION:;; If you don't have weblint already installed, get it.  Weblint
;; can be found on the Web at:;;
;;        http://www.khoros.unm.edu/staff/neilb/weblint.html;;
;; Follow the instructions for installing weblint, and test it on a couple
;; of files to make sure it's working.;;
;; To install the emacs interface just copy the contents of this file to
;; your .emacs file.  Change the variable weblint-path if necessary,
;; and set the command key to whatever pleases you.  I use ^C-w.;;
;; Alternatively you can just add the line
;;     (autoload 'weblint "weblint" "Weblint syntax checker" t)
;; to your .emacs file and store this file with the rest of your emacs
;; libraries.;;
;; To use this interface, edit the HTML code as you normally would.  When
;; you want to do the syntax check, invoke the "weblint" command by
;; typing the command key you've selected (or use M-x and type "weblint").
;; This will invoke the emacs compile function, first displaying the
;; weblint command to give you an opportunity to add or delete any command
;; line switches, and then running weblint in a new window named *weblint*.
;; Errors will appear in this window one by one and you can use the standard
;; emacs keys for autoscrolling to the next error:;;
;;     ^X-`     Scroll to the next error
;;     ^C-^C    Jump to the error the cursor is on
;;     ^C-RET   Run weblint again;;
;; Author: Lincoln D. Stein (lstein@genome.wi.mit.edu)
;;         http://www-genome.wi.mit.edu/~lstein;; with help from:
;;         Nadeem Vaidya (vaidya@genome.wi.mit.edu)
;;         http://www-genome.wi.mit.edu/~vaidya
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weblint stufff

;;;; Uncomment this and set it to whatever you want
;;(global-set-key "\C-cw" 'weblint)

;; path to weblint
(defvar weblint-path "perl f:/usefulperl/weblint/weblint.pl"
  "Full path to weblint.")

;; Additional options for weblint
;; Allow Netscape extensions to HTML.
(defvar weblint-options "-x netscape"
  "Any additional options to pass to weblint.")

;; the weblint function, which just calls "compile"
(defun weblint ()
  "Emacs front end to Neil Bowers' Weblint."  (interactive)
  ;;(let ((compile-command (format "%s %s %s"
;; weblint-path weblint-options (buffer-name))))
  (let ((compile-command (format "%s %s *.html" ; for use w/ htmlpp -- c^2
				 weblint-path weblint-options)))    
    (call-interactively 'compile) )
  (save-excursion
    (set-buffer"*compilation*")
    (rename-buffer "*weblint*")))


;; Set the classpath for the JDE.
(setq jde-global-classpath "F:/jdk1.1.6/lib/classes.zip;.;E:/Mhaoteu/")
(require 'jsee)


;; Load the process viewer / killer
(require 'vkill)

;; Load the support for the NT Command shell
(require 'nt-shell)

(provide 'winnt)
