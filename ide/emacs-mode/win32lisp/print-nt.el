;;; print-NT.el --- Special printing functions for Windows NT
;;
;; Author: Frederic Corne <frederic.corne@erli.fr> 
;; Created: jul 1996
;; Version: 1.0 1996/10/02 
;; Keywords: print, PostScript
;;
;;; Commentary:
;; 
;; This file is based on the example of Pascal Obry (in the ntemacs FAQ).
;;
;; This file overwrites the items of the Tools menu, so it must be loaded 
;; before any use of the menu.
;;
;; It alows to print with faces or not.
;;
;; USAGE: Byte-compile this file, and add the following line to your
;; emacs initialization file (.emacs/_emacs) according to the type 
;; of printer command (print or lpr):
;; 
;; if you use print.exe command (The UNC style location for regular print jobs):
;; Remember to escape the backslashes(use '\\\\' for each '\\') for emacs.
;; 
;; (setq lpr-command "print"
;; lpr-destination '("/D:\\\\host\\share-name") ;; for normal printer
;; ps-lpr-destination '("/D:\\\\host\\share-name") ;; for postscript printer
;; 
;; (setq ps-paper-type 'ps-a4 ) ; the type of paper (if needed)
;; 
;; (load "print-NT"() t)
;; 
;; 
;; 
;; if you use lpr.exe command, add :
;; 
;; (setq lpr-command "lpr"
;; lpr-destination '("-S host -P share-name") ;; for normal printer
;; ps-lpr-destination '("-S host -P share-name")) ;; for postscript printer
;; 
;; (setq ps-paper-type 'ps-a4 ) ; the type of paper (if needed)
;; 
;; (load "print-NT"() t)
;; 
;;
;; You can also change the temporary buffers values ps-lpr-buffer and txt-lpr-buffer
;;;;; 
;;;;; Change log:
;;
;; Revision 1.0 1996/10/02 fcorne
;; Initial revision
;;
;; Credit for suggestions, patches and bug-fixes:
;; David J. Fiande <davidf@mks.com>
;; Brad Sileo <brad@GeoData-GIS.com> 
;;;;; 
;;;;; Change log:
;;
;; Revision 1.1 1997/4/10 gorkab
;;
;; Brian Gorka <gorkab@sanchez.com>
;; 
;; Added nt-ps-2up-print-buffer
;;
;; With psnup.exe you can print multiple pages per sheet...
;;
;; (defvar ps-psnup-command "psnup")
;; (defvar ps-psnup-switches '(" -l -2 -m.25in "))
;; (defvar ps-psnup-buffer "c:\\tmp\\psnup.ps")
;;;;; 
;;;;; Change log:
;;
;; Revision 1.11 1997/4/28 gorkab
;;
;; Brian Gorka <gorkab@sanchez.com>
;; 
;; Added nt-ps-gs-preview-buffer
;; Added nt-ps-gs2up-preview-buffer
;;
;; With Aladdin Ghostscript 4.03 you can preview postscript output. (and most likely GNU)
;; From GSView, you can then print the files as PDF or to a non postscript printer.
;; Ghostscript and GSview ae available from sites listed here:
;; http://www.cs.wisc.edu/~ghost/aladdin/obtain.html
;;
;; ;; Location and name of ghostscript
;; (defvar gs-print-command "d:/gs/gswin32.exe") 
;; ;; Location and name of GSView
;; (defvar gs-view-command "D:/gs/gsview/gsview32.exe")
;;
;; I plan to add direct to PDF and direct to non-postscript printer support 
;; in the next version. The code is sloppy (many redundant functions) 
;; but, it works, and it's easy to modify.
;; 
;; Suggestions and bug reports are welcome...
;;; Code:

(defvar lpr-destination '("-S host -P share-name"))
(defvar ps-lpr-destination '("-S host -P share-name"))

(defvar ps-lpr-buffer "c:\\temp\\psspool.ps")
(defvar txt-lpr-buffer "c:\\temp\\psspool.txt")

(defvar ps-psnup-command "psnup")
(defvar ps-psnup-switches '(" -l -2 -m.25in "))
(defvar ps-psnup-buffer "c:\\temp\\psnup.ps")

(defvar gs-print-command "c:\\gs\\gswin32.exe")
(defvar gs-view-command "c:\\gs\\gsview\\gsview32.exe")

;;;;;;; print simple
(defun print-lpr-buffer ()
(let ((shell-file-name "cmd.exe") ; switch back to cmd.exe temporarily
(win32-quote-process-args nil)
(shell-command-switch "/c"))
(shell-command
(apply 'concat (append (list lpr-command " ")
lpr-destination (list " " txt-lpr-buffer)))))
(delete-file txt-lpr-buffer)
)
(defun nt-print-buffer ()
(interactive)
(write-region (point-min) (point-max) txt-lpr-buffer)
(print-lpr-buffer )
)
(defun nt-print-region ()
(interactive)
(write-region (point) (mark) txt-lpr-buffer)
(print-lpr-buffer )
)

(define-key menu-bar-tools-menu [print-region]
'("Print Region" . nt-print-region))

(define-key menu-bar-tools-menu [print-buffer]
'("Print Buffer" . nt-print-buffer))

;;;;;;; ps-print 

(require 'ps-print)

(defun ps-print-lpr-buffer ()
(let ((shell-file-name "cmd.exe") ; switch back to cmd.exe temporarily
(win32-quote-process-args nil)
(shell-command-switch "/c"))
(shell-command
(apply 'concat (append (list ps-lpr-command " ")
ps-lpr-destination
(list " " ps-lpr-buffer)))))
(delete-file ps-lpr-buffer)
)

(defvar ps-print-use-faces nil
"*Non-nil means alway print with faces.")

(defun toggle-use-face ()
(interactive)
(if ps-print-use-faces 
(progn (setq ps-print-use-faces nil)
(message "ps-print-use-faces disabled"))
(setq ps-print-use-faces t)
(message "ps-print-use-faces enabled"))
)

(define-key menu-bar-tools-menu [toggle-use-face]
'("Toggle on/off print-with-faces" . toggle-use-face))

(defun nt-ps-print-buffer ()
(interactive)
(if ps-print-use-faces 
(ps-print-buffer-with-faces ps-lpr-buffer)
(ps-print-buffer ps-lpr-buffer)
)
(ps-print-lpr-buffer)
)

(define-key menu-bar-tools-menu [ps-print-buffer]
'("Postscript Print Buffer" . nt-ps-print-buffer))
(define-key global-map "\C-cp" 'nt-ps-print-buffer)

(defun nt-ps-print-region ()
(interactive)
(if ps-print-use-faces 
(ps-print-region-with-faces (point) (mark) ps-lpr-buffer)
(ps-print-region (point) (mark) ps-lpr-buffer)
)
(ps-print-lpr-buffer)
)

(define-key menu-bar-tools-menu [ps-print-region]
'("Postscript Print Region" . nt-ps-print-region))

(define-key global-map "\C-crp" 'nt-ps-print-region)

(defun nt-ps-2up-print-buffer ()
(interactive)
(ps-print-buffer-with-faces ps-lpr-buffer)
(let ((shell-file-name "cmd.exe") ; switch back to cmd.exe temporarily
(win32-quote-process-args nil)
(shell-command-switch "/c"))
(shell-command
(apply 'concat (append (list ps-psnup-command " ")
ps-psnup-switches
(list " " ps-lpr-buffer)
(list " " ps-psnup-buffer)
)))

(delete-file ps-lpr-buffer)

(shell-command
(apply 'concat (append (list ps-lpr-command " ")
ps-lpr-destination
(list " " ps-psnup-buffer)))))

(delete-file ps-psnup-buffer)
)

(define-key menu-bar-tools-menu [2up-print-buffer]
'("2up Print Buffer" . nt-ps-2up-print-buffer))

(define-key global-map "\C-c2p" 'nt-ps-2up-print-buffer)

(defun nt-ps-gs-preview-buffer ()
(interactive)
(ps-print-buffer-with-faces ps-lpr-buffer)
(let ((shell-file-name "cmd.exe") ; switch back to cmd.exe temporarily
(win32-quote-process-args nil)
(shell-command-switch "/c"))
(shell-command
(apply 'concat (append (list gs-view-command " ")
(list " " ps-lpr-buffer)
)))

(delete-file ps-lpr-buffer)

))

(define-key menu-bar-tools-menu [gs-preview-buffer]
'("GS Preview Buffer" . nt-ps-gs-preview-buffer))

(define-key global-map "\C-cgp" 'nt-ps-gs-preview-buffer)

(defun nt-ps-gs2up-preview-buffer ()
(interactive)
(ps-print-buffer-with-faces ps-lpr-buffer)
(let ((shell-file-name "cmd.exe") ; switch back to cmd.exe temporarily
(win32-quote-process-args nil)
(shell-command-switch "/c"))
(shell-command
(apply 'concat (append (list ps-psnup-command " ")
ps-psnup-switches
(list " " ps-lpr-buffer)
(list " " ps-psnup-buffer)
)))

(delete-file ps-lpr-buffer)

(shell-command
(apply 'concat (append (list gs-view-command " ")
(list " " ps-psnup-buffer)
)))

(delete-file ps-psnup-buffer)
))

(define-key menu-bar-tools-menu [gs2up-preview-buffer]
'("GS 2up Preview Buffer" . nt-ps-gs2up-preview-buffer))

(define-key global-map "\C-cg2p" 'nt-ps-gs2up-preview-buffer)