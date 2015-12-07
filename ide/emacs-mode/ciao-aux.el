;;; ciao-aux.el --- Auxiliary definitions for Ciao Mode for Emacs
;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and
;; M. Hermenegildo and others (herme@fi.upm.es, UPM-CLIP, Spain).

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'ciao-faces)

;; This is to avoid warnigns in fsf from xemacs vars and functions.
(eval-when-compile
  (if (boundp 'xemacs-logo)
      ()
    ;; fsf
    (defun make-glyph (&rest foo))
    (defun extent-at (&rest foo))
    (defun make-extent (&rest foo))
    (defun set-extent-property (&rest foo))
    ))

;;===========================================================================
;; Auxiliary
;;===========================================================================

;;---------------------------------------------------------------------------
;; Some paths and location of some library files
;;---------------------------------------------------------------------------

(require 'ciao-config) ; ciao-get-config

(defun ciao-find-icon (icon)
  "Icon with absolute path (for xemacs)"
  (let (bundledir-core bundledir-ide icon-core icon-ide)
    (setq bundledir-core (ciao-get-config :bundledir-core))
    (setq bundledir-ide (concat bundledir-core "/../ide/emacs-mode/"))
    (setq icon-core (expand-file-name icon bundledir-core))
    (setq icon-ide (expand-file-name icon bundledir-ide))
    (cond ((file-exists-p icon-core) icon-core)
	  ((file-exists-p icon-ide) icon-ide))))

;;---------------------------------------------------------------------------
;; Ciao's own windows abstraction
;;
;; TODO: Allow get-buffer-window search in other frames?  That would
;;       make possible detaching some process or menu in a different
;;       frame.
;;---------------------------------------------------------------------------

(defun ciao-switch-this-window (buffer) ; (bufferp buffer)
  "Switch to the window containing `buffer', if exists, or open
in the selected window otherwise."
  (if (eq buffer nil)
      nil ; Do nothing ; TODO: show error?
    (let ((window (get-buffer-window buffer)))
      (if window
	  ;; Buffer found in other window, select
	  (select-window window)
	;; Buffer not visible, open in the selected window
	(switch-to-buffer buffer)))))

(defun ciao-switch-other-window (buffer) ; (bufferp buffer)
  "Switch to the window containing `buffer', if exists, or open a
new one otherwise."
  (if (eq buffer nil)
      nil ; Do nothing ; TODO: show error?
    (let ((window (get-buffer-window buffer)))
      (if window
	  ;; Buffer found in other window, select
	  (select-window window)
	;; Buffer not visible, open a new window for it
	(switch-to-buffer-other-window buffer)))))

;;------------------------------------------------------------
;; Mark a region with a color
;;------------------------------------------------------------

;; JFMC: Old version
;;  "Switch to buffer `buffer' in other window, except it we are
;;already visiting that buffer."
;;  (if (string= (buffer-name) (buffer-name buffer))
;;      t ; We are already there
;;    (switch-to-buffer-other-window buffer)))

(defun ciao-color (startline endline color over)
  "Highlight region from STARTLINE to ENDLINE using COLOR with overlay name
OVER."
  (let (start end overlay)
    (save-excursion
      (goto-line startline)
      (setq start (point))
      (goto-line endline)
      (end-of-line)
      (if (or (eq over 'ciao-error) (eq over 'ciao-debug))
	  (setq end (+ (point) 1))
	(setq end (point))))
    (setq overlay (make-overlay start end))
    (overlay-put overlay 'face color)
    (overlay-put overlay over t)))

(defun ciao-uncolor (startline endline over)
  "Unhighlights the region from STARTLINE to ENDLINE with the overlay name
OVER."
  (let (start)
    (save-excursion
      (goto-line startline)
      (setq start (point)))
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr over) 
			     (delete-overlay ovr))))
	    (overlays-at start))))

;;------------------------------------------------------------
;; Miscellaneous
;;------------------------------------------------------------

;; TODO: use native what-line if possible? (probably that is more efficient)
(defun ciao-what-line ()
  "Return the line number. This function is a fix for the fact that in
xemacs the function what-line does not behave as in emacs."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))
	 
;; Local version of replace-regexp-in-string, since it is not 
;; present in older versions of emacsen
(defun ciao-replace-regexp-in-string (regexp rep string &optional
					     fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (ciao-replace-regexp-in-string 
   \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1) 
  => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun ciao-insert-image (type image default)
  "Portable image insertion (emacs, xemacs). Third argument is text to
be used if images not supported (e.g., in text mode)"
  (let
      (imagefile imagefile-fullpath first-char)
    (setq first-char (substring image 0 1))
    (if (or (string= first-char "/")              ;; /foo 
	    (string= first-char ".")              ;; ./foo
	    (string= first-char "\\")             ;; \foo
	    (string= (substring image 1 2) ":"))  ;; C:foo
	;; Path given: keep in all cases
	(progn
	  (setq imagefile image)
	  (setq imagefile-fullpath image))
      ;; Probably no path: look under icons for emacs, 
      (setq imagefile (concat "icons/" image))
      ;; put full lib path for xemacs
      (setq imagefile-fullpath 
	    (ciao-find-icon (concat "icons/" image))))
    (cond 
     ((and (fboundp 'tool-bar-mode) window-system);; emacs, graphical
      (insert-image 
       (find-image (list (list :type type :file imagefile )))))
     ((and (boundp 'xemacs-logo) window-system);; xemacs, graphical
      (ciao-xemacs-insert-glyph ;; xemacs needs full path
       (make-glyph (vector type :file imagefile-fullpath ))))
     (t ;; text mode
      (insert default)))))

(defun ciao-xemacs-insert-glyph (gl)
  "Insert a glyph at the left edge of point."
  (let ((prop 'ciaoimage)        ;; ciaoimage is an arbitrary name
	extent)
    ;; First, check to see if one of our extents already exists at
    ;; point.  For ease-of-programming, we are creating and using our
    ;; own extents (multiple extents are allowed to exist/overlap at the
    ;; same point, and it's quite possible for other applications to
    ;; embed extents in the current buffer without your knowledge).
    ;; Basically, if an extent, with the property stored in "prop",
    ;; exists at point, we assume that it is one of ours, and we re-use
    ;; it (this is why it is important for the property stored in "prop"
    ;; to be unique, and only used by us).
    (if (not (setq extent (extent-at (point) (current-buffer) prop)))
	(progn
	  ;; If an extent does not already exist, create a zero-length
	  ;; extent, and give it our special property.
	  (setq extent (make-extent (point) (point) (current-buffer)))
	  (set-extent-property extent prop t)
	  ))
    ;; Display the glyph by storing it as the extent's "begin-glyph".
    (set-extent-property extent 'begin-glyph gl)
    ))

;; MH cygdrive Fixed for newer version of cygwin
;; MH //c/ and also /cygdrive/
(defun fix-cygwin-drive-letter (filename)
  (if (eq (string-match "//./" filename) 0)
      (concat (substring filename 2 3) ":" (substring filename 3))
    (if (eq (string-match "/cygdrive/" filename) 0)
	(concat (substring filename 10 11) ":" (substring filename 11))
      filename
    )))

;; Define match-string-no-properties, if not defined (i.e., in xemacs)
(or (fboundp 'match-string-no-properties)
    (defun match-string-no-properties (number)
      "Return string of text matched by last search."
      (buffer-substring-no-properties (match-beginning number)
                                      (match-end number))))

;;------------------------------------------------------------
;; Region handling
;;------------------------------------------------------------

(defun ciao-write-region (minpoint maxpoint filename)
  (let (original-buffer buffercont temp-buffer)
    (setq original-buffer (current-buffer))
    (setq buffercont (buffer-substring-no-properties minpoint maxpoint))
    (setq temp-buffer (generate-new-buffer "temp-buffer"))
    (set-buffer temp-buffer)
    (insert buffercont "\n")
    (write-region (point-min) (point-max) filename nil nil)
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

;;------------------------------------------------------------
;; A simple implementation of queues (see Elib `queue')
;;------------------------------------------------------------

(defun ciao-queue-clear (queue)
  "Remove all elements from `queue'"
  (set queue nil))

(defun ciao-queue-empty (queue)
  "The queue `queue' is empty"
  (eq (symbol-value queue) nil))

(defun ciao-queue-enqueue (queue elem)
  "Add `elem' as the last element in queue `queue'"
  (let ((queueval (symbol-value queue)))
    (setq queueval (append queueval (list elem)))
    (set queue queueval)))

(defun ciao-queue-dequeue (queue)
  "Extract and return the first element from queue `queue'"
  (let* ((queueval (symbol-value queue))
	 (elem (car queueval))
	 (rest (cdr queueval)))
    (set queue rest)
    elem))

;;------------------------------------------------------------
;; Easy choice selection 
;;------------------------------------------------------------

;; E.g., '(choice (const a) (const b) ...)
(defun ciao-completion-choice (opts)
  "Choice list (for use in `defcustom')"
  `(choice
    ,(mapcar
      (function (lambda (x) `(const ,x)))
      opts)))

;; E.g., '((a 1) (b 2) ...)
(defun ciao-completion-collection (opts)
  "Choice collection `completing-read'"
  (let ((i 1))
    (mapcar 
     (function (lambda (x)
		 (setq i (1+ i))
		 (list x i)))
     opts)))

;; E.g., "a, b, ..."
(defun ciao-completion-str (opts)
  "Option string for `completing-read'"
  (mapconcat
   (function (lambda (x)
	       (format "%s" x)))
   opts ", "))

(defun ciao-completing-read (var msg choices)
  "Ask for a new value of `var' (showing message `msg') among the
values of `choices'"
  (set
   var
   (completing-read
    (concat msg " ("
	    (ciao-completion-str choices)
	    ") ")
    (ciao-completion-collection choices)
    nil t (symbol-value var))))


;; Provide ourselves:

(provide 'ciao-aux)

;;; ciao-aux.el ends here

