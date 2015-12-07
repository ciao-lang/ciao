; -*- mode: emacs-lisp; -*-
;;; ciao-widgets-test.el --- Test for ciao-widgets

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

(require 'ciao-widgets)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old tests --has to dissapear from here at some point.

(defun mytest ()
;;  (set-default-font "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")  
;;  (set-background-color "peach puff")
;  (set-background-color "black")
;  (set-background-color "white")
;  (ciao-startup)
;;  (find-file "qsort_func.pl")
;;   (split-window (selected-window) 20)
;;   (other-window 1)
  (ciao-widget-example)
  (setq cursor-type nil)
;;  (other-window 1)
  )

; (setq ciao-ciaopp-prog-lang 0) ; Ciao
; (setq ciao-ciaopp-prog-lang 1) ; Java

(defun ciao-widget-example ()  
  (interactive)
  (let
      ((l '(
	    "Use Saved Menu Configuration:    [none, mc1, mc1, mc1, mc1, default, demo1,
                                  demo1, d1, d1, algo, ver, ver1, ver2, df_h, 
                                  comp, rtcp2, rtc3, rtcp, ctc, ctrt] (none) ? "
	    "{ERROR: Please specify more: [asdf,111aaa,asdf].}"
	    "{NOTE (aaa): Just a note.}"
	    "Collapse AI Info:                [off, on] (on) ? "
	    "{Note: Just an strange note}
                            Print Program Point Info:        [off, on] (off) ? "
	    "Perform Determinism Analysis:    [none, det] (none) ? "
	    "Perform Non-Failure Analysis:    [none, nf, nfg] (none) ? "
	    "Select Cost Analysis:            [none, steps_ub, steps_lb, steps_ualb, steps_o]
                                  (none) ? "
	    "{NOTE (aaa): This is a compose note
so it can have several lines}"
	    "Select Action Group:             [analyze, check_assertions, optimize]
                                  (analyze) ? "
	    "Select Menu Level:               [naive, expert] (naive) ?"
	    "An Edit box:                     (on) ? "
	    )))
    (ciao-create-widget-buffer-from-list l))
)


;; Provide ourselves:

(provide 'ciao-widgets-test)

;;; ciao-widgets-test.el ends here

