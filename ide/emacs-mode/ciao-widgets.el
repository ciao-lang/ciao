;;; ciao-widgets.el --- Graphical forms based on emacs widgets

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

;; ==========================================================================
;; 
;; This is a generic library which captures ASCII menus (see
;; library/menu.pl) for selecting options from an Ciao inferor shell
;; mode, and presents a graphical interface.
;; 
;; TODO: Remove references to CiaoPP within this code. It should be
;;   next to the code that presents the menus from the command line.

(eval-when-compile (require 'wid-edit))

(require 'ciao-aux) ; ciao-insert-image
(require 'ciao-process) ; ciao-get-logo
(require 'ciao-common) ; ciao-ciaopp-gmenu-buffer-name

;;------------------------------------------------------------
;; Preprocess buffer - CiaoPP graphical menu instrumental variables
;;------------------------------------------------------------

; TODO: This is just for the icon, find a better way
(defvar ciao-ciaopp-prog-lang 0
  "Whether the program loaded is a ciao (0) or a Java (1) program." )

(defvar ciao-widget-str-list nil
  "This variable accummulates the text that
ciao-ciaopp-process-graphical-menu gets with the previous calls to
this hook in order to be able to parse it (needed since Emacs returns
pieces of strings, like: 'men' 'u option: ' '[a,b]' '? ', at each
call.")

(defvar ciao-widget-id 0
   "Every time a line that matches a menu string (function is_a_menu)
is printed, a new graphical menu widget is created. These widgets are
identified by a widget identifier, that is currently a number. This
number is the same as the line number that was used to create the
widget. This widget id is used to find out in which buffer-based menu
question the value of the widget has to be typed.")

(defvar ciao-widget-last-changed-option -1
  "This variable identifies which widget was modified. The algorithm
will introduce answers in the CiaoPP buffer (as is the user had typed
them) while the current menu question number is less or equal that the
value of this variable.")

(defvar ciao-widget-values nil
   "A list that contains the current values of the widgets created. It
should look like: '((1 option_widget1) (2 option_widget2)).")

(defvar ciao-cancel-widget-values nil
  "Each time the menu is started, this variable saves the current menu
values. This is in order to be able to restore these values will be
restored if the cancel button is pressed in the graphical menu.")

(defvar ciao-gm-recovering 0
  "Sometimes the CiaoPP top-level menu can generate a 'Note:
Incorrect Option'. When this happens, the ciao-gm-recovering
variable is increased in 1, and the graphical menu
hook (ciao-ciaopp-process-graphical-menu) starts inserting
carriage returns in to avoid more errors. When a prompt is
reached, the current prompt hook (ciao-ciaopp-show-graphic-menu)
will restart the menu process if this variable is greater than
0. After incorrect option is obtained twice, it will stop and
print an error message.")

;;------------------------------------------------------------
;; Images
;;------------------------------------------------------------

;; TODO: I was not able to use `defimage'. This code is equivalent.

(defvar ciao-cb-arrow-img
  (find-image '((:type xpm :file "icons/cb_arrow.xpm" :ascent center)))
  "Combo box arrow")

(defvar ciao-cancel-img
  (find-image '((:type png :file "icons/cancel2.png" :ascent center)))
  "Cancel icon")

(defvar ciao-ok-img
  (find-image '((:type png :file "icons/go.png" :ascent center)))
  "OK/Apply icon")

(defvar ciao-customize-img
  (find-image '((:type xpm :file "icons/ciaocustomize.xpm" :ascent center)))
  "Ciao customize icon")

;;------------------------------------------------------------
;; Graphical Menu
;;------------------------------------------------------------

(defvar ciao-g-showoutput nil
  "Stored the value of local variable of
ciao-build-ciaopp-specific-command because it is need in a hook after
clicking the OK button in the graphical menu.")


;; (let ((frame (make-frame '(
;; 			   (title . "ypuiii2")
;; 			   (background-color . "gray")
;; 			   (foreground-color . "black")
;; 			   (width . 50)
;; 			   (height . 50)
;; 			   (vertical-scroll-bars . right)
;; 			   (minibuffer . nil)
;; 			   (cursor-type . nil)
;; 			   (scroll-bar-foreground . "gray")
;; 			   (scroll-bar-background . "gray")
;; 			   )
;; 			 )
;; 	     ))
;;   (set-frame-size frame 60 30)
;;   (set-frame-position frame 
;; 		      (* (/ (- (screen-width)  (frame-width frame )) 2) (frame-char-width  frame))
;; 		      (* (/ (- (screen-height) (frame-height frame)) 2) (frame-char-height frame))
;; 		      )
;;   )

; magic line! (x-popup-menu `((0 0) ,(frame-first-window (car (frame-list)))) '("Titulo general" ("titulo" ("Print Buffer" . print-buffer) ("---"))) )
;; (x-popup-dialog `((0 0) ,(frame-first-window (car (frame-list))))
;; 	      '("Titulo" ("titulo" ("Print Buffer" . p1) ("---") ("Print Buffer2" . p2)) 
;; 		("titulo222" ("Print Buffer" . p3) ("---") ("Print Buffer2" . p4) 
;; 		 ("titulo444" ("Print Buffer" . p3) ("---") ("Print Buffer2" . p4)))
;; 		) 
;; )

(defface ciao-button-widget-face 
  '((((class color) (background dark))
     (;:box (:line-width 2 :style released-button)
;      :background "grey20"
      :foreground "white"
      :weight bold
      ))
    (((class color) (background light))
     (;:box (:line-width 2 :style released-button) 
;      :background "lightgrey"
      :foreground "black"
      :weight bold
      ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defvar ciao-title-widget-face 'ciao-title-widget-face)
(defface ciao-title-widget-face ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (background light)) 
     (:foreground "black" :weight bold :family "helv" :height 1.3))
    (((class color) (background dark)) 
     (:foreground "white" :family "helv" :height 1.3))
    (t (:inverse-video t :weight bold)))
  "Face to use for interactive menu title."
  :group 'ciao-faces)

(defface ciao-button-pressed-widget-face
  '((((type x w32 mac) (class color) (background dark))
     (
;;    :inherit    ciao-button-widget-face
;;    :width      normal
;;    :italic     nil
;;    :weight     normal
;;    :italic     nil
      :background "grey20"
      :foreground "white"
      :box        (:line-width 2 :style pressed-button)
;      :slant       normal
      ))
    (((type x w32 mac) (class color) (background light))
     (
      :box        (:line-width 2 :style pressed-button)
      :background "lightgrey"
      :foreground "black"
;      :slant       normal
      ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defface ciao-edit-widget-face
  '((((class color) (background dark))
     (:foreground "white"
      :background "grey20"
;     :background "lightgrey" :foreground "black")
;      :slant       italic
      ))
    (((class color) (background light))
     (:foreground "black"
      :background "lightgrey"
;      :slant      italic
      ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

; (face for mouse over)
(defface ciao-mouse-widget-face
  '((((class color) (background dark))
     (:background "white"
      :foreground "black"
;     :background "lightgrey" :foreground "black")
;      :slant       italic
      ))
    (((class color) (background light))
     (:background "black"
      :foreground "white"
;      :slant      italic
      ))
    (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defface ciao-text-widget-face '(
     (((class color) (background dark))
     (:foreground "lightgrey"))
     (((class color) (background light))
      (:foreground "black"))
     (t nil))
  "Face used for documentation text."
  :group 'ciao-faces)

(defface ciao-menu-error-widget-face '(
     (((class color) (background dark))
      (:foreground "firebrick"))
     (((class color) (background light))
      (:foreground "firebrick"))
     (t nil))
  "Face used for menu error representation in graphical interface."
  :group 'ciao-faces)

(defface ciao-menu-note-widget-face '(
     (((class color) (background dark))
      (:foreground "slate blue"))
     (((class color) (background light))
      (:foreground "slate blue"))
     (t nil))
  "Face used for menu note representation in graphical interface."
  :group 'ciao-faces)

(defvar ciao-widget-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\t"           'widget-forward)
    (define-key map [(shift tab)]  'widget-backward)
    (define-key map [backtab]      'widget-backward)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map "\C-m"         'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.")

(defun ciao-create-enumerated-list (args num)
"Starting by number 'num', it returns a list of pairs (value number)
composed by 1 element of the list 'args' and successive numbers of
'num'.

 Example:
 (ciao-create-enumerated-list '(a b c) 0)
  => ((0 a) (1 b) (2 c))"
(let ( (ele   (car args)) )
  (if (not (cdr args))
      `((,(number-to-string num) ,ele))
    (cons `(,(number-to-string num) ,ele)
	  (ciao-create-enumerated-list (cdr args) (1+ num))))))

(defun ciao-createpair-list (args)
"For a given list (a b c), it creates the list ((a a) (b b) (c c))"
(let ((ele (car args)))
    (if (not (eq ele '()))
        (cons `(,ele ,ele) (ciao-createpair-list (cdr args)))
      '() )))

(defun ciao-gm-define-combo-box-list (args)
"For a given list of pair option-value, it returns a list of usable
combo-box items.

Example:
 (ciao-gm-define-combo-box-list '((aa 1) (bb 2) (cc 3)))
  => ((item :tag aa :value 1) (item :tag bb :value 2) (item :tag cc :value 3))"
(let ( (tag    (car  (car args)))
       (value  (cadr (car args)))
       (rest_a (cdr args)))
  (if (eq args '( ))
      '()
    ( cons
      `'(choice-item 
	 :tag         ; ,tag 
	 ,(concat tag (truncate-string-to-width "" (- 9 (length tag)) 0 ?  ))
	 :value       ,value 
	 :sample-face ciao-edit-widget-face
	 :mouse-face ciao-mouse-widget-face
	 :pressed-face ciao-mouse-widget-face
	 :format      "%[%{ %t %}%]"
	 :keymap widget-field-keymap
	 )
      (ciao-gm-define-combo-box-list rest_a)))))

(defun ciao-create-combo-box (id title args default_option)
"Returns a combo-box with args elements as options"
(let* ( (combo-list (eval (append
	   '(widget-create 'menu-choice
			   :widgetid    id
			   :tag         title
			   :menu-tag    "Choose"
			   :case-fold   nil
			   :sample-face 'ciao-text-widget-face
			   :format      "%{%t:%} %v"
			   :value       default_option
			   :notify      'ciao-widget-hook
			   )
	   (ciao-gm-define-combo-box-list (ciao-createpair-list args))
	   )))
	(arrow      (widget-create 'push-button
			   :tag       "\\/"
			   :tag-glyph ciao-cb-arrow-img
			   :sample-face 'ciao-edit-widget-face 
			   :pressed-face 'ciao-edit-widget-face 
					; custom-button-face
					;   :button-face
					;   '(ciao-button-widget-face
					;   custom-button-face
					;   custom-button-pressed-face)
			   :format "%[%v %]\n"
			   :ascent 'center
			   )))
  (widget-put arrow :parent combo-list)
  (widget-put arrow :action 'widget-parent-action)))

(defun ciao-widget-indent-text (title)
"Put spaces before title to make all text fit on the same columns
  (ciao-widget-indent-text \"helooow\")
=> \"                          helooow\"

WARNING: do not remove the space between \"? )\", because '? ' MEANS
SPACE character"
  (let ((menu_width  33))
    (concat (make-string (max 0 (- menu_width (length title))) ? ) title)))

(defun ciao-widget-indent-center-text (title)
"Put spaces before title to make all text be centered."
  (let ((menu_width  80))
    (concat (make-string (max 0 (/ (- menu_width (length title)) 2))
			 ?  ) title)))

(defun ciao-widget-cb-indent-text (title)
  "Same than ciao-widget-indent-text but with another lengt (for combo-boxes)"
  (let*((menu_width  20)
	(half        (max 0 (/ (- menu_width (length title)) 2)))
	(rest        (max 0 (- (- menu_width (length title)) half)))
 	)
    (concat (make-string half ? ) title (make-string rest ? ))))

(defun ciao-remove-all-spaces (str)
  (string-match "\\([ ]*\\)\\([_a-z0-9A-Z]*\\)" str)
  (match-string 2 str))

(defun ciao-create-edit-box (wid title defopt)
"Returns an edit-box with defopt as value of the input field"
  (widget-create 'item
		 :sample-face 'ciao-text-widget-face
		 :format "%{%t: %}"
		 :tag (ciao-widget-indent-text title))
  (widget-create 'item
		 :sample-face 'ciao-edit-widget-face
		 :format "%{ %}")
  (widget-create 'editable-field
		 :widgetid  wid
		 :size 9
		 :action 'ciao-widget-hook
		 :valid-regexp "[a-zA-Z0-9, _]"
		 :value-face 'ciao-edit-widget-face
		 :format "%{%v%}"
		 defopt)
  (widget-insert " \n"))

;
; The _MAGIC_ regular expresion for parsing menu lines
;
(defvar ciao-widgets-menu-regexp
  "\\([A-Z][a-zA-Z \\-]*\\): *\\(\\[[a-zA-Z0-9, _\t\n]*\\]\\)?[ \t\n]*\\(([^)]*)\\)[ \t\n]*\\(\\?\\)"
  "The regular expresion to parse a menu line.")

;; ;
;; ; The _MAGIC_ regular expresion for parsing a prompt
;; ;
;; (defun ciao-widgets-prompt-regexp nil
;; ;  "\\([a-zA-Z]*\\) \\?-"
;;   "\\(ciao\\) \\?-"
;; )

;; Note that [...] below is defines the set of chars ]a-zA-Z0-9, [_:\t\n.-
(defvar ciao-widgets-menu-error-regexp 
  "\\({ERROR:\\) *\\([]a-zA-Z0-9, [_:\t\n.-]*\\)}"
  "The regular expresion for parsing menu ERRORs")

(defvar ciao-widgets-menu-note-regexp 
  "\\(Note:\\) *\\([]a-zA-Z0-9, [_:-]*\\)"
  "The regular expresion to parse menu Notes (which is how we mark the
ones we do not want to show)")

(defvar ciao-widgets-menu-note-complex-regexp 
  "\\({NOTE \\(([^)]*)\\):\\) *\\([]a-zA-Z0-9,. [_:\t\n-]*\\)}"
  "The regular expresion to parse menu NOTES")

(defun is_an_error (str)
  "Decides if str belongs to the menu error format"
  (string-match ciao-widgets-menu-error-regexp str))

(defun is_a_note (str)
  "Decides if str belongs to the menu note format"
  (string-match ciao-widgets-menu-note-complex-regexp str))

(defun is_a_menu (str)
  "Decides if str belongs to the menu note format"
  (string-match ciao-widgets-menu-regexp str))

(defun ciao-notify-error-widget (str)
"Generate a widget (item) to say that there was an error"
  (widget-create 'item
		 :tag (ciao-widget-indent-text str)
		 :sample-face 'ciao-menu-error-widget-face
		 :format "  %{%t%}\n\n"))

;; (defun ciao-notify-note-widget (str)
;; "Generate a widget (item) to say that there was a note"
;;   (widget-create 'item
;; 		 :tag (concat (ciao-widget-indent-text "") str)
;; 		 :sample-face 'ciao-menu-note-widget-face
;; 		 :format "  %{%t%}\n\n"))
(defun ciao-notify-note-widget (str)
"Generate a widget (item) to say that there was a note"
  (widget-create 'item
		 :tag (concat 
		       (ciao-widget-indent-center-text (concat "{" str "}"))
		       "\n")
		 :sample-face 'ciao-menu-note-widget-face
		 :format "%t"))

(defun ciao-get-combo-box-components-from-str (str)
"(ciao-get-combo-box-components-from-str 
    \"Select Menu Level:               [naive, expert] (naive) ?\")
 => (\"Select Menu Level\" (quote (\"naive\" \"expert\")) 
    (quote \"naive\"))"


  (string-match ciao-widgets-menu-regexp str)
  (let ((title      (match-string 1 str))
	(ops_str    (match-string 2 str))
	(def_op     (match-string 3 str))
	(menu_width  43))
   `(
      ,(ciao-widget-indent-text            title)
     ',(ciao-get-toplevel-menu-options          ops_str)
     ',(ciao-get-toplevel-menu-default-option   def_op)
     )))

(defun ciao-get-toplevel-menu-default-option (str)
"Remove ( and ) from the parsed menu option. So, 
 \"(default_option)\" is transformed into \"default_option\""
  (if (eq str nil)
      nil
    (string-match "(\\([_a-z0-9A-Z]*\\))" str)
    (ciao-widget-cb-indent-text (match-string 1 str))))

(defun ciao-get-toplevel-menu-options (str)
"Extract options from a list (written in a
 string): (ciao-get-toplevel-menu-options \"[naive, expert]\")
 => (\"naive\" \"expert\")"
  (if (eq str nil)
      nil
      (ciao-get-toplevel-menu-options-aux 1 str 1)))

(defun ciao-get-toplevel-menu-options-aux (argnum str strnum)
  (string-match "\\([_a-zA-Z0-9]*\\)\\([, \t\n]*\\)" str strnum)
  (let* (
	 (argnum1    (1+ argnum))
	 (result_op  (match-string 1 str))
	 (result_esp (match-string 2 str))
	 (the_end1   (match-end 1))
	 (the_end2   (match-end 2))
	 (iresult_op (ciao-widget-cb-indent-text result_op))
	 )
    (if (eq the_end1 the_end2)
	`(,iresult_op)
      (cons iresult_op (ciao-get-toplevel-menu-options-aux 
			(1+ argnum1) str the_end2)))))

;; (defun ciao-ciao-create-combo-box-components-from-str (id str)
;;   (eval (cons 
;; 	 'ciao-create-combo-box
;; 	 (cons id
;; 	  (ciao-get-combo-box-components-from-str str)
;; 	  ))))

(defun ciao-create-widget (id str &optional def_opt)
"Create a widget (combo or edit-box) depending on how the string is."
(let ( (a 0)
       (b 0)
       (c 0) 
       note_str )
  
  (if (is_an_error str)
      (progn 
	(setq a (match-end 2))
	(ciao-notify-error-widget (match-string 2 str))))
   
  (if (is_a_note str)
      (progn 
	(setq b (match-end 2))
	(setq note_str (match-string 3 str))
	(message (concat "*** String received:" note_str "***"))
	(ciao-notify-note-widget note_str)
	(message (concat "*** String received, again:" note_str "***"))
	(if (string= "Incorrect Option" note_str)
	    (progn
	      ; let's not try
	      (setq ciao-gm-recovering (1+ ciao-gm-recovering))
	      ; This is to avoid repeating the last "bad" option
	      ; We assume that id = ciao-widget-id
	      (setq ciao-widget-last-changed-option (1- id))
	      (setq ciao-widget-id (1- id))))))
  
  (if (is_a_menu str)
      (progn
	(setq c (+ 3 (match-end 3)))
	(let* ( (arglist (ciao-get-combo-box-components-from-str str))
		(title   (nth 0 arglist))
		(options (nth 1 arglist))
		(defopt  (if (equal def_opt nil)
			     (nth 1 (nth 2 arglist))
			   def_opt))
		(comboarg (cond 
			   ((equal def_opt nil) arglist)
			   ((not (equal def_opt nil))
			    (list title options `(quote ,def_opt)))))
		)
	  (if (equal options 'nil)
	      (ciao-create-edit-box id title defopt)
	    (if (not (equal (member defopt (cadr options)) nil))
		(eval (cons 'ciao-create-combo-box (cons id comboarg)))
;	      (prin1 (list "defopt: " defopt "  args: " options "\n\n"))
	      )))))
  (max a b c)))

(defun ciao-create-widgets-from-list (n l)
  "For a given list of strings (assumed to be ciaopp-menu strings), it
creates widgets for each element."
(let ((rest (cdr l)))
  (ciao-create-widget n (car l))
  (if (not (equal rest nil))
      (ciao-create-widgets-from-list (1+ n) rest)
    nil
    )))

(defun ciao-create-widgets-buffer nil
  "Create the CiaoPP widgets buffer."
  (interactive)
  (kill-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name))
  (set-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name))
  ;; (switch-to-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name))
  (kill-all-local-variables)

;;       (custom-set-variables
;;	'(widget-image-directory "/home/clip/Systems/graphic-ciao-emacs-menu"))

; (set-default-font "-adobe-courier-bold-r-normal--*-240-*-*-m-*-iso8859-1")  
; (set-default-font "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")  
; (set-background-color "peach puff")
; (set-background-color "black")

;  (set (make-local-variable 'widget-field-face)
;                            'ciao-edit-widget-face)
  (set (make-local-variable 'widget-button-pressed-face)
       'ciao-button-pressed-widget-face)
  (set (make-local-variable 'widget-mouse-face)
       'ciao-button-widget-face) 

  (set (make-local-variable 'cursor-type) nil)

  (insert "\n")
  ;; (ciao-insert-image 'xpm ciao-clip-logo "CLIP")
  ;; (insert " ")
  ;; Display what language are we analyzing
  (if (eq ciao-ciaopp-prog-lang 1)
      (progn
	;; TODO: add a ciaopp-java logo?
	(ciao-insert-image 'png (ciao-get-logo :ciaopp) "")
	(ciao-insert-image 'png (ciao-get-logo :java) "Java"))
    ;; Default: Ciao
    (progn
      (insert "      ")
      (ciao-insert-image 'png (ciao-get-logo :ciaopp) "")
      (insert "   "))
    )
  (ciao-insert-with-face 
   "Preprocessor Option Browser" 'ciao-title-widget-face)
  (insert "   ")
  ;; (ciao-insert-image 'xpm ciao-customize-img "Customize")
  (insert-image ciao-customize-img)
  (insert "\n\n"))

(defun ciao-insert-with-face (string face)
  "Puts string in current buffer with face face."
  (let ((begin (point)))
    (insert string)
    (overlay-put 
     (make-overlay begin (+ (length string) begin) (current-buffer))
     'face face)))

(defun ciao-run-widget-buffer nil
  (use-local-map ciao-widget-keymap)
  (widget-setup))

(defun ciao-create-widgets-buttons nil
  (widget-insert "\n                       ")
  (widget-create 'push-button
		 :tag         "cancel"
		 :tag-glyph   ciao-cancel-img
		 :button-face 'ciao-button-widget-face
		 :pressed-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-cancel-button-widget-hook
		 "Push button")
  (widget-create 'push-button
		 :tag         "Cancel"
		 :button-face 'ciao-button-widget-face
		 :pressed-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-cancel-button-widget-hook
		 "Push button")
  (widget-insert "  ")
  (widget-create 'push-button
		 :tag         "Apply"
		 :tag-glyph   ciao-ok-img
		 :button-face 'ciao-button-widget-face
		 :pressed-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-ok-button-widget-hook
		 "Push button")
  (widget-create 'push-button
		 :tag         "Apply"
		 :button-face 'ciao-button-widget-face
		 :pressed-face 'ciao-button-widget-face
		 :format      "%[ %t %]"
		 :action      'ciao-ok-button-widget-hook
		 "Push button"))

(defun ciao-create-widget-buffer-from-list (str-list)
  "Create the widgets from str list."
  
  (ciao-create-widgets-buffer)
  (switch-to-buffer (get-buffer-create ciao-ciaopp-gmenu-buffer-name))
  (widget-insert "\n")
  (ciao-create-widgets-from-list 1 str-list)

  (ciao-create-widgets-buttons)
  (ciao-run-widget-buffer))


;; Provide ourselves:

(provide 'ciao-widgets)

;;; ciao-widgets.el ends here
