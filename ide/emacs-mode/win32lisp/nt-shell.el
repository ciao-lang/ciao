;; nt-shell.el -- Sample of how to setup CMD.EXE as the shell for GNU Emacs.
;;  Posted to the NTEmacs mailing list by Howard Melman
;; <howard@silverstream.com>
;;

;; first some basic variables
(setq explicit-cmdproxy.exe-args '("/Q"))
(setq explicit-cmd.exe-args '("/q"))
(setq explicit-sh.exe-args '("-i"))

(setq shell-prompt-pattern "^[a-zA-Z]:\[^>]*>")
(setq shell-cd-regexp "cd\\( /d\\)?")

;; this prevents ^M's in shell buffers
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; The standard definition has more unix shell specific highlighting
;; that highlights too much (job control, lines with colons?!?)
(setq shell-font-lock-keywords
  '((eval . (cons shell-prompt-pattern 'font-lock-warning-face))))

;; Use dirtrack.el to do directory tracking based on the prompt
(setq dirtrack-list '("^\\([a-zA-Z]:[\\][^>\r\n]*\\)>" 1))

(add-hook 'shell-mode-hook
          (function (lambda ()
                      (setq comint-preoutput-filter-functions
                            (append (list 'dirtrack)
                                    comint-preoutput-filter-functions)))))

;; I don't know why shell-mode doesn't have a local abbrev table
;; but it seems like a reasonable thing.
;; I really want it since cmd.exe requires "cd /d" to
;; let you cd to a path containing a different drive
(defvar shell-mode-abbrev-table nil
  "Abbrev table used while in shell mode.")

(define-abbrev-table 'shell-mode-abbrev-table '(
    ("cd" "cd /d" nil 2)
    ))

(defun hrm-shell-hook ()
  "Run when entering shell mode."
  (setq shell-dirtrackp nil)            ; I'm using dirtrack
  (define-key shell-mode-map "\C-cr" 'rename-uniquely)
  (setq local-abbrev-table shell-mode-abbrev-table)
  (abbrev-mode 1))

(add-hook 'shell-mode-hook 'hrm-shell-hook)

(provide 'nt-shell)