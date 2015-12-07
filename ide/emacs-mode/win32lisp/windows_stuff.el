
;; See also:
;; http://www.khngai.com/emacs/cygwin.php

;; 
;; This contains a series of definitions which may be useful when
;; running under MS-Windows (depends on the version of emacs)
;; 

;;command.com shell on Win95/98
;;
;;The latest end-of-line handling code in Emacs prevents
;;command.com from being used as the interactive shell on
;;Win95 out of the box. To use command.com as your shell,
;;place the following in your startup file: 

(setq process-coding-system-alist
      '(("cmdproxy" . (raw-text-dos . raw-text-dos))))

;; Preventing ctrl-m's from being printed in the shell
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Preventing shell commands from being echoed
(defun my-comint-init ()
  (setq comint-process-echoes t))

(add-hook 'comint-mode-hook 'my-comint-init)

;; Complete directories with "\"
(add-hook 'shell-mode-hook
	  '(lambda () (setq comint-completion-addsuffix '("\\" . "")))
	  t)

