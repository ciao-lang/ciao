;; nt-bash.el  -- Setup NTEmacs to use bash.exe as its shell.
;;
;;  Minimal setup that seems to work for Steve, (tm).

(require 'which)

(defvar bash-bash-exe (which "bash"))


(if bash-bash-exe
    (progn
      (setq -unix-filesystems t)
      (setq shell-file-name bash-bash-exe)
      (setq explicit-shell-file-name shell-file-name)
      (setq explicit-bash.exe-args '("-login" "-i"))
      (setq shell-command-switch "-c")
      (setq archive-zip-use-pkzip nil)
      (setenv "SHELL" shell-file-name)
      (setq win32-quote-process-args t)
      (setq comint-process-echoes t)
      )
  (message "bash.el : Cannot find bash.exe in the System PATH.  Aborted"))


(provide 'nt-bash)
