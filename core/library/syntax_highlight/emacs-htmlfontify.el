;; ---------------------------------------------------------------------------
;; Call htmlfontify on selected file
;;
;; Arguments: modename inputfile outputfile
;; ---------------------------------------------------------------------------

;; open a file, check fontification, if fontified, write a fontified copy
;; to the destination directory, otherwise just copy the file:
(defun my-hfy-copy-and-fontify-file (modename srcdir file target)
  ""
  (let ((source nil)
        (html   nil))
    (cd srcdir)
    (with-temp-buffer
      (insert-file-contents file)
      (funcall (intern (concat modename "-mode")))
      (if (not (hfy-opt 'skip-refontification)) (hfy-force-fontification))
      (if (or (hfy-fontified-p) (hfy-text-p srcdir file))
          (progn (setq html  (hfy-fontify-buffer srcdir file))
                 (set-buffer  html)
                 (write-file  target)
                 (kill-buffer html))
        (copy-file file target 'overwrite)))))

(let ((modename (pop argv)) 
      (input (pop argv)) 
      (output (pop argv))) 
  (let ((srcdir (or (file-name-directory input) "./")) 
        (file (file-name-nondirectory input)))
    (require 'htmlfontify) 
    (htmlfontify-load-rgb-file) 
    (let ((hfy-page-header '(lambda (file style) ""))
	  (hfy-page-footer '(lambda (file) ""))
	  (hfy-display-class '((type . x-toolkit) 
                               (class . color) 
                               (background . light)))) 
      (my-hfy-copy-and-fontify-file modename srcdir file output)))) 

