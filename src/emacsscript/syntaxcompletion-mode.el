(defvar syntaxcomplete-mode-map nil)
(setq syntaxcomplete-mode-map (make-sparse-keymap))

(define-key syntaxcomplete-mode-map [(tab)] 'complete)

(defun toList (str)
  (split-string str "\n"))

(setq isSimple "True")

(defun complete ()
  (interactive)

  (connect-server)
  (send-cursorPos_and_isSimple)

  (set-process-filter 
   (get-process "syntaxa") 
   (lambda (process output) 
     ))
  (disconnect-server)

  (connect-server)
  (send-buffer-upto-cursor)
  (disconnect-server)

  (connect-server)
  (set-process-filter
   (get-process "syntaxa") 
   (lambda (process output)
     (cond ((string= output "LexError") ; Lexical error
	    (message "There is some lexical error up to the cursor position."))
	   ((string= output "ParseError") ; Parse error
	    (message "There is some parse error up to the cursor position."))
	   ((string= output "SuccessfullyParsed") ; Successfully parsed
	    (message "Successfully parsed so that there are no candidates."))
	   (t 
	    (let* ((outputList (toList output))
		   (cands (cdr outputList))
		   )
	      (if (= (length cands) 1)
		  (insert (car cands))
		(let ((name (popup-menu* cands)))
		  (insert name)))))))))

(defun connect-server ()
  (setq buf (get-buffer-create "syntax1"))
  (setq server (open-network-stream "syntaxa" buf "localhost" 50000))
)

(defun disconnect-server ()
  (delete-process server)
)

(defun send-cursorPos_and_isSimple () 
  (process-send-string 
   server 
   (concat (number-to-string (point)) " " isSimple)
   )
  )

(defun send-buffer ()
  (interactive)
  (process-send-string 
   server 
    (buffer-string)
   )
  )

(defun send-buffer-upto-cursor ()
  (interactive)
  (process-send-string 
   server 
    (buffer-substring 1 (point))
   )
  )

(defun syntaxcomplete-mode ()
  "Syntax complete Mode (Simple)"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Syntax complete (simple)")
  (setq major-mode 'syntaxcomplete-mode)
  (setq isSimple "True")
  
  (use-local-map syntaxcomplete-mode-map)
  (run-hooks 'syntaxcomplete-mode-hook))

(defun syntaxcomplete-mode-nested ()
  "Syntax complete Mode (Nested)"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Syntax complete nested)")
  (setq major-mode 'syntaxcomplete-mode)
  (setq isSimple "False")
  
  (use-local-map syntaxcomplete-mode-map)
  (run-hooks 'syntaxcomplete-mode-hook))

(provide 'syntaxcomplete-mode)
