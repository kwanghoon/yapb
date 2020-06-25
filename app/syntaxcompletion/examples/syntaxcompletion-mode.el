(defvar syntaxcomplete-mode-map nil)
(setq syntaxcomplete-mode-map (make-sparse-keymap))

(define-key syntaxcomplete-mode-map [(tab)] 'complete)

(defun toList (str)
  (split-string str "\n"))

;; (defun run-server ()
;;   (start-process-shell-command "completionServer" "foo" "./a.out")
;; )

(defun complete ()
  (interactive)
;  (run-server)

  (connect-server)
  (send-cursorPosition (point))

  (set-process-filter 
   (get-process "syntaxa") 
   (lambda (process output) 
;     (message "output: %s" output)
     ))
  (disconnect-server)

  (connect-server)
;  (send-buffer)
  (send-buffer-upto-cursor)
  (disconnect-server)

  (connect-server)
  (set-process-filter
   (get-process "syntaxa") 
   (lambda (process output) 
;     (message (format "output: %s" output))
     (let* ((outputList (toList output))
;	    (cursorlen (string-to-number (car outputList)))
	    (cands (cdr outputList))
;	    (foo (message (format "cands: %s" cands)))
;	    (foo (message (format "outputList: %s" outputList)))
;	    (foo (message (format "output: %s" output)))
	    )
;       (if (/= cursorlen -1)
	   (let ((name (popup-menu* cands)))
;	     (message (format "popup: %s" name)) 
;	     (message (format "cursorlen: %s" cursorlen))
;	     (delete-backward-char cursorlen)
	     (insert name)))))
;  (disconnect-server)
  )

(defun connect-server ()
  (setq buf (get-buffer-create "syntax1"))
  (setq server (open-network-stream "syntaxa" buf "localhost" 50000))
;  (message "connect-server")
)

(defun disconnect-server ()
  (delete-process server)
;  (message "disconnect-server")
)

;; (defun send-string ()
;;   (interactive)
;;   (process-send-string server (read-from-minibuffer ">")))

(defun send-cursorPosition (point) 
  (process-send-string 
   server 
   (number-to-string point)
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
  "Syntax complete Mode"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Syntax complete")
  (setq major-mode 'syntaxcomplete-mode)
  
  (use-local-map syntaxcomplete-mode-map)
  (run-hooks 'syntaxcomplete-mode-hook))
 
(provide 'syntaxcomplete-mode)
