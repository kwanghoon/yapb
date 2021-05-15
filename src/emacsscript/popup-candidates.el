(defun maxlen-aux (cands max)
  (if cands
      (let ((len (length (car cands))))
        (if (> len max)
            (maxlen-aux (cdr cands) len)
          (maxlen-aux (cdr cands) max)))
    max))

(defun maxlen (cands)
  (maxlen-aux cands 0))
 	
(defun current-line ()
  "Return the vertical position of point"
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun insert-newlines (n)
  (if (= n 0) t
    (insert ?\n)
    (insert-newlines (- n 1))))

(defun delete-newlines (n)
  (save-excursion
    (goto-char (point-max))
    (when (> n 0)
      (save-excursion
        (if (= (char-before) ?\n)
            (delete-char -1))
        (delete-newlines (- n 1))))))

(defun delovls (ovlvec index)
  (when (> index 0) 
    (delete-overlay (aref ovlvec (- index 1)))
    (delovls ovlvec (- index 1))))

(defun focus (i)
  (let ((candstr (aref candstrvec i))
        (o (aref ovlvec i))
        (ws (aref wsvec i))
        (after (aref afterstrvec i))
        )
    (if (not after)
        (overlay-put o 'face focused-back)
      (add-face-text-property 0 width focused-back nil candstr)
      (overlay-put o 'after-string (concat ws candstr)))))

(defun unfocus (i)
  (let ((candstr (aref candstrvec i))
        (o (aref ovlvec i))
        (ws (aref wsvec i))
        (after (aref afterstrvec i))
        )
    (if (not after)
        (overlay-put o 'face unfocused-back)
      (add-face-text-property 0 width unfocused-back nil candstr)
      (overlay-put o 'after-string (concat ws candstr)))))

(defun make-cand (ccList)
  (if (null ccList)
      ""
    (let* ((color (car ccList))
           (cand (cadr ccList)))
      (cond ((equal color "white")
             (concat cand " " (make-cand (cddr ccList))))
            ((equal color "gray")
             (let ((line (caddr ccList))
                   (column (cadr (cddr ccList))))
               (add-face-text-property 0 (length cand) gray-foreground nil cand)
               (concat cand " " (make-cand (cddr (cddr ccList))))))
            (t (throw 'color t))))))

(defun deleteEmpStr (strList)
  (if (null strList)
      nil
    (let ((s (car strList)))
      (if (equal s "")
          (deleteEmpStr (cdr strList))
        (cons s (deleteEmpStr (cdr strList)))))))

(defun make-cands (cands)
  (if (null cands)
      nil
    (let* ((cc (car cands))
           (ccList (deleteEmpStr (split-string cc " ")))
           (cand (make-cand ccList)))
      (cons cand (make-cands (cdr cands))))))

(defun popup-cands (cands_)
  (let* ((cands (make-cands cands_))
         (width (maxlen cands))
         (col (current-column))
         (line (current-line))
         (num_of_cands (length cands))
         (num_of_lines_after_cursor
          (save-excursion
            (goto-char (point-max))
            (- (current-line) line)))
         (num_of_newlines
          (if (< num_of_lines_after_cursor
                 num_of_cands)
              (- num_of_cands num_of_lines_after_cursor)
            0)))
    (setq ovlvec (make-vector num_of_cands nil))
    (setq candstrvec (make-vector num_of_cands nil))
    (setq wsvec (make-vector num_of_cands nil))
    (setq afterstrvec (make-vector num_of_cands nil))
    (save-excursion
      ;; insert newlines if there do not exist enough lines
      ;; to show candidates.
      ;; Inserted newlines will be deleted after completion.
      (goto-char (point-max))
      (insert-newlines num_of_newlines))
    (save-excursion
      (popup-cands-aux cands col width ovlvec 0 0))
    (setq cond t)
    (setq i 0)
    (while cond
      (save-excursion (setq c (read-char)))
      (cond
       ((= c ?\r)
        (let ((cand (nth i cands)))
          (remove-text-properties 0 (length cand) '(face nil) cand)
          (insert cand)
          (setq cond nil)))
       ((= c ?\C-n)
        (when (< i (- (length cands) 1))
          (unfocus i)
          (setq i (+ i 1))
          (focus i)
          ))
       ((= c ?\C-p)
        (when (> i 0)
          (unfocus i)
          (setq i (- i 1))
          (focus i)
          ))
       (t (setq cond nil))
      ))
    (delovls ovlvec num_of_cands) ;; Delete overlays
    (delete-newlines num_of_cands) ;; Delete inserted newlines
    ))

(setq unfocused-back '(:background "blue"))
(setq focused-back '(:background "brightmagenta"))
(setq gray-foreground '(:foreground "brightblack"))

(defun popup-cand (cand col width ovlvec index back)
  (let ((eolc
         (save-excursion (end-of-line) (current-column)))
        (eolp
         (save-excursion (end-of-line) (point)))
        (candstr
         (concat cand
                 (make-string (- width (length cand)) ? ))))
    (aset candstrvec index candstr)
    (if (< col eolc)
        (if (<= width (- eolc col))
            (let ((o (make-overlay (point) (+ (point) width))))
              (aset ovlvec index o)
              (overlay-put o 'display candstr)
              (overlay-put o 'face back)
              )
          (let ((o (make-overlay (point) eolp)))
            (aset ovlvec index o)
            (overlay-put o 'display candstr)
            (overlay-put o 'face back)
            ))
      (let ((o (make-overlay (point) (point)))
            (ws (make-string (- col eolc) ? )))
        (aset ovlvec index o)
        (aset wsvec index ws)
        (aset afterstrvec index t)
        (add-face-text-property 0 width back nil candstr)
        (overlay-put o 'after-string
                     (concat ws candstr))
        )
      )))

(defun popup-cands-aux (cands col width ovlvec index focus)
  (if cands
      (let
          ((back
            (if (= index focus) focused-back unfocused-back)))
        (next-line 1)
        (popup-cand (car cands) col width ovlvec index back)
        (popup-cands-aux (cdr cands) col width ovlvec (+ 1 index) focus))))
