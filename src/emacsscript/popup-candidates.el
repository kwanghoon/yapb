(defun cand-len (cand)
  (if cand
      (let ((len (length (car cand))))
        (+ len 1 (cand-len (cdr cand))))
    0))

(defun maxlen-aux (cands max)
  (if cands
      (let ((len (cand-len (car cands))))
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

(defun make-cand (ccList index)
  (if (null ccList)
      nil
    (let ((color (car ccList))
           (cand (cadr ccList)))
      (cond ((equal color "white")
             (cons cand (make-cand (cddr ccList) (+ 1 index))))
            ((equal color "gray")
             (let* ((line (string-to-number (caddr ccList)))
                    (column (string-to-number (cadr (cddr ccList))))
                    (pt
                     (save-excursion
                       (goto-line line)
                       (move-to-column (- column 1))
                       (point)))
                    )
               (add-face-text-property
                0 (length cand) overlapping-foreground nil cand)
               (add-text-properties
                0 (length cand) (list 'comment pt) cand)
               (cons cand (make-cand (cddr (cddr ccList)) (+ 1 index)))))
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
           (cand (make-cand ccList 0)))
      (cons cand (make-cands (cdr cands))))))

(defun isWhiteChar (c)
  (let ((str (char-to-string c)))
    (or (equal str " ") (equal str "\t") (equal str "\n"))))

(defun isGray (str)
  (numberp (get-text-property 0 'comment str)))

(defun insert-cand (cand offset)
  (if (null cand)
      t
    (let* ((str (car cand))
           (pt (get-text-property 0 'comment str))) 
      (if (numberp pt) ;; If gray symbol (when point is in comment of str)
          (progn ;; Gray symbols are not insert and cursor is moved forward.
            (goto-char (+ pt offset))
            (forward-char (length str))) 
        (progn
          (insert str) ;; White symbols are inserted.
          (setq offset (+ offset (length str))))
        )
      (if (consp (cdr cand))
          (if (or (and (isGray (cadr cand))
                       (not (isWhiteChar (char-after (point)))))
                  (not (isGray (cadr cand))))
              (progn (insert " ")
                     (setq offset (+ 1 offset)))
            ))
      (insert-cand (cdr cand) offset))))

(defun popup-cands (cands_)
  (let* ((num_of_cands (length cands_))
         (cands (make-cands cands_))
         (width (maxlen cands))
         (col (current-column))
         (line (current-line))
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
        (let* ((cand (nth i cands)))
          (insert-cand cand 0)
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
(setq overlapping-foreground '(:foreground "brightblack"))

(defun strlist2str (strList)
  (if (null strList)
      ""
    (concat (car strList) " " (strlist2str (cdr strList)))))

(defun popup-cand (cand col width ovlvec index back)
  (let ((eolc
         (save-excursion (end-of-line) (current-column)))
        (eolp
         (save-excursion (end-of-line) (point)))
        (candstr
         (concat (strlist2str cand)
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
