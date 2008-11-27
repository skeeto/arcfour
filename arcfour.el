;; Emacs Lisp Arcfour Implementation

;;  Key schedule algorithm
;;
;; for i from 0 to 255
;;   S[i] := i
;; endfor
;; j := 0
;; for i from 0 to 255
;;   j := (j + S[i] + key[i mod keylength]) mod 256
;;   swap(S[i],S[j])
;; endfor
;;
;;  Random generator algorithm
;;
;; i := 0
;; j := 0
;; while GeneratingOutput:
;;   i := (i + 1) mod 256
;;   j := (j + S[i]) mod 256
;;   swap(S[i],S[j])
;;   output S[(S[i] + S[j]) mod 256]
;; endwhile

;;----------------------------------------------------------------------
;; Arcfour functions

(defun rc4-init-state ()
  "Initialize the arcfour state vector."
  (interactive)
  (setq rc4-state (make-vector 256 0))
  (setq rc4-i 0)
  (setq rc4-j 0)
  (let (i)
    (dotimes (i 256 rc4-state)
      (aset rc4-state i i))))

(defun rc4-swap (i j)
  "Swap two elements in the state vector."
  (let ((temp (aref rc4-state i)))
    (aset rc4-state i (aref rc4-state j))
    (aset rc4-state j temp)))

(defun rc4-key-sched (key)
  "Arcfour key-scheduler: initalize state from key."
  (interactive "sEnter key: ")
  (let ((j 0) i)
    (dotimes (i 256 rc4-state)
      (setq j (% (+ j 
		    (aref rc4-state i) 
		    (aref key (% i (length key)))) 256))
      (rc4-swap i j))))

(defun rc4-gen-byte ()
  "Generate a single byte."
  (interactive)
  (setq rc4-i (% (1+ rc4-i) 256))
  (setq rc4-j (% (+ rc4-j (aref rc4-state rc4-i)) 256))
  (rc4-swap rc4-i rc4-j)
  (aref rc4-state (% (+ (aref rc4-state rc4-i) 
			(aref rc4-state rc4-j)) 256)))

;;----------------------------------------------------------------------
;; Porcelain

(defun rc4-eight-bytes (key)
  "Generate eight bytes as a vector from the random generator."
  (rc4-init-state)
  (rc4-key-sched key)
  (let ((rlist (make-vector 8 0)) n)
    (dotimes (n 8 rlist)
      (aset rlist n (rc4-gen-byte)))))

(defun rc4-region (start end key)
  "Encrypt/decrypt region with arcfour using given key."
  (interactive "r\nsEnter key: ")
  (rc4-init-state)
  (rc4-key-sched key)
  (save-excursion
    (let (c)
      (goto-char start)
      (while (< (point) end)
	(setq c (char-after))
	(delete-char 1)
	(insert-char (logxor c (rc4-gen-byte)) 1)))))

(defun rc4-buffer (key)
  "Encrypt/decrypt entire buffer with arcfour."
  (interactive "sEnter key: ")
  (rc4-region (point-min) (point-max) key))

;;----------------------------------------------------------------------
;; Test vectors

(defun vector= (v1 v2)
  (if (not (= (length v1) (length v2))) nil
    (let ((n 0)
	  (pass t))
      (while (and (< n (length v1)) pass)
	(setq pass (= (aref v1 n) (aref v2 n)))
	(setq n (1+ n)))
      pass)))

(vector= (rc4-eight-bytes [?\x01 ?\x23 ?\x45 ?\x67 ?\x89 ?\xAB ?\xCD ?\xEF])
	 [?\x74 ?\x94 ?\xC2 ?\xE7 ?\x10 ?\x4B ?\x08 ?\x79])

(vector= (rc4-eight-bytes "Testkey")
	 [13 14 210 177 148 194 209 68])
