;;; arcfour.el --- RC4 stream cipher implementation

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;;  Key schedule algorithm:

;; for i from 0 to 255
;;   S[i] := i
;; endfor
;; j := 0
;; for i from 0 to 255
;;   j := (j + S[i] + key[i mod keylength]) mod 256
;;   swap(S[i],S[j])
;; endfor

;;  Random generator algorithm:

;; i := 0
;; j := 0
;; while GeneratingOutput:
;;   i := (i + 1) mod 256
;;   j := (j + S[i]) mod 256
;;   swap(S[i],S[j])
;;   output S[(S[i] + S[j]) mod 256]
;; endwhile

;;; Code:

(require 'cl)

(defstruct rc4
  "State of an RC4 cipher."
  (state (coerce (number-sequence 0 255) 'vector))
  (i 0) (j 0))

(defun rc4--swap (rc4 i j)
  "Swap two elements in the state vector."
  (let ((temp (aref (rc4-state rc4) i)))
    (aset (rc4-state rc4) i (aref (rc4-state rc4) j))
    (aset (rc4-state rc4) j temp)))

(defun rc4-key-schedule (rc4 key)
  "Arcfour key-scheduler: initialize state from key."
  (let ((j 0) i)
    (dotimes (i 256 (rc4-state rc4))
      (setq j (% (+ j (aref (rc4-state rc4) i)
                      (aref key (% i (length key)))) 256))
      (rc4--swap rc4 i j))))

(defun rc4-emit (rc4)
  "Generate a single byte from the cipher."
  (setf (rc4-i rc4) (% (+ (rc4-i rc4) 1) 256))
  (setf (rc4-j rc4) (% (+ (rc4-j rc4) (aref (rc4-state rc4) (rc4-i rc4))) 256))
  (rc4--swap rc4 (rc4-i rc4) (rc4-j rc4))
  (aref (rc4-state rc4) (% (+ (aref (rc4-state rc4) (rc4-i rc4))
                              (aref (rc4-state rc4) (rc4-j rc4))) 256)))

;; Interactive functions

(defun rc4-region (start end key)
  "Encrypt/decrypt region with arcfour using given key."
  (interactive "r\nsEnter key: ")
  (set-buffer-multibyte nil)
  (let ((rc4 (make-rc4)))
    (rc4-key-schedule rc4 key)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((c (char-after)))
          (delete-char 1)
          (insert-char (logxor c (rc4-emit rc4)) 1))))))

(defun rc4-buffer (key)
  "Encrypt/decrypt entire buffer with arcfour."
  (interactive "sEnter key: ")
  (rc4-region (point-min) (point-max) key))

;; Test vectors

(require 'ert)

(defun rc4--byte-vector (key count)
  "Generate COUNT bytes as a vector from the cipher."
  (let ((rc4 (make-rc4)))
    (rc4-key-schedule rc4 key)
    (coerce (loop for i from 1 to count collect (rc4-emit rc4)) 'vector)))

(defun rc4--test (key vector)
  (should (equal (rc4--byte-vector key (length vector)) vector)))

(ert-deftest rc4 ()
  "Test the cipher output against test vectors."
  (rc4--test [?\x01 ?\x23 ?\x45 ?\x67 ?\x89 ?\xAB ?\xCD ?\xEF]
             [?\x74 ?\x94 ?\xC2 ?\xE7 ?\x10 ?\x4B ?\x08 ?\x79])
  (rc4--test "Testkey" [13 14 210 177 148 194 209 68]))

(provide 'arcfour)

;;; arcfour.el ends here
