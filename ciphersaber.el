;; Emacs Lisp CipherSaber-1 Implementation

(load-file "arcfour.el")

(defun cs-encrypt-buffer (key)
  "Encrypt buffer with CipherSaber-1."
  (interactive "sEnter key: ")
  (set-buffer-multibyte nil)
  (let* ((iv (cs-generate-iv))
	 (cs-key (concat key iv)))
    (rc4-buffer cs-key)
    (beginning-of-buffer)
    (insert iv)))

(defun cs-decrypt-buffer (key)
  "Decrypt buffer with CipherSaber-1."
  (interactive "sEnter key: ")
  (let* ((iv (cs-retrieve-iv))
	 (cs-key (concat key iv)))
    (rc4-buffer cs-key)))

(defun cs-generate-iv ()
  "Generate a 10-byte initialization vector."
  (let ((iv "") i)
    (dotimes (i 10 iv)
      (setq iv (concat iv (char-to-string (random 256)))))))

(defun cs-retrieve-iv ()
  "Remove initialization vector from buffer and return it."
  (beginning-of-buffer)
  (let ((iv "") i)
    (dotimes (i 10 iv)
      (setq iv (concat iv (char-to-string (char-after))))
      (delete-char 1))))
