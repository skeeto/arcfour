;;; ciphersaber.el --- CipherSaber-1 implementation

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl)
(require 'arcfour)

(defun cs-encrypt-buffer (key)
  "Encrypt buffer with CipherSaber-1."
  (interactive "sEnter key: ")
  (let ((iv (cs-generate-iv)))
    (set-buffer-multibyte nil)
    (rc4-buffer (concat key iv))
    (beginning-of-buffer)
    (insert iv)))

(defun cs-decrypt-buffer (key)
  "Decrypt buffer with CipherSaber-1."
  (interactive "sEnter key: ")
  (set-buffer-multibyte nil)
  (rc4-buffer (concat key (cs-retrieve-iv))))

(defun cs-generate-iv ()
  "Generate a 10-byte initialization vector."
  (map 'string #'random (make-vector 10 256)))

(defun cs-retrieve-iv ()
  "Remove initialization vector from buffer and return it."
  (prog1 (buffer-substring-no-properties 1 11)
    (beginning-of-buffer)
    (delete-char 10)))

(provide 'ciphersaber)

;;; ciphersaber.el ends here
