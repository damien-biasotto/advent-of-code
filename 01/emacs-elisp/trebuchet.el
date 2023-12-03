(require 'seq)

(defun trebuchet (file-path)
  (seq-reduce (lambda (carry item) (+ carry item)) (parse-input (string-split (load-input file-path))) 0))
  

(defun load-input (file-path)
  "Load the content of a file into a temp-buffer (but returns it as a string)"
 (decode-coding-string (with-temp-buffer
			 (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file-path nil nil nil)
    (buffer-substring-no-properties (point-min) (point-max))) 'utf-8))

(defun parse-input (str)
  "Extract all the digits from the string, then either returns the number as it is, or concat the first and last digit, or repeat the single digit"
  (mapcar (lambda (x) (let ((only-alpha (remove-alpha x))) (cond
							    ((length= only-alpha 2) (string-to-number only-alpha))
							    ((length< only-alpha 2) (string-to-number (concat only-alpha only-alpha)))
							    ((length> only-alpha 2) (string-to-number (concat (char-to-string(car (string-to-list only-alpha))) (char-to-string (car(last(string-to-list only-alpha))))))))))
							    str))


(defun remove-alpha (str)
  "Removes alphabetical characters for a given string"
  (replace-regexp-in-string "[a-z]+" "" str))


(provide 'trebuchet)
