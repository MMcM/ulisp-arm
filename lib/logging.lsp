;; Logging for ulisp
;;
;; Licensed under the following MIT licence:
;;
;; Copyright (c) 2019 Dave Astels
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defvar *LOGGER:LEVEL* 3)

(defun logger:set-level (level)
  (setf *LOGGER:LEVEL* level))

(defun logger:do-log (level-name log-level format-str &rest args)
  (when (<= log-level *LOGGER:LEVEL*)
    (let ((full-format (concatenate 'string "~A - ~A (~A): " format-str)))
      (dolist (arg (list log-level level-name (millis)))
        (push arg args))
      (eval `(format t ,full-format @args)))))


(defmacro logger:define-level (name level-value)
  (let ((macro-name (intern (format nil "log~a" name))))
    `(defmacro ,macro-name (format-str &rest args)
       `(logger:do-log ,,name ,,level-value ,format-str @args))))

(logger:define-level :emerg 1)
(logger:define-level :alert 2)
(logger:define-level :crit 3)
(logger:define-level :error 4)
(logger:define-level :warn 5)
(logger:define-level :notice 6)
(logger:define-level :info 7)
(logger:define-level :debug 8)
(logger:define-level :debug1 9)
(logger:define-level :debug2 10)
(logger:define-level :debug3 11)
(logger:define-level :debug4 12)
