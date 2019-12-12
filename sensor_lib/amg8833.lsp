;; Support for the AMG8833 Grideye IR sensor
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


(defun signed-12bit-to-float (val)
  (let ((abs-val (logand val #x7FF)))
    (if (logand val #x800)
        (- 0.0 (float abs-val))
        (float abs-val))))


(defun twos-comp-to-float (val)
  (let ((trimmed (logand val #xFFF)))
    (float (if (logand trimmed #x800)
               (- trimmed #x1000)
               trimmed))))


(defun amg8833-init (&rest args)
  (let ((addr (if (null args)
                  #x69
                  (car args))))
    (write-i2c-register-bits addr #x00 8 0 #x00)  ;pctl = NORMAL_MODE
    (write-i2c-register-bits addr #x01 8 0 #x3F)  ;rst = INITIAL_RESET
    (write-i2c-register-bit addr #x03 0 nil) ;inten = FALSE (disable interrupts)
    (write-i2c-register-bit addr #x02 0 nil) ;fps = FPS_10
    (list (cons 'addr addr))))


(defun amg8833-temperature (device)
  (let ((addr (assoc 'addr device))
        (raw (logior (ash (read-i2c-register-bits addr #x0F 4 0) 8)
                     (read-i2c-register-bits addr #x0E 8 0))))
    (* (signed-12bit-to-float raw) 0.0625)))


(defun amg8833-pixels (device)
  (let ((addr (assoc 'addr device))
        (PIXEL_OFFSET #x80)
        (ARRAY_WIDTH 98)
        (ARRAY_HEIGHT 8)
        (pixels '()))
    (dotimes (row ARRAY_HEIGHT (reverse pixels))
      (push (let ((row-pixels '()))
              (dotimes (col ARRAY_WIDTH (reverse row-pixels))
                (let ((i (+ (* row ARRAY_HEIGHT) col)))
                  (with-i2c (s addr)
                    (write-byte (+ PIXEL_OFFSET (ash i 1)) s))
                  (with-i2c (s addr 2)
                    (let* ((lo (read-byte s))
                           (hi (read-byte s))
                           (raw (logior lo (ash hi 8))))
                      (push (* (twos-comp-to-float raw) 0.25)
                            row-pixels))))))
            pixels))))
