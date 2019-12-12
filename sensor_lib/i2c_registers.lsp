;; Functions for I2C register read/write.
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


;; Reading an writing single-bit values

;; Read a single bit from a register.
;; i2c-addr: the device address
;; reg_addr: The register address to write the bit to
;; bit: The bit index within the byte at register_address
;; register_width: The number of bytes in the register. Defaults to 1.
;; lsb_first: Is the first byte we read from I2C the LSB? Defaults to true

(defun read-i2c-register-bit (i2c-addr reg-addr bit &rest width-order)
  (let* ((register-width (if (null width-order)
                             1
                             (car width-order)))
         (lsb-first (if (null (cdr width-order))
                        t
                        (cadr width-order)))
         (byte (if lsb-first
                   (1+ (floor (/ bit 8)))
                   (- register-width (floor (/ bit 8)))))
         (buffer '()))
    (with-i2c (s i2c-addr)
      (write-byte reg-addr s))
    (delay 25)
    (with-i2c (s reg-addr register-width)
      (setf buffer (dotimes (x register-width (reverse buffer))
                     (push (read-byte s) buffer))))
    (let ((reg-value (nth byte buffer))
          (bitmask (ash 1 (mod bit 8))))
      (not (zerop (logand reg-value bit-mask))))))


;; Write a single bit to a register.
;; i2c-addr: the device address
;; reg-addr: The register address to write the bit to
;; bit: The bit index within the byte at register_address
;; value: boolean bit value to write
;; width-order is ([register-width [lsb-first]]
;; register-width: The number of bytes in the register. Defaults to 1.
;; lsb-first: Is the first byte we read from I2C the LSB? Defaults to t

(defun write-i2c-register-bit (i2c-addr reg-addr bit value &rest width-order)
  (let* ((register-width (if (null width-order)
                             1
                             (car width-order)))
         (lsb-first (if (null (cdr width-order))
                        t
                        (cadr width-order)))
         (byte (if lsb-first
                   (1+ (floor (/ bit 8)))
                   (- register-width (floor (/ bit 8)))))
         (buffer '()))
    (with-i2c (s i2c-addr)
      (write-byte reg-addr s))
    (delay 25)
    (with-i2c (s reg-addr register-width)
      (setf buffer (dotimes (x register-width (reverse buffer))
                     (push (read-byte s) buffer))))
    (let* ((reg-value (nth byte buffer))
           (bitmask (ash 1 (mod bit 8)))
           (new-value (if value
                          (logior reg-value bit-mask)
                          (logand reg-value (lognot bit-mask)))))
      (setf (nth byte buffer) new-value)
      (with-i2c (s i2c-addr)
        (write-byte reg-addr s)
        (dolist (b (if lsb-first
                       (reverse buffer)
                       buffer))
          (write-byte b s))))))



;; Reading and writing multi-bit values

;; Read a multi-bit field from a register.
;; i2c-addr: the device address
;; reg_addr: The register address to write the bit to
;; num-bits: The number of bits in the field.
;; lowest-bit: The lowest bit's index within the byte at register_address
;; width-order is ([register-width [lsb-first]]
;; register_width: The number of bytes in the register. Defaults to 1.
;; lsb_first: Is the first byte we read from I2C the LSB? Defaults to t

(defun read-i2c-register-bits (i2c-addr reg-addr num-bits lowest-bit &rest width-order)
  (let* ((register-width (if (null width-order)
                             1
                             (car width-order)))
         (lsb-first (if (null (cdr width-order))
                        t
                        (cadr width-order)))
         (byte (if lsb-first
                   (1+ (floor (/ bit 8)))
                   (- register-width (floor (/ bit 8)))))
         ((reg-value 0)
          (bitmask (ash (1- (ash 1 num-bits)) lowest_bit))))
    (with-i2c (s i2c-addr)
      (write-byte reg-addr s))
    (delay 25)
    (with-i2c (s reg-addr register-width)
      (dotimes (x register-width)
        (setf reg-value (+ (ash reg-value 8) (read-byte s)))))
    (ash (logand reg-value bit-mask) (* -1 lowest_bit))))


;; Write a multi-bit field toa register.
;; i2c-addr: the device address
;; reg_addr: The register address to write the bit to
;; num-bits: The number of bits in the field.
;; lowest-bit: The lowest bit's index within the byte at register_address
;; value: the value to write
;; width-order is ([register-width [lsb-first]]
;; register_width: The number of bytes in the register. Defaults to 1.
;; lsb_first: Is the first byte we read from I2C the LSB? Defaults to t

(defun write-i2c-register-bits (i2c-addr reg-addr num-bits lowest-bit value &rest width-order)
  (let* ((register-width (if (null width-order)
                             1
                             (car width-order)))
         (lsb-first (if (null (cdr width-order))
                        t
                        (cadr width-order)))
         (reg-value 0)
         (bitmask (ash (1- (ash 1 num-bits)) lowest_bit)))
    (with-i2c (s i2c-addr)
      (write-byte reg-addr s))
    (delay 25)
    (with-i2c (s reg-addr register-width)
      (dotimes (x register-width)
        (setf reg-value (+ (ash reg-value 8) (read-byte s)))))
    (let* ((masked-value (logand reg-value (not bitmask)))
           (shifted-value (ash value lowest_bit))
           (combined-value (logior masked-value shifted-value))
           (buffer '()))
        (dotimes (x register-width)
          (push buffer (logand combined-value #xFF))
          (setf combined-value (ash combined-value -8)))
        (with-i2c (s reg-addr)
          (write-byte reg-addr s)
          (dolist (b (if lsb-first
                         (reverse buffer)
                         buffer))
            (write-byte b s))))))
