;;; hashe --- a simple hash table implementation in elisp.

;;; Commentary:
;; I've read (Steve Yegge in his Ejacs post) that hash tables in elisp
;; are too heavyweight.  This got me thinking about what's involved in
;; a hash table, so I decided to write one in elisp.  Of course, this is
;; even more heavyweight.
;;
;; This implementation stores keys as strings only for simplicity.
;;
;;; Usage:
;; Put hashe.el in your emacs load path then, for example in IELM:
;;
;; ELISP> (require 'hashe)
;; hashe
;; ELISP> (setq h (make-hashe))
;; [cl-struct-hashe 16 0 0 1.0 1.5 hashe-default-function
;;                  [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]]
;;
;; ELISP> (hashe-put h 'key 'value)
;; value
;; ELISP> (hashe-get h "key")
;; value
;; ELISP> (hashe-get h 'key)
;; value
;; ELISP> (hashe-put h "foo" "bar")
;; "bar"
;; ELISP> (hashe-keys h)
;; ("key" "foo")
;; ELISP> (hashe-values h)
;; (value "bar")
;; ELISP> (hashe-to-alist h)
;; (("key" . value)
;;  ("foo" . "bar"))
;; ELISP> (hashe-get (hashe-from-plist (hashe-to-plist h)) 'foo)
;; "bar"
;;
;;; License:
;; Copyright © Andrew Kirkpatrick
;; This work is made available under the Artistic License 2.0
;; See http://www.perlfoundation.org/artistic_license_2_0
;;
;;; Code:

(require 'cl)
(require 'ert)

(defstruct (hashe (:constructor hashe--create))
  "A stuct for a simple hash table"
  (num-buckets 16)
  (num-used-buckets 0)
  (num-elements 0)
  (grow-threshold 1.0)
  (grow-factor 1.5)
  (function 'hashe-default-function)
  buckets)

(defun hashe-default-function (max key)
  "Convert key (a string) into a number 0..max-1
Adapted from the AP hash function by Arash Partow,
see http://www.partow.net/programming/hashfunctions/"
  (loop with h = #b10101010101010101010101010101 ;; 29 bits
        with n = 0
        for c across key
        do
        (if (zerop (% n 2))
            (setq h (logxor h (logxor (ash h 7) (* c (ash h -3)))))
          (setq h (logxor h (lognot (logxor (+ (ash h 11) c) (ash h -5))))))
        (incf n)
        finally return (% (abs h) max)))

(defun make-hashe (&optional num-buckets hash-function)
  "Construct and return a hashe table. Accepts optional
parameters num-buckets and hash-function."
  (let ((hash (hashe--create)))
    (when (not num-buckets)
      (setf num-buckets (hashe-num-buckets hash)))
    (setf (hashe-buckets hash) (make-vector num-buckets nil))
    (setf (hashe-num-buckets hash) num-buckets)
    (setf (hashe-function hash) (or hash-function 'hashe-default-function))
    hash))

(defsubst hashe--stringify (key)
  "Stringify whatever type the key is, optimized for string and
symbol."
  (cond
   ((stringp key) key)
   ((symbolp key) (symbol-name key))
   (t (format "%s" key))))

(defmacro hashe--setup (hash key &rest body)
  "Wrapper for common use in hashe-get, -exists, -put, -del functions,
providing to the function body:

key-str  - stringified key
buckets  - vector of hash buckets
slot-idx - index into the vector according to the hash function
slot     - element of the vector referred to by slot-idx"
  (declare (indent defun))
  (let ((hash-sym (gensym "hash")))
    `(let* ((,hash-sym ,hash)
            (key-str (hashe--stringify ,key))
            (buckets (hashe-buckets ,hash-sym))
            (slot-idx (funcall
                       (hashe-function ,hash-sym)
                       (hashe-num-buckets ,hash-sym) key-str))
            (slot (elt buckets slot-idx)))
       ,@body)))

(defmacro hashe--iter (hash var &rest body)
  "Wrapper to consistently iterate over all elements in the table."
  (declare (indent defun))
  (let ((bucket-sym (gensym "bucket")))
    (assert (and var (symbolp var)) t "var parameter must be a symbol.")
    `(loop for ,bucket-sym across (hashe-buckets ,hash) do
           (loop for ,var in ,bucket-sym do
                 ,@body))))

(defun hashe-get (hash key &optional no-error-p)
  "Search `HASH' for `KEY' to obtain the value.

If the key is found, returns the corresponding value.  If the key
is not found an error is thrown, unless `NO-ERROR-P' is a true,
value in which case nil is returned."
  (hashe--setup hash key
    (loop for elt in slot do
          (when (string= (car elt) key-str)
            (return (cdr elt)))
          finally do
          (if no-error-p
              (return nil)
            (error "key not found in hashe object")))))

(defun hashe-exists (hash key)
  "Searches the hash for key to determine its presence.
Returns t if found, nil otherwise."
  (hashe--setup hash key
    (loop for elt in slot do
          (when (string= (car elt) key-str)
            (return t))
          finally
          return nil)))

(defun hashe-put (hash key val)
  "Sets the value associated with key to val. May trigger a hash resize.
Returns t if the hash element was created, nil if it was
overwritten"
  (hashe--setup hash key
    (let (replaced)
      (setf replaced
            (loop for elt in slot
                  do (when (string= (car elt) key-str)
                       ;; replacement
                       (setf (cdr elt) val)
                       (return t))))

      (unless replaced
        ;; creation
        (incf (hashe-num-elements hash))
        (when (not slot)
          (incf (hashe-num-used-buckets hash)))
        (setf (elt buckets slot-idx)
              (cons (cons key-str val) slot)))

      (unless replaced
        (hashe-maybe-resize hash)))
    val))

(defun hashe-del (hash key)
  "Deletes the entry associated with key. Does not trigger a hash resize.
Returns t if a hash element was deleted, nil if it was not found"
  (hashe--setup hash key
    (loop with idx = 0
          for elt in slot do
          (when (string= (car elt) key-str)
            ;; deletion
            (if (eq idx 0)
                (setf (elt buckets slot-idx) (delq elt slot))
              (delq elt slot))
            (decf (hashe-num-elements hash))
            (when (not (elt buckets slot-idx))
              (decf (hashe-num-used-buckets hash)))
            (return t)
            finally
            ;; not found
            nil))
    ;; (hashe-maybe-resize hash) ;; only call this if sizing down is desired
    ))

(defun hashe-map (hash func &optional cons-cell)
  "Function which maps over the hash, passing each element to the
function provided. If cons-cell is non-nil, func should accept
just one argument a cons cell with the key in the car and the
value in the cdr. Otherwise func should
;accept two arguments, the key and value."
  (hashe--iter hash elt
    (if cons-cell
        (funcall func elt)
      (funcall func (car elt) (cdr elt)))))

(defun hashe-keys (hash)
  "Returns a list of keys in the hash table."
  (let (keys)
    (hashe--iter hash elt
      (push (car elt) keys))
    keys))

(defun hashe-values (hash)
  "Returns a list of values in the hash table."
  (let (keys)
    (hashe--iter hash elt
      (push (cdr elt) keys))
    keys))

(defun hashe-to-alist (hash)
  "Convenience function to convert a hash to an alist.
Returns a new alist with the existing data."
  (let (alist)
    (hashe--iter hash elt
      (push elt alist))
    alist))

(defun hashe-from-alist (alist)
  "Convenience function to convert an alist to a hash. If a key is repeated,
the value associated with the last occurence will overwrite the
previous one.  Returns a new hash with the existing data."
  (let ((hash (make-hashe (length alist))))
    (loop for (key . val) in alist do
          (hashe-put hash key val))
    hash))

(defun hashe-to-plist (hash)
  "Convenience function to convert a hash to a plist.
Returns a new plist with the existing data."
  (let (plist)
    (hashe--iter hash elt
      (push (cdr elt) plist)
      (push (car elt) plist))
    plist))

(defun hashe-from-plist (plist)
  "Convenience function to convert an plist to a hash. If a key is repeated,
the value associated with the last occurence will overwrite the
previous one.  Returns a new hash with the existing data."
  (let ((hash (make-hashe (/ (1+ (length plist)) 2))))
    (loop for (key val) on plist by 'cddr do
          (hashe-put hash key val))
    hash))

(defun hashe-maybe-resize (hash)
  "Check if the hash should be resized, according to its stored thresholds"
  (let (
        (element-bucket-ratio (/ (float (hashe-num-elements hash))
                                 (hashe-num-buckets hash)))
        new-size)
    (setq new-size
          (cond
           ((> element-bucket-ratio (hashe-grow-threshold hash))
            (floor (* (hashe-grow-factor hash) (hashe-num-buckets hash))))
           (t nil)))
    (when new-size
      (when (< new-size 8) (setf new-size 8))
      ;;(message "resizing hash from %d to %d buckets" (hashe-num-buckets hash) new-size)
      (hashe-resize hash new-size))))

(defun hashe-resize (hash new-size)
  "Resizes the hash to have new-size buckets"
  (when (not (eq new-size (length (hashe-buckets hash))))
    (let ((oldvec (hashe-buckets hash)))
      (setf (hashe-buckets hash) (make-vector new-size nil))
      (setf (hashe-num-buckets hash) new-size)
      (setf (hashe-num-used-buckets hash) 0)
      (setf (hashe-num-elements hash) 0)
      (loop for bucket across oldvec do
            (loop for drop in bucket do
                  (hashe-put hash (car drop) (cdr drop)))))))

(defun hashe-stats (hash)
  "Return an alist of stats such as bucket use ratio, element to
bucket ratio, maximum bucket population and possibly others."
  (list
   (cons 'bucket-use-ratio (/ (float (hashe-num-used-buckets hash))
                              (hashe-num-buckets hash)))
   (cons 'element-bucket-ratio (/ (float (hashe-num-elements hash))
                                  (hashe-num-buckets hash)))
   (cons 'max-bucket-population (loop for bucket across (hashe-buckets hash)
                                      maximizing (length bucket)))))


;;; Tests

(defmacro* hashe--test-fixture (n-buckets func &body body)
  (declare (indent defun))
  (let ((args (list n-buckets)))
    (when func
      (setq args (append args (list func))))
    `(let* ((hash-table (apply 'make-hashe ',args)))
       ,@body)))

(ert-deftest hashe-constructor-test ()
  (hashe--test-fixture 100 nil
    (should (eq (type-of hash-table) 'vector))
    (should (eq (elt hash-table 0) 'cl-struct-hashe))
    (should (= (hashe-num-buckets hash-table) 100))
    (should (= (length (hashe-buckets hash-table)) 100))
    (should (zerop (hashe-num-elements hash-table)))))

(ert-deftest hashe-elements-test ()
  (hashe--test-fixture 20 nil
    (loop for i from 1 upto 40
          do (hashe-put hash-table (format "%d" i) (format "%d^2=%d" i (* i i))))
    (should (= (hashe-num-elements hash-table) 40))

    (hashe-del hash-table "16")
    (should (= (hashe-num-elements hash-table) 39))

    (loop for i from 41 upto 81
          do (hashe-put hash-table (format "%d" i) (format "%d^2=%d" i (* i i))))
    (should (= (hashe-num-elements hash-table) 80))

    (hashe-map hash-table (lambda (k v) (hashe-del hash-table k)))
    (should (= (hashe-num-elements hash-table) 0))))

(ert-deftest hashe-transform-test ()
  (let* ((alist '(("one" . 1) ("two" . 2)))
         (plist '("three" 3 "four" 4))
         (ahash (hashe-from-alist alist))
         (phash (hashe-from-plist plist)))
    (should (equalp (hashe-to-plist phash) plist))
    (should (equalp (hashe-to-alist ahash) alist))))

(defun hashe-run-tests ()
  "Run the hashe package test suite."
  (ert "^hashe-"))


(provide 'hashe)
;;; hashe.el ends here
