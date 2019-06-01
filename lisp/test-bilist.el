;;; test-bilist.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Dale Sedivec

;; Author: Dale Sedivec <dale@codefu.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'bilist)
(require 'buttercup)

(describe "A bilist"
  :var (bilist)

  (it "supports creation with no max length"
    (setq bilist (bilist-new)))

  (it "is initially empty"
    (expect (zerop (bilist-len bilist))))

  (it "can have items added to the end"
    (bilist-append bilist 'a)
    (bilist-append bilist 'b)
    (bilist-append bilist 'c)
    (expect (bilist-len bilist) :to-equal 3))

  (it "can read by index from the head"
    (expect (bilist-ref bilist 0) :to-be 'a)
    (expect (bilist-ref bilist 1) :to-be 'b))

  (it "can read by index from the tail"
    (expect (bilist-ref bilist -1) :to-be 'c)
    (expect (bilist-ref bilist -2) :to-be 'b))

  (it "throws an error for out-of-bounds access"
    (expect (bilist-ref bilist 3) :to-throw 'bilist-out-of-range))

  (it "throws an error for negative out-of-bounds access"
    (expect (bilist-ref bilist -4) :to-throw 'bilist-out-of-range))

  (it "can return a default instead of error on out-of-bounds access"
    (expect (bilist-ref bilist 3 nil nil 'my-default) :to-be 'my-default))

  (it "ignores the default argument when accessing a valid index"
    (expect (bilist-ref bilist 2 nil nil 'another-default) :to-be 'c))

  (it "can insert elements at head"
    (bilist-insert bilist 0 'x)
    (bilist-insert bilist 0 'y)
    (expect (bilist-len bilist) :to-equal 5))

  (it "lets you insert after the pointer"
    (let ((bilist (bilist-new)))
      (bilist-insert bilist 0 'first t t)
      (bilist-insert bilist 1 'second t t)
      (bilist-insert bilist 1 'third t t)
      (expect (bilist-values bilist) :to-equal '(first second third))))

  (it "can delete all elements"
    (bilist-delete-range bilist)
    (expect (bilist-len bilist) :to-equal 0))

  (it "can be created with a list of initial elements"
    (setq bilist (bilist-new '(a b c d e)))
    (expect (bilist-len bilist) :to-equal 5))

  (it "can return all of its elements in a list"
    (expect (bilist-values bilist) :to-equal '(a b c d e)))

  (it "can use its pointer to iterate forward through its elements"
    (bilist-set-ptr-idx bilist 0)
    (expect (bilist-ref bilist 0 t) :to-be 'a)
    (expect (cl-loop
               repeat 4
               collect (bilist-ref bilist 1 t t))
            :to-equal '(b c d e)))

  (it "can use its pointer to iterate backward through its elements"
    (bilist-set-ptr-idx bilist 5)
    (expect (cl-loop
               repeat 5
               collect (bilist-ref bilist -1 t t))
            :to-equal '(e d c b a)))

  (it "can expire elements from the head"
    (let ((bilist (bilist-new (number-sequence 1 10) 10 'head)))
      (expect (bilist-values bilist) :to-equal (number-sequence 1 10))
      (bilist-append bilist 11)
      (expect (bilist-values bilist) :to-equal (number-sequence 2 11))))

  (it "can expire elements from the tail"
    (let ((bilist (bilist-new (number-sequence 10 1 -1) 10 'tail)))
      (expect (bilist-values bilist) :to-equal (number-sequence 10 1 -1))
      (bilist-insert bilist 0 11)
      (expect (bilist-values bilist) :to-equal (number-sequence 11 2 -1))))

  (it "can expire elements from either head or tail based on ptr and ratio"
    (let ((bilist (bilist-new (number-sequence 100 109) 10 0.5)))
      (bilist-set-ptr-idx bilist 5)
      (expect (bilist-ref bilist 0 t) :to-equal 105)
      (bilist-insert-bilist bilist (bilist-new '(a b c d e f)))
      (expect (bilist-values bilist) :to-equal '(a b c d e f 103 104 105 106))
      (expect (bilist-ptr-idx bilist) :to-equal 8)
      (expect (bilist-ref bilist 0 t) :to-equal 105)))

  (it "supports `seq-into'"
    (let ((bilist (bilist-new (number-sequence 65 74))))
      (expect (seq-into bilist 'list) :to-equal (number-sequence 65 74))
      (expect (seq-into bilist 'vector)
              :to-equal (vconcat (number-sequence 65 74)))
      (expect (seq-into bilist 'string) :to-equal "ABCDEFGHIJ")))

  (it "can be iterated over with `bilist-do'"
    (let ((bilist (bilist-new '(a b c d)))
          items)
      (bilist-do (item bilist)
        (push item items))
      (expect items :to-equal '(d c b a))))

  (it "can be iterated over backwards with `bilist-do'"
    (let ((bilist (bilist-new '(a b c d)))
          items)
      (bilist-do (item bilist :backward t)
        (push item items))
      (expect items :to-equal '(a b c d))))

  (it "can be empty and iterated over with `bilist-do'"
    (expect (bilist-do (_ (bilist-new) :result 42)) :to-equal 42))

  (it "can be empty and iterated over backwards with `bilist-do'"
    (expect (bilist-do (_ (bilist-new) :result 123 :backward t))
            :to-equal 123)))

(provide 'test-bilist)
;;; test-bilist.el ends here

;; Local Variables:
;; eval: (buttercup-minor-mode)
;; End:
