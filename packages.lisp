(in-package :cl-user)

(defpackage :proto-scel-utilities
  (:use :common-lisp :alexandria)
  (:nicknames :psu)
  (:export))

(defpackage :proto-scel-compiler
  (:use :common-lisp :alexandria :psu)
  (:nicknames :psc))

(defpackage :proto-scel-vm
  (:use :common-lisp :alexandria :psu)
  (:nicknames :psvm))

;; (pushnew :optimize-vm *features*)