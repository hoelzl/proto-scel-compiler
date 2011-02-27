;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:psc-asd
  (:use :common-lisp :asdf))

(in-package :psc-asd)

(defsystem "psc"
  :version "0.0.1"
  :author "Matthias Hölzl <tc@xantira.com>"
  :maintainer "Matthias Hölzl <tc@xantira.com>"
  :license "BSD sans advertising (see file COPYING for details)"
  :description "Implementation of the Proto SCEL Compiler."
  :long-description "Common Lisp implementation of a language vague resembling SCEL."
  :depends-on ("alexandria")
  :serial t
  :components ((:file "packages")
	       (:file "utils"
		      :depends-on ("packages"))
	       (:module vm
			:serial t
			:components ((:file "vm-macros")
				     (:file "vm-opcodes")
				     (:file "vm")))
	       (:module compiler
			:components ())))