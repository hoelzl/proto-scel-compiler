(in-package :psvm)

(defstruct (vm-instruction
	     (:conc-name :inst-)))

(defgeneric execute-instruction (vm-instruction vm)
  (:documentation "Execute VM-INSTRUCTION on the virtual machine VM."))

#+(or)
(define-vm-instruction foo 0 (index-1 index-2)
  (list index-1 index-2))

#+(or)
(define-vm-instruction bar 1 ((arg-1 0.0 :type double-float))
  (sin arg-1))

(define-vm-instruction lvar 1 (frame slot)
  (push (aref (aref vm frame) slot)
	stack))