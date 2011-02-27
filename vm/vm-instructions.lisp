(in-package :psvm)

;;; VM-Opcodes.
;;; ==========

;;; The super-structure for all opcodes.

(defstruct (vm-opcode
	     (:conc-name :inst-)))

(defgeneric execute-opcode (vm-opcode vm)
  (:documentation "Execute VM-OPCODE on the virtual machine VM."))

#+(or)
(define-vm-opcode foo 0 (index-1 index-2)
  (list index-1 index-2))

#+(or)
(define-vm-opcode bar 1 ((arg-1 0.0 :type double-float))
  (sin arg-1))

(define-vm-opcode lvar 1 (frame slot)
  (push (aref (aref vm frame) slot)
	stack))

(define-vm-opcode lset 2 (frame slot)
  (setf (aref (aref vm frame) slot)
	(top stack)))