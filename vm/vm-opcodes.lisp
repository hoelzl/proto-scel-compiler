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
(define-vm-opcode foo 0 (index-1 index-2)
    "Foo Doc"
  (list index-1 index-2))

#+(or)
(define-vm-opcode bar 1 ((arg-1 0.0 :type double-float))
  (sin arg-1))


;;; Variable Handling.
;;; -----------------

(define-vm-opcode lvar 1 (frame slot)
  "Push the lexical variable in FRAME/SLOT on the stack."
  (push (aref (aref vm frame) slot) stack))

(defun top (stack)
  "Return the topmost element of the stack."
  (first stack))

(define-vm-opcode lset 2 (frame slot)
  "Set the lexical variable in FRAME/SLOT to the topmost element of
the stack."
  (setf (aref (aref vm frame) slot) (top stack)))

(define-vm-opcode gvar 3 (name)
  "Return the global variable NAME."
  (push (gethash name globals) stack))

(define-vm-opcode gset 4 (name)
  "Set the global variable NAME to the topmost element of the stack."
  (setf (gethash name globals) (top stack)))

(define-vm-opcode pop 5 ()
  "Pop the stack."
  (pop stack))

(define-vm-opcode const 6 (value)
  "Push a constant value on the stack."
  (push value stack))


;;; Branching.
;;; ---------

(define-vm-opcode jump 10 (label)
  "Jump unconditionally to LABEL."
  (setf pc label))

(define-vm-opcode fjump 11 (label)
  "Pop the stack and jump to LABEL when the popped-off element is
false."
  (when (null (pop stack))
    (setf pc label)))

(define-vm-opcode tjump 12 (label)
  "Pop the stack and jump to LABEL when the popped-off element is
true."
  (when (pop stack)
    (setf pc label)))
