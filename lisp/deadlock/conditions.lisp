(in-package :deadlock)

(define-condition not-enought-money (simple-error)
  ((got-money :initarg :got-money)
   (need-money :initarg :need-money)))