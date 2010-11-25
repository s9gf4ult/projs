(in-package :deadlock)

(define-condition not-enought-money (simple-error)
  ((got-money :initarg :got-money :reader error-got-money)
   (need-money :initarg :need-money :reader error-need-money)))