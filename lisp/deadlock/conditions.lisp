(in-package :deadlock)

(define-condition quick-error (simple-error)
  ())

(define-condition not-enought-money (quick-error)
  ((got-money :initform nil :initarg :got-money :reader error-got-money)
   (need-money :initform nil :initarg :need-money :reader error-need-money)))

(define-condition incorrect-position (quick-error)
  ())

