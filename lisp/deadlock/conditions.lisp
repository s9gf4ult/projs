(in-package :deadlock)

(define-condition quick-error (simple-error)
  ())

(define-condition not-enought-money (quick-error)
  ((got-money :initform nil :initarg :got-money :reader error-got-money)
   (need-money :initform nil :initarg :need-money :reader error-need-money)))

(define-condition incorrect-position (quick-error)
  ())
(define-condition wrong-period (quick-error) ())
(define-condition incorrect-quick (quick-error) ())

(define-condition incorrect-arguments (quick-error) ())
(define-condition end-of-hystory (quick-error)())
(define-condition can-not-finalize (quick-error) ())