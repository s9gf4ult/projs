(in-package :deadlock)

(define-condition quick-error (simple-error)
  ())
(define-condition quick-structure-failure (quick-error)())

(define-condition not-enought-money (quick-error)
  ((got-money :initform nil :initarg :got-money :reader error-got-money)
   (need-money :initform nil :initarg :need-money :reader error-need-money)))

(define-condition incorrect-position (quick-structure-failure)
  ())
(define-condition incorrect-request (quick-structure-failure)())
(define-condition wrong-period (quick-structure-failure) ())
(define-condition incorrect-quick (quick-structure-failure) ())

(define-condition incorrect-arguments (quick-structure-failure) ())
(define-condition end-of-hystory (quick-error)())
(define-condition can-not-finalize (quick-error) ())