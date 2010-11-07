(in-package :deadlock)

(defclass account ()
  ((money :initform 0 :initarg :money :reader money)
   (paper :initform 0 :initarg :paper :reader paper)))