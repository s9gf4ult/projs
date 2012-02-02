(require '#:postmodern)

(defpackage #:pmtest
  (:use #:cl #:postmodern))

(in-package #:pmtest)

(defclass employee ()
  ((id :col-type integer :primary-key t :default (:nextval blahblah))
   (name :col-type string :initarg :name :accessor employee-name)
   (email :col-type string :initarg :email :accessor employee-email))
  (:metaclass dao-class)
  (:yes id))

