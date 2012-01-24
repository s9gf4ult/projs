(defpackage #:pack1
  (:use #:cl #:clsql)
  (:export classlist))

(in-package #:pack1)

(defparameter classlist '(person employee company address employee-address deferred-employee-address))

(def-view-class person ()
  ((height :db-kind :base :accessor height :type float
           :initarg :height)
   (married :db-kind :base :accessor married :type boolean
            :initarg :married)
   (birthday :type clsql:wall-time :initarg :birthday)
   (bd-utime :type clsql:universal-time :initarg :bd-utime)
   (hobby :db-kind :virtual :initarg :hobby :initform nil)))

(def-view-class employee (person)
  ((emplid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :emplid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (first-name
    :accessor first-name
    :type (varchar 30)
    :initarg :first-name)
   (last-name
    :accessor last-name
    :type (varchar 30)
    :initarg :last-name)
   (email
    :accessor employee-email
    :type (varchar 100)
    :initarg :email)
   (ecompanyid
    :type integer
    :initarg :companyid)
   (company
    :accessor employee-company
    :db-kind :join
    :db-info (:join-class company
			  :home-key ecompanyid
			  :foreign-key companyid
			  :set nil))
   (managerid
    :type integer
    :initarg :managerid)
   (manager
    :accessor employee-manager
    :db-kind :join
    :db-info (:join-class employee
			  :home-key managerid
			  :foreign-key emplid
			  :set nil))
   (addresses
    :accessor employee-addresses
    :db-kind :join
    :db-info (:join-class employee-address
			  :home-key emplid
			  :foreign-key aemplid
			  :target-slot address
			  :set t)))
  (:base-table employee))

(def-view-class company ()
  ((companyid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :companyid)
   (groupid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :groupid)
   (name
    :type (varchar 100)
    :initarg :name)
   (presidentid
    :type integer
    :initarg :presidentid)
   (president
    :reader president
    :db-kind :join
    :db-info (:join-class employee
			  :home-key presidentid
			  :foreign-key emplid
			  :set nil))
   (employees
    :reader company-employees
    :db-kind :join
    :db-info (:join-class employee
			  :home-key (companyid groupid)
			  :foreign-key (ecompanyid groupid)
			  :set t))))

(def-view-class address ()
  ((addressid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :addressid)
   (street-number
    :type integer
    :initarg :street-number)
   (street-name
    :type (varchar 30)
    :void-value ""
    :initarg :street-name)
   (city
    :column "city_field"
    :void-value "no city"
    :type (varchar 30)
    :initarg :city)
   (postal-code
    :column zip
    :type integer
    :void-value 0
    :initarg :postal-code))
  (:base-table addr))

;; many employees can reside at many addressess
(def-view-class employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
	    :db-info (:join-class address
				  :home-key aaddressid
				  :foreign-key addressid
				  :retrieval :immediate)))
  (:base-table "ea_join"))

(def-view-class deferred-employee-address ()
  ((aemplid :type integer :initarg :emplid)
   (aaddressid :type integer :initarg :addressid)
   (verified :type boolean :initarg :verified)
   (address :db-kind :join
	    :db-info (:join-class address
				  :home-key aaddressid
				  :foreign-key addressid
				  :retrieval :deferred
				  :set nil)))
  (:base-table "eax_join"))