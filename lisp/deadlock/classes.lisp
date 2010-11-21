(in-package :deadlock)

(defclass quick()
  ((name :initform "quick" :initarg :name :reader quick-name)
   (subaccounts :initform (list (make-instance 'subaccount)) :initarg :subaccounts :reader quick-subaccounts :documentation "список субаккаунтов доступных для работы")
   (money-holds :initform nil :initarg :money-holds :reader quick-money-holds :documentation "список удержаний средств")
   (requests :initform nil :initarg :requests :reader quick-requests :documentation "список заявок")
   (positions :initform nil :initarg :positions :reader quick-positions :documentation "список открытых позиций")
   (log :initform (make-instance 'quick-log) :initarg :log :reader quick-log :documentation "специальный объект логирования")
   (instruments :initform nil :initarg :instruments :reader quick-instuments :documentation "список торговых инструментов"))
  (:documentation "сущность надо которой будут проводиться все операции и котороая хранит состояние торгового терминала"))
   

(defclass subaccount()
  ((name :initform "subaccount" :initarg :name :reader subaccount-name)
   (free-money :initform nil :initarg :free-money :reader subaccount-free-money :documentation "свободные средства на счете"))
  (:documentation "класс описывающий субсчет"))

(defclass money-hold ()
  ((subaccount :initarg :subaccount :reader money-hold-subaccount :documentation "ссылка на субсчет в котором удержаны денежные средства")
   (money :initform 0 :initarg :money :reader money-hold-money :documentation "количество денег удержанных в данной сущности"))
  (:documentation "сущность олицетворяющая удержанные средства (ГО при открытии позиции удерживается)"))

(defclass request ()
  ((direction :initarg :direction :reader request-direction :documentation "must be :long or :short")
   (instrument :initarg :instrument :reader request-instrument :documentation "ссылка на ценную бумагу")
   (count :initform 1 :initarg :count :reader request-direction
   
   
   