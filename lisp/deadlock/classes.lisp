(in-package :deadlock)

(defclass quick()
  ((name :initform "quick" :initarg :name :reader quick-name)
   (subaccounts :initform (list (make-instance 'subaccount)) :initarg :subaccounts :reader quick-subaccounts :documentation "список субаккаунтов доступных для работы")
   (requests :initform nil :initarg :requests :reader quick-requests :documentation "список заявок")
   (positions :initform nil :initarg :positions :reader quick-positions :documentation "список открытых позиций")
   (log :initform (make-instance 'quick-log) :initarg :log :reader quick-log :documentation "специальный объект логирования")
   (subaccount-instrument :initform nil :initarg :subaccount-instrument :reader quick-subaccount-instrument :documentation "связь многие ко многим между инстрементами и субсчетами на которых они торгуются список таких списков (list subaccount-instrument)")
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
(defmethod initialize-instance :after ((obj money-hold) &rest initargs &key)
  (with-slots (money subaccount) obj
    (declare (type number money)
             (type subaccount subaccount))
    (withdraw-money subaccount money)
    obj))

(defclass request ()
  ((direction :initarg :direction :reader request-direction :documentation  "must be `:buy' or `:sell'")
   (subaccount :initform nil :initarg :subaccount :reader request-subaccount)
   (instrument :initarg :instrument :reader request-instrument :documentation "ссылка на ценную бумагу (торговый инструмент)")
   (count :initform 1 :initarg :count :reader request-count :documentation "количество лотов для покупки")
   (price :initform nil :initarg :price :reader request-price)
   (ttl :initform nil :initarg :ttl :accessor request-ttl :documentation "время жизни заявки, по истечении которого должен вызваться коллбэк")
   (set-date :initform nil :initarg :set-date :reader request-set-date :documentation "дата выставления заявки")
   (execution-date :initform nil :reader request-execution-date :documentation "дата исполнения заявки")
   (overtime-date :initform nil :reader request-overtime-date :documentation "дата просрочки исполнения заявки")
   (state :initform :awaiting :initarg :state :reader request-state :documentation "can be `:awaiting' `:executed' or `:overtimed'")
   (set-callback :initform nil :initarg :set-callback :reader request-set-callback :documentation "коллбэк на установку заявки в торговую систему")
   (execute-callback :initform nil :initarg :execute-callback :reader request-execute-callback :documentation "коллбэк на исполнение заявки")
   (overtime-callback :initform nil :initarg :overtime-callback :reader request-overtime-callback :documentation "коллбэк на просрочку заявки"))
  (:documentation "заявка на покупку - продажу"))
(defmethod shared-initialize :after ((obj request) slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (let ((direction (slot-value obj 'direction))
        (state (slot-value obj 'state)))
    (declare (type (member :sell :buy) direction)
             (type (member :awaiting :executed :overtimed) state))
    obj))

(defclass instrument ()
  ((name :initform nil :initarg :instrument :reader instrument-name :documentation "название ценной бумаги")
   (code :initform nil :initarg :code :reader instrument-code :documentation "код ценной бумаги")
   (buy-go :initform nil :initarg :buy-go :reader instrument-buy-go :documentation "гарантийное обеспечение покупателя")
   (sell-go :initform nil :initarg :sell-go :reader instrument-sell-go :documentation "гарантийное обеспечение продавца")
   (hystory :initform nil :initarg :hystory :reader instrument-hystory :documentation "история изменения цены инструмента"))
  (:documentation "торговый инструмент"))

(defclass open-position ()
  ((code :initform nil :initarg :code :reader open-position-code)
   (direction :initform nil :initarg :direction :reader open-position-direction :documentation "can be `:long' or `:short'")
   (deals :initform nil :initarg :deals :reader open-position-deals :documentation "список сделок по данной позиции")
   (money-holds :initform nil :initarg :money-holds :reader open-position-money-holds :documentation "список удержаний денежных средств по позиции")
   (profit :initform 0 :initarg :profit :reader open-position-profit :documentation "профит при закрытии позиции, вычисляется автоматически")
   (open-date :initform nil :initarg :open-date :reader open-position-open-date)
   (close-date :initform nil :initarg :close-date :reader open-position-close-date))
  (:documentation "открытая позиция по инструменту"))
(defgeneric open-position-instrument (obj)
  (:documentation "возвращает инструмент по которому октрыта позиция, просматривает сделки, приклепленные к позиции, возвращает ошибку если есть сделки по разным инструментам, перегрузок не определено"))
(defmethod open-position-instrument ((obj open-position))
  (let (ret)
    (dolist (di (mapcar #'deal-instrument (open-position-deals obj)))
      (if ret
          (when (not (eq di ret))
            (error (make-condition 'incorrect-position :format-control "there is deals with other papers ~a and ~a" :format-arguments '(ret di))))
          (setf ret di)))
    ret))
(defgeneric open-position-count (obj)
  (:documentation "считает разницу между сделками покупи и сделкми продажи для длинной позиции и наоборот для короткой. Если полученное число отрицательное вызывает исключение, перегрузок не определено"))
(defmethod open-position-count ((obj open-position))
  (let ((buy 0)
        (sell 0))
    (dolist (deal (open-position-deals obj))
      (case (deal-direction deal)
        (:buy (incf buy (deal-count deal)))
        (:sell (incf sell (deal-count deal)))))
    (let ((ret (case (open-position-direction obj)
                 (:long (- buy sell))
                 (:short (- sell buy)))))
      (if (< ret 0)
          (error (make-condition 'incorrect-position :format-control "there is ~a sell and ~a buy deals in ~a position" :format-arguments '(sell buy (open-position-direction obj)))))
      ret)))

(defclass deal ()
  ((date :initform nil :initarg :date :reader deal-date)
   (directtion :initform nil :initarg :dirction :reader deal-direction :documentation "can be `:buy' or `:sell'")
   (instrument :initform nil :initarg :instrument :reader deal-instrument)
   (count :initform 0 :initarg :count :reader deal-count :documentation "количество лотов по инструменту")
   (price :initform 0 :initarg :price :reader deal-price)
   (commission :initform 0 :initarg :commission :reader deal-commission :documentation "комиссия с совершения сделки")))
   
(defclass quick-log ()
  ((file-name :initform nil :initarg :file-name :reader quick-log-file-name)
   (sqlite-handler :initform nil :initarg :sqlite-handler :reader quick-log-sqlite-handler)))

(defmethod shared-initialize :after ((obj quick-log) slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (when (and (quick-log-file-name obj) (not (quick-log-sqlite-handler obj)))
    (let ((con (setf (slot-value obj 'sqlite-handler) (connect (quick-log-file-name obj)))))
      (execute-non-query con "pragma foreign_keys=on")
      (execute-non-query con "create table if not exists positions (id integer primary key not null, open_date integer not null, close_date integer not null, direction varchar not null, profit read not null, instrument varchar not null)")
      (execute-non-query con "create table if not exists deals (id integer primary key not null, direction varchar not null, instrument varchar not null, count real not null, date varchar not null, position integer not null, foreign key (position) references positions(id) on delete cascade)")
      (execute-non-query con "create table if not exists requests (id integer primary key not null, direction varchar not null, instrument varchar not null, count integer not null, price real not null, set_date varchar not null, execution_date varchar, overtime_date varchar, state varchar not null)"))))
      
(defclass strategy () ())

(defclass hystory-data ()
  ((sqlite-handle :initform nil :initarg :sqlite-handle :accessor hystory-sqlite-handle)
   (file-name :initform nil :initarg :file-name :reader hystory-file-name)
   (current-date :initform nil :initarg :current-date :reader hystory-current-date)
   (candle-period :initform :min :initarg :candle-period :reader hystory-candle-period)))
(defmethod shared-initialize :after ((obj hystory-data) slot-names &rest initarts &key)
  (if (not (hystory-sqlite-handle obj))
      (let ((con (setf (hystory-sqlite-handle obj) (connect (hystory-file-name obj)))))
        (unless (hystory-current-date obj)
          (setf (slot-value obj 'current-date)
                (execute-single con "select min(datetime) from candles"))))))
        

(defclass candle()
  ((open :initform nil :initarg :open :reader candle-open)
   (close :initform nil :initarg :close :reader candle-close)
   (high :initform nil :initarg :high :reader candle-high)
   (low :initform nil  :initarg :low :reader candle-low)
   (volume :initform nil :initarg :volume :reader candle-volume)
   (datetime :initform nil :initarg :datetime :reader candle-datetime)
   (datetime-close :initform nil :initarg :datetime-close :reader candle-datetime-close)
   (type :initform nil :reader candle-type)
   (period :initarg :period :initform :sec :reader candle-period :documentation "period can be one symbol of this (:sec :min :hour :day :week :month :year) or list like this (`symbol' number) where `symbol' is one of above listed. It can be number, in this case it will the same as (:sec number)")))
(defmethod shared-initialize :after ((obj candle) slot-names &rest initargs &key)
  (declare (ignore slot-names initargs))
  (setf (slot-value obj 'type) (cond
                                 ((> (candle-close obj) (candle-open obj)) :long)
                                 ((< (candle-close obj) (candle-open obj)) :short)
                                 (t :empty))))
