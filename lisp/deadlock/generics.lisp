(in-package :deadlock)

(defgeneric hold-money (subaccount count)
  (:documentation "удержать нужную сумму денег со счета, возвращает удержание денег"))
(defgeneric withdraw-money (subaccount money)
  (:documentation "withdraw some money from subaccount"))
(defgeneric free-holded-money (money-hold))