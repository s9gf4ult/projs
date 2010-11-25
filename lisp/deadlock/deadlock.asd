(asdf:defsystem deadlock
  :depends-on (:cl-ppcre :sqlite :alexandria :iterate)
  :components ((:file "packages")
               (:file "common" :depends-on ("packages"))
               (:file "candle" :depends-on ("packages"))
               (:file "account" :depends-on ("packages" "common"))
               (:file "trade-position" :depends-on ("packages"))
               (:file "hystory-data" :depends-on ("packages" "common" "candle"))
               (:file "strategy" :depends-on ("packages" "common" "account" "hystory-data" "trade-position"))
               (:file "classes" :depends-on ("packages" "common"))
               (:file "generics" :depends-on ("packages" "classes"))
               (:file "methods" :depends-on ("packages" "generics"))))