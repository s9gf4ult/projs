(asdf:defsystem kladr
  :depends-on (:clsql :iterate)
  :components ((:file "packages")
               (:file "kladr" :depends-on ("packages"))))