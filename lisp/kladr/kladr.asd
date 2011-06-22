(asdf:defsystem #:kladr
  :depends-on (#:sqlite #:iterate #:cl-gtk2-gtk #:uuid #:cl-libxml2)
  :components ((:file "packages")
               (:file "sqlite")
               (:file "kladr" :depends-on ("packages" "sqlite"))))