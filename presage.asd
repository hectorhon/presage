(asdf:defsystem "presage"
  :depends-on ("presage/utilities")
  :serial t
  :components ((:file "packages")
               (:file "http/parser")
               (:file "http/server")))

(asdf:defsystem "presage/utilities"
  :serial t
  :pathname "utilities"
  :components ((:file "packages")
               (:file "test-utils")
               (:file "string-utils")
               (:file "logging")
               (:file "byte-utils")))

(asdf:defsystem "presage/pg-client"
  :depends-on ("presage/utilities")
  :serial t
  :pathname "pg-client"
  :components ((:file "packages")
               (:file "read-write")
               (:file "message")
               (:file "frontend-messages")
               (:file "backend-messages")
               (:file "interface")))
