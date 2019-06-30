(asdf:defsystem "presage"
  :depends-on ("presage/utilities")
  :serial t
  :components ())

(asdf:defsystem "presage/utilities"
  :serial t
  :pathname "utilities"
  :components ((:file "packages")
               (:file "test-utils")
               (:file "string-utils")
               (:file "logging")
               (:file "byte-utils")
               (:module "crypto"
                        :serial t
                        :components ((:file "sha256")
                                     (:file "hmac")))))

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

(asdf:defsystem "presage/http-server"
  :depends-on ("presage/utilities")
  :serial t
  :pathname "http"
  :components ((:file "packages")
               (:file "parser")
               (:file "server")))
