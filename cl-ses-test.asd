#|
  This file is a part of cl-ses project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ses-test-asd
  (:use :cl :asdf))
(in-package :cl-ses-test-asd)

(defsystem cl-ses-test
  :author "Cody Reichert"
  :license ""
  :depends-on (:cl-ses
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-ses"))))
  :description "Test system for cl-ses"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
