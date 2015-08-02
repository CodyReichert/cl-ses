#|
  This file is a part of cl-ses project.
  Copyright (c) 2015 Cody Reichert (codyreichert@gmail.com)
|#

#|
  Send AWS SES Emails from Common Lisp

  Author: Cody Reichert (codyreichert@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ses-asd
  (:use :cl :asdf))
(in-package :cl-ses-asd)

(defsystem cl-ses
  :version "0.1"
  :author "Cody Reichert"
  :license "BSD 3-Clause"
  :depends-on (:drakma)
  :components ((:module "src"
                :components
                ((:file "cl-ses"))))
  :description "Send AWS SES Emails from Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-ses-test))))
