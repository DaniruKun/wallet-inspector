(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fiveam :jsown)))

(in-package :cl-user)

(defpackage wallet-inspector-test
  (:use :cl
   :fiveam
   :jsown
   :wallet-inspector))

(in-package :wallet-inspector-test)

(def-suite location
  :description "Test location creation and printing.")

(in-suite location)

(test make-location-from-json
  (let ((result (wallet-inspector::make-location-from-json (jsown:new-js
                                                             ("longitude" 50)
                                                             ("latitude" 60)
                                                             ("relevantText" "Building A")
                                                             ("altitude" 10)))))
    (is (equalp (wallet-inspector::create-location 50 60 "Building A" 10) result))))

(def-suite barcode
  :description "Test barcode creation and printing.")

(in-suite barcode)

(test make-barcode-from-json
  (let ((result (wallet-inspector::make-barcode-from-json (jsown:new-js
                                                            ("message" "1q2w3e4r")
                                                            ("messageEncoding" "iso-8859-1")
                                                            ("format" "PKBarcodeFormatQR")))))
    (is (equalp (wallet-inspector::create-barcode "1q2w3e4r" "iso-8859-1" "PKBarcodeFormatQR" "") result))))

(def-suite pass-boarding-pass
  :description "Test boarding pass creation.")

(in-suite pass-boarding-pass)

(test make-pass-boarding-pass-from-json
  (let ((result (wallet-inspector::make-pass-boarding-pass-from-json (jsown:new-js
                                                                       ("auxiliaryFields" '((jsown:new-js ("key" "val"))))
                                                                       ("headerFields" nil)
                                                                       ("primaryFields" nil)
                                                                       ("secondaryFields" nil)
                                                                       ("backFields" nil)
                                                                       ("transitType" "PKTransitTypeAir")))))
    (is (equalp (wallet-inspector::make-pass-boarding-pass :auxiliary-fields '((jsown:new-js ("key" "val")))
                                                           :header-fields nil
                                                           :primary-fields nil
                                                           :secondary-fields nil
                                                           :back-fields nil
                                                           :transit-type "PKTransitTypeAir") result))))
