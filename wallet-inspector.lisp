(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:unix-opts :jsown :zip :flexi-streams)))

(defpackage :wallet-inspector
  (:use :cl)
  (:export :toplevel))

(in-package :wallet-inspector)

(defstruct pass
  "Structure defining an Apple Wallet Pass."
  (description (error "description is missing") :type string)
  (format-version 1 :type integer) ;; required
  (organization-name (error "organizationName is missing") :type string)
  (pass-type-identifier (error "passTypeIdentifier is missing") :type string)
  (serial-number (error "serialNumber is missing") :type string)
  (team-identifier (error "teamIdentifier is missing") :type string)
  relevant-date
  locations
  barcodes
  event-ticket
  boarding-pass
  store-card
  coupon
  generic)

(defstruct pass-fields
  "A structure that represents the groups of fields that display information on the front and back of a pass."
  auxiliary-fields
  back-fields
  header-fields
  primary-fields
  secondary-fields)

(defstruct (pass-barcode (:constructor create-barcode
                           (message message-encoding format alt-text)))
  "Structure that represents a barcode on a Pass."
  alt-text
  format
  message
  message-encoding)

(defmacro => (key)
  "Macro to safely get the value of a KEY from a JSON Jsown object in the surrounding scope.
E.g. (=> \"somekey\")"
  `(jsown:val-safe json ,key))

(defun make-barcode-from-json (json)
  "Create a barcode struct from a JSOWN object."
  (create-barcode
   (=> "message")
   (=> "messageEncoding")
   (=> "format")
   (or (=> "altText") "")))

(defstruct (pass-location (:constructor create-location
                         (longitude latitude relevant-text altitude)))
  "Structure that represents a location that the system uses to show a relevant pass."
  longitude
  latitude
  altitude
  relevant-text)

(defun make-location-from-json (json)
  "Create a location struct from a JSOWN object."
  (create-location
   (=> "longitude")
   (=> "latitude")
   (or (=> "relevantText") "")
   (or (=> "altitude") 0)))

(defstruct (pass-boarding-pass (:include pass-fields))
  "A struct that represents the groups of fields that display the information for a boarding pass."
  transit-type)

(defun make-pass-boarding-pass-from-json (json)
  (make-pass-boarding-pass
   :auxiliary-fields (=> "auxiliaryFields")
   :header-fields (=> "headerFields")
   :primary-fields (=> "primaryFields")
   :secondary-fields (=> "secondaryFields")
   :back-fields (=> "backFields")
   :transit-type (=> "transitType")))

(defstruct (pass-generic (:include pass-fields))
  "A struct that represents the groups of fields that display the information for a generic pass.")

(defun make-pass-generic-from-json (json)
  "Create a pass event ticket struct from a JSOWN object."
  (make-pass-generic
   :auxiliary-fields (=> "auxiliaryFields")
   :header-fields (=> "headerFields")
   :primary-fields (=> "primaryFields")
   :secondary-fields (=> "secondaryFields")
   :back-fields (=> "backFields")))

(defun make-pass-from-json (json)
  "Creates a pass struct from a JSOWN object."
  ;; (let ((locations '('kek))))
  (make-pass
   :description (=> "description")
   :format-version (=> "formatVersion")
   :organization-name (=> "organizationName")
   :pass-type-identifier (=> "passTypeIdentifier")
   :serial-number (=> "serialNumber")
   :team-identifier (=> "teamIdentifier")
   :relevant-date (=> "relevantDate")
   :locations (mapcar #'make-location-from-json (=> "locations"))
   :barcodes (mapcar #'make-barcode-from-json (=> "barcodes"))
   :event-ticket (make-pass-generic-from-json (=> "eventTicket"))
   :boarding-pass (make-pass-boarding-pass-from-json (=> "boardingPass"))
   :store-card (make-pass-generic-from-json (=> "storeCard"))
   :coupon (make-pass-generic-from-json (=> "coupon"))
   :generic (make-pass-generic-from-json (=> "generic"))))

(defun pass-info (pathname)
  (zip:with-zipfile (pass-file pathname)
    (let* ((pass-info (zip:get-zipfile-entry "pass.json" pass-file))
           (bytes (zip:zipfile-entry-contents pass-info))
           (json (jsown:parse (flexi-streams:octets-to-string bytes :external-format :utf-8))))
      (make-pass-from-json json))))

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help"))

(defun toplevel ()
  (multiple-value-bind (options free-args)
    (opts:get-opts)
  (when (getf options :help)
    (opts:describe
         :prefix "You are in my app. Usage:"
         :args "[keywords]")
        (uiop:quit))

  (when (getf options :nb) (format t "NB"))))
