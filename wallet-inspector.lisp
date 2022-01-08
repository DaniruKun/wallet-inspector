(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:unix-opts :jsown :zip :flexi-streams)))

(defpackage :wallet-inspector
  (:use :cl)
  (:export :toplevel))

(in-package :wallet-inspector)

;; Constants ---------------------------------------------------------

(defconstant version "0.1.0")

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

(defun print-barcode (barcode)
  "Pretty print a BARCODE struct of type PASS-BARCODE."
  (format t "  Msg: ~10a MsgEnc: ~10a Fmt: ~20a AltTxt: ~10a~%"
          (pass-barcode-message barcode)
          (pass-barcode-message-encoding barcode)
          (pass-barcode-format barcode)
          (pass-barcode-alt-text barcode)))

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

(defun print-location (location)
  "Print a LOCATION struct to stdout in a pretty format."
  (format t "  Lon: ~10a Lat: ~10a Alt: ~6a RelText: ~20a~%"
          (pass-location-longitude location)
          (pass-location-latitude location)
          (pass-location-altitude location)
          (pass-location-relevant-text location)))

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

(defun print-field-props (field)
  "Print all props of a FIELD which is a JSOWN object."
  (jsown:do-json-keys (keyword value) field
    (format T "    ~16A : ~10A~&" keyword value)))

(defun print-pass-boarding-pass (pass-boarding-pass)
  "Pretty print a PASS-BOARDING-PASS struct."
  (format t "  Transit Type~%  ~a~%" (pass-boarding-pass-transit-type pass-boarding-pass))
  (format t "  Primary Fields~%")
  (mapcar #'print-field-props (pass-boarding-pass-primary-fields pass-boarding-pass))
  (format t "  Secondary Fields~%")
  (mapcar #'print-field-props (pass-boarding-pass-secondary-fields pass-boarding-pass))
  (format t "  Auxiliary Fields~%")
  (mapcar #'print-field-props (pass-boarding-pass-auxiliary-fields pass-boarding-pass))
  (format t "  Back Fields~%")
  (mapcar #'print-field-props (pass-boarding-pass-back-fields pass-boarding-pass))
  (format t "  Header Fields~%")
  (mapcar #'print-field-props (pass-boarding-pass-header-fields pass-boarding-pass)))

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

(defun print-pass-generic (pass-generic)
  "Pretty print a PASS-GENERIC struct."
  (let ((prim-fields (pass-generic-primary-fields pass-generic))
        (sec-fields (pass-generic-secondary-fields pass-generic))
        (aux-fields (pass-generic-auxiliary-fields pass-generic))
        (bck-fields (pass-generic-back-fields pass-generic))
        (head-fields (pass-generic-header-fields pass-generic)))
    (format t "  Primary Fields~%")
    (mapcar #'print-field-props prim-fields)
    (format t "  Secondary Fields~%")
    (mapcar #'print-field-props sec-fields)
    (format t "  Auxiliary Fields~%")
    (mapcar #'print-field-props aux-fields)
    (format t "  Back Fields~%")
    (mapcar #'print-field-props bck-fields)
    (format t "  Header Fields~%")
    (mapcar #'print-field-props head-fields)))

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

(defun print-pass (pass)
  "Print the contents of a PASS."
  (let ((general-info `(("Description" ,#'pass-description)
                        ("Format Version" ,#'pass-format-version)
                        ("Org Name" ,#'pass-organization-name)
                        ("Pass Type Identifier" ,#'pass-pass-type-identifier)
                        ("Serial Number" ,#'pass-serial-number)
                        ("Team Identifier" ,#'pass-team-identifier)
                        ("Relevant Date" ,#'pass-relevant-date)))
        (event-ticket (pass-event-ticket pass))
        (store-card (pass-store-card pass))
        (coupon (pass-coupon pass))
        (generic (pass-generic pass))
        (boarding-pass (pass-boarding-pass pass)))
    (format t "~%General~%")
    (mapcar #'(lambda (field)
                (format t "~22a: ~a~%" (first field) (apply (second field) (list pass))))
            general-info)
    (format t "~%Locations~%")
    (mapcar #'print-location (pass-locations pass))
    (format t "~%Barcodes~%")
    (mapcar #'print-barcode (pass-barcodes pass))
    (format t "~%Fields~%")
    (when (pass-boarding-pass-transit-type boarding-pass)
      (print-pass-boarding-pass boarding-pass))
    (when (pass-generic-primary-fields event-ticket)
      (print-pass-generic event-ticket))
    (when (pass-generic-primary-fields store-card)
      (print-pass-generic store-card))
    (when (pass-generic-primary-fields coupon)
      (print-pass-generic coupon))
    (when (pass-generic-primary-fields generic)
      (print-pass-generic generic))))

(defun print-pass-info (pathname)
  (zip:with-zipfile (pass-file pathname)
    (let* ((pass-info (zip:get-zipfile-entry "pass.json" pass-file))
           (bytes (zip:zipfile-entry-contents pass-info))
           (json (jsown:parse (flexi-streams:octets-to-string bytes :external-format :utf-8)))
           (pass (make-pass-from-json json)))
      (print-pass pass))))

;; CLI ---------------------------------------------------------------

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :version
   :description "Print the tool version"
   :short #\v
   :long "version"))

(defun toplevel ()
  (multiple-value-bind (options free-args)
      (opts:get-opts)
    (when (getf options :version)
      (format t "Version ~a" version))
    (when (or (getf options :help))
      (opts:describe
       :prefix (format nil "
Apple Wallet Inspector Tool

Usage:~%  wallet-inspector <pass-file> [options]~%")
       :args "[keywords]"
       :suffix (format nil "Example:~%  wallet-inspector ticket.pkpass~%"))
      (uiop:quit))
    (if free-args
        (print-pass-info (first free-args))
        (format t "No pass file supplied. See usage example with -h"))))
