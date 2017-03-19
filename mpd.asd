;;;; vim: set fenc=utf-8 lisp ic wm=0 et tw=0 sm para=lpppipnpbp sect=shuh ai :
;;;; ---------------------------------------------------------------------------
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:      cl-mpd.lisp
;;;; Purpose:   system definition for CL-MPD
;;;; Author:    Stephen P. Horner
;;;; Started:   2006-04-14
;;;;
;;;; $Id:$
;;;;
;;;; This file is Copyright © 2006 by Stephen P. Horner
;;;;
;;;; ---------------------------------------------------------------------------

(defpackage :mpd-system (:use #:asdf #:cl))
(in-package :mpd-system)

(defsystem :mpd
    :version "0.3"
    :author "Stephen Horner <sphorner@gmail.com>"
    :description "CL-MPD: a common lisp interface to Music Player Daemon."
    :components
    ((:doc-file "README")
     (:static-file "LICENSE")
     (:static-file "COPYING")
     (:static-file "AUTHORS")
     (:file "package")
     (:file "mpdclient" :depends-on ("package"))))

