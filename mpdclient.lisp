;;;; ---------------------------------------------------------------------------
;;;; Name:      mpdclient.lisp
;;;; Purpose:   A Common Lisp interface to MPD.
;;;; Author:    Stephen P. Horner
;;;; Started:   2006-03-28
;;;;
;;;; Copyright © 2006,2017 by Stephen P. Horner - All Rights Reserved.
;;;;
;;;; This project's homepage is: http://www.common-lisp.net/project/cl-mpd
;;;; ---------------------------------------------------------------------------

;;; TODO
;;; Add the code to the primary methods that execute mpd commands that checks
;;; for an active connection to mpd before executing commands, and reconnect
;;; to MPD if necessary.
;;;
;;; TODO
;;; Rewrite my parsers; all of them! This multi-STRING= method needs replacing.
;;;
;;; TODO
;;; Rewrite all the code that is using SUBSEQ to analyze parts of a sequence,
;;; because SUBSEQ conses.


(eval-when (:compile-toplevel :load-toplevel)
  (declaim (optimize (speed 0) (space 0) (safety 1) (debug 3) (compilation-speed 0))))

(defpackage #:mpd
  (:use #:cl))

(in-package #:mpd)

;;; Find out when a REQUIRE is normally evaluated.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-bsd-sockets)
  #+lispworks (require "comm"))

(defconstant +mpd-welcome-message+       "OK MPD ")
;;; These aren't used yet, but will be soon.
(defconstant +mpd-error-not-list+        1)
(defconstant +mpd-error-arg+             2)
(defconstant +mpd-error-password+        3)
(defconstant +mpd-error-permission+      4)
(defconstant +mpd-error-unknown-cmd+     5)
(defconstant +mpd-error-no-exist+       50)
(defconstant +mpd-error-playlist_max+	51)
(defconstant +mpd-error-system+		52)
(defconstant +mpd-error-playlist-load+	53)
(defconstant +mpd-error-update-already+	54)
(defconstant +mpd-error-player-sync+	55)
(defconstant +mpd-error-exist+		56)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug-on* t
  "Either NIL or T, determines if debug routines run or not."))


;;; --------------------------
;;; Utilities
;;; --------------------------

(defmacro fprint (strm &body body)
  "FPRINT equates to forced print, since FORMAT is to dumb to print automagically."
  `(progn
     (format ,strm ,@body)
     (finish-output)))


(defmacro dprint (&body body)
  "Prints debug info if code was compiled with *DEBUG-ON* set to T."
  (if *debug-on*
    `(progn
       (format *debug-io* ,@body)
       (finish-output))))


(defun quit ()
  #+sbcl (sb-ext:quit)
  #+clisp (ext:quit))


(defun fullgc ()
  #+sbcl (sb-ext:gc :full t)
  #+clisp (ext:gc))


;;; --------------------------
;;; Backend code
;;; --------------------------

(defun scrub-newline-chars (str &key verbose)
  (when verbose
    (write-line "SCRUB-NEWLINE-CHARS was passed...")
    (let ((tmp (map 'vector #'(lambda (x)
                                (format t "~@C " x))
                    str)))))
  (substitute #\Space #\Newline str :test #'char=))


(declaim (inline append-newline-char))
(defun append-newline-char (str)
  (concatenate 'string str '(#\Newline)))


#+sbcl
(defun lookup-hostname (host &key ignore-cache)
  (when (stringp host)
    (sb-bsd-sockets:host-ent-address
      (sb-bsd-sockets:get-host-by-name host))))


;;; Find out how to pass the raw socket as a VALUES on clisp
(defun make-active-socket (server port)
  "Returns (VALUES STREAM SOCKET)"
  #+clisp
  (let ((iostream (socket:socket-connect port server :external-format :unix)))
    (values iostream iostream))
  #+lispworks
  (let ((sock (comm:open-tcp-stream server port)))
    (values sock sock))
  #+sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp))
        (hostname (lookup-hostname server)))
    (sb-bsd-sockets:socket-connect socket hostname port)
    (values
      (sb-bsd-sockets:socket-make-stream socket 
                                         :input t 
                                         :output t 
                                         :element-type 'base-char)
      socket))
  #-(or clisp sbcl lispworks) (error "CL-MPD is not implemented on this lisp."))


(defun read-buf-nonblock (buffer stream)
  "Like READ-SEQUENCE, but returns early if the full quantity of data isn't there to be read.
  Blocks if no input at all"
  (let ((eof (gensym)))
    (do ((i 0 (1+ i))
         (c (read-char stream nil eof)
            (read-char-no-hang stream nil eof)))
      ((or (>= i (length buffer)) (not c) (eq c eof)) i)
      (setf (elt buffer i) c))))


;;; TODO
;;; 1) What is preventing this from getting the mpd welcome message in MPD-MAKE-CONNECTION?
;;; 2) What is preventing mpd-get-return-elements from getting the end OK\n?
;;; 3) Does the EOF set to OK\n work correctly?
;;; 
;;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;; This needs to be rewritten to call another function which calls
;;; READ-BUF-NONBLOCK, so that I can then take the return of that function and
;;; pass it to CHECK-FOR-ERRORS. Then have this function do it's normat
;;; splitting the return elements into (key:value) pairs. I think this will
;;; also fix the above.
(defun mpd-get-return-elements (iostream)
  "Returns a list of MPD return elements, each cons cell being key:value."
  (let ((ret ())
        (eof (append-newline-char "OK")))
    (do ((c (read-line iostream nil eof) (read-line iostream nil eof)))
      ((null (listen iostream)) (nreverse ret))
      #+nil
      (dprint "~&## DEBUG: in MPD-GET-RETURN-ELEMENTS pushing ~D on RET" c)
      (push (list c) ret))))


(defun split-mpd-key-value-pair (str)
  (let ((pos (position #\: str)))
    (list (subseq str 0 pos) (subseq str (1+ pos)))))


;;; UNTESTED
#+nil
(defun flush-return-elements (iostream)
  (loop while (mpd-get-return-elements iostream)))


(defun mpd-get-port ()
  "Get the environment variable MPD_PORT."
  #+clisp
  (let ((port (parse-integer (ext:getenv "MPD_PORT"))))
    (if (not (null port))
      port
      6600))
  #+sbcl
  (let ((port (parse-integer (sb-ext:posix-getenv "MPD_PORT"))))
    (if (not (null port))
      port
      6600))
  #+lispworks
  6600
  )


(defun mpd-get-host ()
  "Get the environment variable MPD_HOST."
  #+clisp
  (let ((host (ext:getenv "MPD_HOST")))
    (if (not (null host))
      host
      "localhost"))
  #+sbcl
  (let ((host (sb-ext:posix-getenv "MPD_HOST")))
    (if (not (null host))
      host
      "localhost"))
  #+lispworks
  "localhost"
  )


(defun mpd-make-connection (host port &key (timeout 1))
  "Creates a connection to MPD. Returns (VALUES STREAM SOCKET)."
  (let ((data (make-string 200)))
    (multiple-value-bind (iostream sock) (make-active-socket host port)
      (if (null iostream)
        (error "~&Failed to establish a connection with MPD, IOSTREAM was null.~%"))
      (sleep timeout)
      (let ((data (subseq data 0 (read-buf-nonblock data iostream))))
        (dprint "~&## DEBUG: Recieved [~D] from MPD~%" (scrub-newline-chars data))
        (if (not (string= +mpd-welcome-message+ (subseq data 0 7)))
          (error "~&Failed to establish a connection with MPD, +MPD-WELCOME-MESSAGE+ was bad.~%"))
        (values iostream sock)))))


;;; TODO Get this working with MPD-GET-RETURN-ELEMENTS
#+nil
(defun mpd-make-connection (host port &key (timeout 1))
  "Creates a connection to MPD. Returns (VALUES STREAM SOCKET)."
  (multiple-value-bind (iostream sock) (make-active-socket host port)
    (if (null iostream)
      (error "~&Failed to establish a connection with MPD~%"))
    (sleep timeout)
    (dprint "~&## DEBUG: LISTEN returns ~S" (listen iostream))
    (let ((ret (mpd-get-return-elements iostream)))
      (dprint "~&## DEBUG: MPD sent us... ~S." (car ret))
      (values iostream sock))))



;;; --------------------------
;;; MPD Commands
;;; --------------------------

;;; test if this declare helps at all
(defun mpd-send-command (iostream command)
  (declare (optimize (speed 3) (safety 1))
           (type string command) (type iostream iostream))
  (if (open-stream-p iostream)
    (progn
      (write-string (append-newline-char command) iostream)
      (force-output iostream))
    (error "MPD-SEND-COMMAND was passed a closed stream.")))


(defun mpd-send-add-command (iostream path)
  (let ((command (format nil "add \"~D\"" path)))
    (mpd-send-command iostream command)))


(defun mpd-send-addid-command (iostream id)
  (let ((command (format nil "addid \"~S\"" id)))
    (mpd-send-command iostream command)))


(defun mpd-send-clear-command (iostream) 
  "Clears the current playlist."
  (mpd-send-command iostream "clear"))


(defun mpd-send-clearerror-command (iostream) 
  (mpd-send-command iostream "clearerror"))


(defun mpd-send-close-command (iostream)
  "Closes the connection to MPD."
  (mpd-send-command iostream "close"))


(defun mpd-send-commands-command (iostream)
  "Get list of MPD commands."
  (mpd-send-command iostream "commands"))


(defun mpd-send-crossfade-command (iostream seconds)
  "Sets crossfading between songs."
  (let ((command (format nil "crossfade \"~S\"" seconds)))
    (mpd-send-command iostream command)))


(defun mpd-send-currentsong-command (iostream) 
  (mpd-send-command iostream "currentsong"))


(defun mpd-send-delete-command (iostream song)
  "Delete the song in the playlist at SONG position."
  (let ((command (format nil "delete \"~D\"" song)))
    (mpd-send-command iostream command)))


(defun mpd-send-deleteid-command (iostream songid)
  "Deletes SONGID in the playlist."
  (let ((command (format nil "deleteid \"~D\"" songid)))
    (mpd-send-command iostream command)))


;;; MPD-SEND-DISABLEOUPUT-COMMAND


;;; MPD-SEND-ENABLEOUTPUT-COMMAND


(defun mpd-send-find-command (iostream type what)
  (let ((command (format nil "find \"~D\" \"~D\"" type what)))
    (mpd-send-command iostream command)))


(defun mpd-send-kill-command (iostream) 
  "Kills MPD."
  (mpd-send-command iostream "kill"))


;;; untested
(defun mpd-send-list-command (iostream type &optional (arg nil arg-supplied-p))
  "Lists all tags of TYPE, ARG is an optional specifier such as an artist."
  (if arg-supplied-p
    (progn
      (let ((command (format nil "list \"~D\" \"~D\"" type arg)))
        (mpd-send-command iostream command)))
    (progn
      (let ((command (format nil "list \"~D\"" type)))
        (mpd-send-command iostream command)))))
    

(defun mpd-send-listall-command (iostream dir)
  "Lists all songs and directories in DIR recursively."
  (let ((command (format nil "listall \"~D\"" dir)))
    (mpd-send-command iostream command)))


(defun mpd-send-listallinfo-command (iostream dir)
  "Lists contents of DIR recursively, including song metadata."
  (let ((command (format nil "listallinfo \"~D\"" dir)))
    (mpd-send-command iostream command)))


(defun mpd-send-load-command (iostream name)
  "Loads the playlist NAME.m3u from the playlist dir."
  (let ((command (format nil "load \"~D\"" name)))
    (mpd-send-command iostream command)))


(defun mpd-send-lsinfo-command (iostream dir)
  "Lists contents of DIR from the db, including song metadata."
  (let ((command (format nil "lsinfo \"~D\"" dir)))
    (mpd-send-command iostream command)))


(defun mpd-send-move-command (iostream from to)
  "Moves song at position FROM to position TO."
  (let ((command (format nil "move \"~S\" \"~S\"" from to)))
    (mpd-send-command iostream command)))


(defun mpd-send-moveid-command (iostream songid to)
  "Moves the song with the id SONGID to position TO in the playlist."
  (let ((command (format nil "moveid \"~S\" \"~S\"" songid to)))
    (mpd-send-command iostream command)))


(defun mpd-send-next-command (iostream)
  (mpd-send-command iostream "next"))


(defun mpd-send-notcommands-command (iostream)
  (mpd-send-command iostream "notcommands"))


(defun mpd-send-outputs-command (iostream)
  (mpd-send-command iostream "outputs"))


(defun mpd-send-password-command (iostream password)
  "Authenticate with the server with PASSWORD."
  (let ((command (format nil "password \"~D\"" password)))
    (mpd-send-command iostream command)))


(defun mpd-send-pause-command (iostream mode) 
  "Toggles the pause state of mpd to 1 on, or 0 off."
  (let ((command (format nil "pause \"~S\"" mode)))
    (mpd-send-command iostream command)))


(defun mpd-send-ping-command (iostream) 
  (mpd-send-command iostream "ping"))


(defun mpd-send-play-command (iostream song) 
  "Plays song position number SONG in the playlist."
  (let ((command (format nil "play \"~S\"" song)))
    (mpd-send-command iostream command)))


(defun mpd-send-playid-command (iostream id) 
  "Plays song associated with ID from the playlist."
  (let ((command (format nil "playid \"~S\"" id)))
    (mpd-send-command iostream command)))


;;; don't use this, use playlistinfo instead.
(defun mpd-send-playlist-command (iostream)
  (mpd-send-command iostream "playlist"))


(defun mpd-send-playlistid-command (iostream &key songid)
  "Displays list of songs in the playlist, :SONG specializes on a single song id."
  (if songid
    (progn
      (let ((command (format nil "playlistid \"~S\"" songid)))
        (mpd-send-command iostream command)))
    (mpd-send-command iostream "playlistinfo ")))


;;; I need to totally re-write this, since this thing returns a 
;;; shit tonne of info. Also SONG is not the ID in this case, 
;;; rather it is the numerical position in the playlist.
(defun mpd-send-playlistinfo-command (iostream &optional song)
  "Displays list of songs in the playlist, :SONG specializes on a single song."
  (if song
    (progn
      (let ((command (format nil "playlistinfo \"~S\"" song)))
        (mpd-send-command iostream command)))
    (mpd-send-command iostream "playlistinfo ")))


(defun mpd-send-plchanges-command (iostream pls)
  "Displays changed songs currently in the playlist since PLS version."
  (let ((command (format nil "plchanges \"~S\"" pls)))
    (mpd-send-command iostream command)))


(defun mpd-send-prev-command (iostream)
  (mpd-send-command iostream "previous"))


(defun mpd-send-random-command (iostream mode)
  "Set random state to MODE, which is either 0 or 1."
  (let ((command (format nil "random \"~S\"" mode)))
    (mpd-send-command iostream command)))


(defun mpd-send-repeat-command (iostream mode)
  "Set repeat state to MODE, which is either 0 or 1."
  (let ((command (format nil "repeat \"~S\"" mode)))
    (mpd-send-command iostream command)))


(defun mpd-send-rm-command (iostream name)
  "Removes the playlist NAME.m3u from the playlist dir."
  (let ((command (format nil "rm \"~D\"" name)))
    (mpd-send-command iostream command)))


(defun mpd-send-save-command (iostream name)
  "Saves the current playlist to NAME.m3u in the playlist dir."
  (let ((command (format nil "save \"~D\"" name)))
    (mpd-send-command iostream command)))


(defun mpd-send-search-command (iostream type what)
  "Search for song containing WHAT, not case sensitive; TYPE is title, artist, album, of filename."
  (let ((command (format nil "search \"~D\" \"~D\"" type what)))
    (mpd-send-command iostream command)))


(defun mpd-send-seek-command (iostream song time)
  "Seeks to position TIME in seconds of entry SONG, in the playlist."
  (let ((command (format nil "seek \"~S\" \"~S\"" song time)))
    (mpd-send-command iostream command)))


(defun mpd-send-seekid-command (iostream songid time)
  "Seeks to the position TIME (in seconds) of the song with SONGID."
  (let ((command (format nil "seekid \"~S\" \"~S\"" songid time)))
    (mpd-send-command iostream command)))


(defun mpd-send-setvol-command (iostream vol)
  "Set volume to VOL, with range being 0-100."
  (let ((command (format nil "setvol \"~S\"" vol)))
    (mpd-send-command iostream command)))


(defun mpd-send-shuffle-command (iostream) 
  (mpd-send-command iostream "shuffle"))


(defun mpd-send-stats-command (iostream) 
  (mpd-send-command iostream "stats"))


(defun mpd-send-status-command (iostream) 
  (mpd-send-command iostream "status"))


(defun mpd-send-stop-command (iostream) 
  (mpd-send-command iostream "stop"))


(defun mpd-send-swap-command (iostream song1 song2)
  "Swap positions of SONG1 and SONG2."
  (let ((command (format nil "swap \"~S\" \"~S\"" song1 song2)))
    (mpd-send-command iostream command)))


(defun mpd-send-swapid-command (iostream songid1 songid2)
  "Swap positions of SONG1 and SONG2."
  (let ((command (format nil "swapid \"~S\" \"~S\"" songid1 songid2)))
    (mpd-send-command iostream command)))


(defun mpd-send-update-command (iostream &optional (path nil path-supplied-p))
  "Searches music directory for new music and removes old music from the db. :PATH is optional."
  (if path-supplied-p
    (progn
      (let ((command (format nil "update \"~D\"" path)))
        (mpd-send-command iostream command)))
    (mpd-send-command iostream "update")))


(defun mpd-send-urlhandlers-command (iostream)
  (mpd-send-command iostream "urlhandlers"))


;;; Supposedly this is deprecated; is this true?
(defun mpd-send-volume-command (iostream vol)
  "Increment or decrement the volume by VOL."
  (let ((command (format nil "volume \"~S\"" vol)))
    (mpd-send-command iostream command)))



;;; --------------------------
;;; MPD Command Lists
;;; --------------------------

;;; The new and improved, and 'lispy' way to implement command lists.
(defmacro mpd-command-list (client &body forms)
  "Takes an MPD-CLIENT instance and mpd-send-command forms, then sends a command list to mpd."
  (let ((conn (gensym)))
    `(let ((,conn (mpd-iostream ,client)))
       (mpd-send-command ,conn "command_list_begin")
       ,@forms
       (mpd-send-command ,conn "command_list_end"))))



;;; --------------------------
;;; Classes and Methods
;;; --------------------------


(deftype iostream ()
  #+sbcl 'sb-sys:fd-stream
  #+clisp 'stream
  #+lispworks 'comm:socket-stream
  )

;;; this is a stream for clisp until I can get at the raw socket.
(deftype socket ()
  #+sbcl 'sb-bsd-sockets:inet-socket
  #+clisp 'stream
  #+lispworks 'comm:socket-stream
  )

(defclass mpd-client ()
  ((iostream
     :initform nil
     :initarg :iostream
     :accessor mpd-iostream
     :type iostream
     :documentation "The input/output stream to MPD.")
   (socket
     :initform nil
     :initarg :socket
     :accessor mpd-socket
     :type socket
     :documentation "The raw socket to MPD.")
   (io-timeout
     :initform 1
     :initarg :io-timeout
     :accessor mpd-io-timeout
     :type integer
     :documentation "The connection timeout period in seconds")
   (command-list-mode
     :initform 0
     :accessor mpd-command-list-mode
     :type integer
     :documentation "Not used: 1 if we're in, 0 if not.")
   (host
     :initform (mpd-get-host)
     :accessor mpd-host
     :type string
     :documentation "Used to keep tabs of the host of MPD.")
   (port
     :initform (mpd-get-port)
     :accessor mpd-port
     :type integer
     :documentation "Used to keep tabs of the port of MPD.")))


;;; Dangerous abstraction, use with caution.
(defmacro with-mpd-client-slots ((conn) &body body)
  `(with-slots (iostream socket io-timeout command-list-mode host port) ,conn
    ,@body))


;;; Create a socket and fd-stream, then setup our mpd-client appropriate object variables.
(defmethod initialize-instance :after ((conn mpd-client) &rest initargs)
  (with-mpd-client-slots (conn)
    (multiple-value-bind (io sock) (mpd-make-connection host port :timeout io-timeout)
      (setf iostream io)
      (setf socket sock))))


;;; TODO
;;; Check and make sure that this works on LispWorks
(defgeneric mpd-client-disconnect (mpd-client))

(defmethod mpd-client-disconnect ((client mpd-client))
  "Closes the connection to MPD and performs any necessary cleanup."
  (with-mpd-client-slots (client)
    (mpd-send-close-command iostream)
    #+clisp (close socket)
    #+sbcl  (sb-bsd-sockets:socket-close socket)
    #+lispworks (close socket)
    ))


;;; --------------------------
;;; MPD stats
;;; --------------------------

(defclass mpd-stats ()
  ((artists
     :initform 0
     :initarg :artists
     :accessor mpd-stats-artists
     :type integer
     :documentation "Number of artists.")
   (albums
     :initform 0
     :initarg :albums
     :accessor mpd-stats-albums
     :type integer
     :documentation "Number of albums.")
   (songs
     :initform 0
     :initarg :songs
     :accessor mpd-stats-songs
     :type integer
     :documentation "Number of songs.")
   (uptime
     :initform 0
     :initarg :uptime
     :accessor mpd-stats-uptime
     :type integer
     :documentation "Daemon uptime in seconds.")
   (db-playtime
     :initform 0
     :initarg :db-playtime
     :accessor mpd-stats-db-playtime
     :type integer
     :documentation "Sum of all song times in db.")
   (db-update
     :initform 0
     :initarg :db-update
     :accessor mpd-stats-db-update
     :type integer
     :documentation "Last db update in UNIX time.")
   (playtime
     :initform 0
     :initarg :playtime
     :accessor mpd-stats-playtime
     :type integer
     :documentation "Time length of music played.")))


;;; Dangerous abstraction, use with caution.
(defmacro with-mpd-stats-slots ((stats) &body body)
  `(with-slots (artists albums songs uptime db-playtime db-update playtime) ,stats
    ,@body))


(defgeneric mpd-update-stats (mpd-client mpd-stats))

;;; I want to know WTF the point of having STRING= when EQUAL works just as well,
;;; and is MUCH faster?
(defmethod mpd-update-stats ((client mpd-client) (stats mpd-stats))
  (with-mpd-client-slots (client)
    (with-mpd-stats-slots (stats)
      (mpd-send-stats-command iostream)
      (let* ((response (mpd-get-return-elements iostream))
             (key-values (map 'list #'(lambda (x)
                                        (split-mpd-key-value-pair (car x)))
                              response)))
        #+nil
        (format t "~&~{~A ~}" key-values)
        (dolist (i key-values)
          (let ((key (car i))
                (value (parse-integer (cadr i) :junk-allowed nil)))
            (cond ((string= key "artists") (setf artists value))
                  ((string= key "albums") (setf albums value))
                  ((string= key "songs") (setf songs value))
                  ((string= key "uptime") (setf uptime value))
                  ((string= key "db_playtime") (setf db-playtime value))
                  ((string= key "db_update") (setf db-update value))
                  ((string= key "playtime") (setf playtime value))
                  (t (warn "~&MPD-UPDATE-STATS error, Key == ~S Value == ~S" 
                             key value)))))))))


;;; This is an EVIL! optimization, but I want to get this to work.
#|
(declaim (inline my-string=))
(defun my-string= (str-a str-b)
  (declare (optimize (debug 0) (safety 0) (speed 3))
           (type (simple-base-string *) str-a)
           (type (simple-base-string *) str-b))
  #+(not sbcl) (equal str-a str-b)
  #+sbcl (string= str-a str-b))
|#


;;; --------------------------
;;; MPD status
;;; --------------------------

;;; some of this shit never gets returned when I issue status; why?
(defclass mpd-status ()
  ((volume
     :initform 0
     :initarg :volume
     :accessor mpd-status-volume
     :type integer
     :documentation "Volume level 0-100")
   (repeat
     :initform 0
     :initarg :repeat
     :accessor mpd-status-repeat
     :type integer
     :documentation "Repeat state 0 or 1.")
   (random
     :initform 0
     :initarg :random
     :accessor mpd-status-random
     :type integer
     :documentation "Random state 0 or 1.")
   (playlist
     :initform 0
     :initarg :playlist
     :accessor mpd-status-playlist
     :type integer
     :documentation "31-bit unsigned integer, the playlist version number.")
   (playlist-length
     :initform 0
     :initarg :playlist-length
     :accessor mpd-status-playlist-length
     :type integer
     :documentation "Integer, the length of the playlist.")
   (state
     :initform ""
     :initarg :state
     :accessor mpd-status-state
     :type string
     :documentation "play, stop, or pause")
   (song
     :initform 0
     :initarg :song
     :accessor mpd-status-song
     :type integer
     :documentation "Current song stopped on or playing, playlist song number.")
   (songid
     :initform 0
     :initarg :songid
     :accessor mpd-status-songid
     :type integer
     :documentation "Current song stopped on or playing, playlist songid.")
   (time
     :initform 0
     :initarg :time
     :accessor mpd-status-time
     :type string
     :documentation "Represents elapsed:total of current playing/paused song.")
   (bitrate
     :initform 0
     :initarg :bitrate
     :accessor mpd-status-bitrate
     :type integer
     :documentation "Instantaneous bitrate in kbps.")
   (xfade
     :initform 0
     :initarg :xfade
     :accessor mpd-status-xfade
     :type integer
     :documentation "Crossfade in seconds.")
   (audio
     :initform nil
     :initarg :audio
     :accessor mpd-status-audio
     :type string
     :documentation "sample-rate:bits:channels")
   (updating-db
     :initform 0
     :initarg :updating-db
     :accessor mpd-status-updating-db
     :type integer
     :documentation "1 if MPD is updating its db, 0 if not.")
   (error
     :initform ""
     :initarg :error
     :accessor mpd-status-error
     :type string
     :documentation "If there is an error, the message will be here.")))


;;; Dangerous abstraction, use with caution.
(defmacro with-mpd-status-slots ((status) &body body)
  `(with-slots (volume repeat random playlist playlist-length state song songid
                time bitrate xfade audio updating-db error) ,status
    ,@body))


(defgeneric mpd-update-status (mpd-client mpd-status))

;;; I need to parse-integer the lists in AUDIO and TIME
(defmethod mpd-update-status ((client mpd-client) (status mpd-status))
  (with-mpd-client-slots (client)
    (with-mpd-status-slots (status)
      (mpd-send-status-command iostream)
      (let* ((response (mpd-get-return-elements iostream))
             (key-values (map 'list #'(lambda (x)
                                        (split-mpd-key-value-pair (car x)))
                              response)))
        #+nil
        (format t "~&~{~A ~}" key-values)
        (dolist (i key-values)
          (let ((key (car i))
                (value (string-trim '(#\Space) (cadr i))))
            (cond ((string= key "volume") (setf volume (parse-integer value)))
                  ((string= key "repeat") (setf repeat (parse-integer value)))
                  ((string= key "random") (setf random (parse-integer value)))
                  ((string= key "playlist") (setf playlist (parse-integer value)))
                  ((string= key "playlistlength") (setf playlist-length (parse-integer value)))
                  ((string= key "xfade") (setf xfade (parse-integer value)))
                  ((string= key "state") (setf state value))
                  ((string= key "song") (setf song (parse-integer value)))
                  ((string= key "songid") (setf songid (parse-integer value)))
                  ((string= key "time") (setf time value))
                  ((string= key "bitrate") (setf bitrate (parse-integer value)))
                  ((string= key "audio") (setf audio value))
                  ((string= key "updating_db") (setf updating-db (parse-integer value)))
                  ((string= key "error") (setf error value))
                  (t (warn "~&MPD-UPDATE-STATUS error, Key == ~S Value == ~S" 
                             key value)))))))))

;;; TODO 
;;; write generic update MPD-STATUS obj method, and the INITIALIZE-INSTANCE
;;; :after method



;;; --------------------------
;;; MPD songs/currentsong
;;; --------------------------

;;; Big TODO, need to mention to MPD dev's that on these slots that are integer only slots, that I really need MPD to not pass me non integers like Track: 9/12

(defclass mpd-song ()
  ((file
     :initform nil
     :initarg :file
     :accessor mpd-song-file
     :type string
     :documentation "Relative pathname of the the current playing/paused song.")
   (artist
     :initform nil
     :initarg :artist
     :accessor mpd-song-artist
     :type string
     :documentation "Song artist, may be nil.")
   (title
     :initform nil
     :initarg :title
     :accessor mpd-song-title
     :type string
     :documentation "Song title, may be nil.")
   (album
     :initform nil
     :initarg :album
     :accessor mpd-song-album
     :type string
     :documentation "Song album, may be nil.")
   (track
     :initform nil
     :initarg :track
     :accessor mpd-song-track
     :type string
     :documentation "Song track, may be nil.")
   (name
     :initform nil
     :initarg :name
     :accessor mpd-song-name
     :type string
     :documentation "Song name, may be nil.")
   (date
     :initform nil
     :initarg :date
     :accessor mpd-song-date
     :type string
     :documentation "I'm not sure wtf this is here for...?")
   (genre
     :initform nil
     :initarg :genre
     :accessor mpd-song-genre
     :type string
     :documentation "Song's genre.")
   (composer
     :initform nil
     :initarg :composer
     :accessor mpd-song-composer
     :type string
     :documentation "Songs composer.")
   (time
     :initform 0
     :initarg :time
     :accessor mpd-song-time
     :type integer
     :documentation "Song length in seconds.")
   (position
     :initform nil
     :initarg :position
     :accessor mpd-song-position
     :type integer
     :documentation "The position of the song in the playlist.")
   (id
     :initform 0
     :initarg :id
     :accessor mpd-song-id
     :type integer
     :documentation "Song ID for a song in the playlist.")
   (initialized
     :initform nil
     :accessor mpd-song-initialized
     :type integer
     :documentation "0 or 1: Keeps tabs on if this object has been initialized.")))


;;; Dangerous abstraction, use with caution.
(defmacro with-mpd-song-slots ((song) &body body)
  `(with-slots (file artist title album track name date genre composer time position id initialized) ,song
    ,@body))

;;; Write an INITIALIZE-INSTANCE :AFTER method that allows you to initialize the song to
;;; SONG or SONGID by using :POSITION for song and :ID for songid

;;; NOTES/TODOS/BUGS:
;;;
;;; 1) Write this so that it can get the key/values from a song or songid, but
;;; default to MPD-SEND-CURRENTSONG-COMMAND.
;;;
;;; 2) Find out and complain! about the key discrepensies with songs, which
;;; really should be completely lowercase like status and stats keys.
;;;
;;; Ok at the moment this only allows the object to be initialized to either 
;;;   A) the current song, or 
;;;   B) the song in the playlist held in MPD-SONG-POSITION.

;;; Why doesn't this clear out my slots? I want to use this in my :before method
;;; for MPD-UPDATE-SONG.
#+nil
(defmacro unbind-slot (obj slot)
  `(if (not (slot-boundp ,obj ,slot))
     (slot-makunbound ,obj ,slot)))

;;; Use this in :BEFORE to setup our condition handlers.

(defgeneric mpd-update-song (mpd-client mpd-song))

;;; Don't wipe out position, the main method depends on it.
;;; I tried this out with WITH-ACCESSORS but this looked cleaner.
(defmethod mpd-update-song :before ((client mpd-client) (song mpd-song))
  (let ((init (mpd-song-initialized song)))
    (when (eql init 1)
      (setf (mpd-song-file song) nil)
      (setf (mpd-song-artist song) nil)
      (setf (mpd-song-title song) nil)
      (setf (mpd-song-album song) nil)
      (setf (mpd-song-track song) nil)
      (setf (mpd-song-name song) nil) 
      (setf (mpd-song-date song) nil)
      (setf (mpd-song-genre song) nil)
      (setf (mpd-song-composer song) nil) 
      (setf (mpd-song-time song) nil)
      (setf (mpd-song-id song) nil))))


(defmethod mpd-update-song ((client mpd-client) (song mpd-song))
  (with-mpd-client-slots (client)
    (with-mpd-song-slots (song)
      (if (null position)
        (mpd-send-currentsong-command iostream)
        (mpd-send-playlistinfo-command iostream position))
      (let* ((response (mpd-get-return-elements iostream))
             (key-values (map 'list #'(lambda (x)
                                        (split-mpd-key-value-pair (car x)))
                              response)))
        #+nil
        (format t "~&~%~{~A ~}" key-values)
        (dolist (i key-values)
          (let ((key (car i))
                (value (string-trim '(#\Space) (cadr i))))
            (cond ((string= key "file") (setf file value))
                  ((string= key "Time") (setf time (parse-integer value)))
                  ((string= key "Artist") (setf artist value))
                  ((string= key "Title") (setf title value))
                  ((string= key "Album") (setf album value))
                  ((string= key "Track") (setf track value))
                  ((string= key "Name") (setf name value))
                  ((string= key "Date") (setf date value))
                  ((string= key "Genre") (setf genre value))
                  ((string= key "Composer") (setf composer value))
                  ((string= key "Pos") (setf position (parse-integer value)))
                  ((string= key "Id") (setf id (parse-integer value)))
                  (t (warn "~&MPD-UPDATE-SONG error, Key == ~S Value == ~S" 
                             key value)))
            (setf initialized 1)))))))



;;; --------------------------
;;; MPD Error Handling
;;; --------------------------

(define-condition mpd-server-error (error)
  ((mpd-error-code
     :initarg :error-code
     :accessor mpd-error-code
     :documentation "Contains one of the error numbers.")
   (mpd-error-string
     :initarg :error-string
     :accessor mpd-error-string
     :documentation "Contains the string MPD returns, describing the error.")))


(defun check-for-errors (response)
  "Checks server response for error message, and signals an appropriate condition."
  (declare (type string response))
  (when (string= response "ACK " :start1 0 :end1 4)
    (let ((error-code (remove #\@ (subseq response 5 7) :count 1))
          (error-string (subseq response (1+ (position #\] response)))))
      (dprint "~&## DEBUG: CHECK-FOR-ERRORS:~%Error code is <~A>~%Error string is <~A>"
              error-code error-string)
      (error 'mpd-server-error :error-code error-code :error-string error-string))))

