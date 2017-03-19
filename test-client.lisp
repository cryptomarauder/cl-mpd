;;;; vim: set fenc=utf-8 lisp ic wm=0 et tw=0 sm para=lpppipnpbp sect=shuh ai :
;;;; ---------------------------------------------------------------------------
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:      test-client.lisp
;;;; Purpose:   test code for cl-mpd
;;;; Author:    Stephen P. Horner
;;;; Started:   2006-04-14
;;;;
;;;; $Id:$
;;;;
;;;; This file is Copyright © 2006 by Stephen P. Horner
;;;;
;;;; ---------------------------------------------------------------------------

(declaim (optimize (speed 0) (space 0) (safety 1) (debug 3) (compilation-speed 0)))

(in-package #:mpd)

;;; TODO
;;; Write the client side songid database using a hash-table with songid as the
;;; key.  Consider using an object to hold the table, and make the updater
;;; function the primary method.
;;;
;;; I don't think MPD keeps track of songs by songid outside of the current
;;; playlist.  Need to find out if I'm wrong, and perhaps consider adding that
;;; code to MPD itself.

;;; TODO
;;; Rewrite maphash to make traversal 'in' numerical order, which on lispworks
;;; is not. Although I am not so sure this is even worth tinkering with
;;; considering the dynamic nature of hash table keys.  Although I really would
;;; like to know why if you add entries into a hash table in numerical order,
;;; say 1 through 100, why then would they not be placed in that order, and
;;; specifically why this really is only an issue with lispworks.
;;;
;;; TODO
;;; How do I make it so that I can pass a function, such as
;;; MPD-SEND-NEXT-COMMAND, when such a function requires an iostream?
(defun clos-client (&key (pls nil) (bitrate nil))
  (let ((client (make-instance 'mpd-client))
        (stats  (make-instance 'mpd-stats))
        (status (make-instance 'mpd-status))
        (song   (make-instance 'mpd-song :position 1)))

    (with-mpd-client-slots (client)

      (with-mpd-stats-slots (stats)
        (mpd-update-stats client stats)
        (format t "~&~%Slot values for STATS contains...")
        (format t "~&artists: ~S ~%albums: ~S ~%songs: ~S ~%uptime: ~S~%"
                artists albums songs uptime)
        (format t "playtime: ~S ~%db-playtime: ~S ~%db-update: ~S"
                playtime db-playtime db-update))

      (with-mpd-status-slots (status)
        (mpd-update-status client status)
        (format t "~&~%Slot values for STATUS contains...")
        (format t "~&volume: ~S ~%repeat: ~S ~%random: ~S ~%playlist: ~S"
                volume repeat random playlist)
        (format t "~%playlistlength: ~S ~%xfade: ~S ~%state: ~S ~%song: ~S ~%songid: ~S"
                playlist-length xfade state song songid)
        (format t "~%time: ~S ~%bitrate: ~S ~%audio: ~S ~%updating_db: ~S ~%error: ~S"
                time bitrate audio updating-db error))

      (with-mpd-song-slots (song)
        (mpd-update-song client song)
        (format t "~&~%Creating an MPD-SONG object for the song at position 1.")
        (format t "~&~%It's slot values are:")
        (format t "~&file: ~S ~%artist: ~S ~%title: ~S ~%album: ~S ~%track: ~S"
                file artist title album track)
        (format t "~%name: ~S ~%time: ~S ~%position: ~S ~%id: ~S"
                name time position id)
        
        (format t "~&~%You can now reuse an existing MPD-SONG object.")
        (format t "~&Updating object with the values of the current playing song.")

        (setf position (mpd-status-song status))
        (mpd-update-song client song)
        (format t "~&~%Slot values for SONG contains...")
        (format t "~&file: ~S ~%artist: ~S ~%title: ~S ~%album: ~S ~%track: ~S"
                file artist title album track)
        (format t "~%name: ~S ~%time: ~S ~%position: ~S ~%id: ~S~%~%"
                name time position id))

      (when pls
        (let* ((cnt (mpd-status-playlist-length status))
               (table (make-hash-table :size cnt)))
          (dotimes (i cnt)
            (let ((obj (make-instance 'mpd-song :position i)))
              (mpd-update-song client obj)
              (setf (gethash i table) obj)))
          (format t "~&~%The playlist contains:~%")
          (maphash #'(lambda (key value)
                       (format t "~&~S ~A" key (mpd-song-file value)))
                 table)))

      (when bitrate
        (format t "~&~%Testing a concept bitrate monitor~%~%")
        (let ((lbr nil)
              (vbr nil))
        (dotimes (i 10)
          (unless (null lbr) (sleep 1))
          (mpd-update-status client status)
          (let ((cbr (mpd-status-bitrate status)))
            (if (and (null vbr) (eql lbr cbr))
              (progn
                (when (> i 1)
                  (format t " fixed~%") (return)))
              (progn
                (format t "~&bitrate: ~A kbps" cbr)
                (unless (or (null lbr) (eql lbr cbr))
                  (setq vbr t))
                (setq lbr cbr)))))))

      (with-mpd-song-slots (song)
        (format t "~&~%Artist:~A~A~%~&Title:~A~A~%Album:~A~A~%~%"
                #\Tab artist #\Tab title #\Tab album))

      (mpd-client-disconnect client))))



;;; w00t it works, macros rock!!!
(defun test-command-list ()
  (let* ((client (make-instance 'mpd-client))
         (iostream (mpd-iostream client)))
    (mpd-command-list client
      (mpd-send-stop-command      iostream)
      (mpd-send-next-command      iostream)
      (mpd-send-play-command      iostream 1)
      (mpd-send-shuffle-command   iostream)
      (mpd-send-setvol-command    iostream 70)
      (mpd-send-crossfade-command iostream 5))
    (mpd-client-disconnect client)))


;;; Damnit I want to know why I need the fucking SUBSEQ in this!
(defun test-check-for-errors ()
  (let* ((client (make-instance 'mpd-client))
         (iostream (mpd-iostream client)))
    (mpd-send-command iostream "foobar")
    (let* ((buf (make-string 200))
           (ret (subseq buf 0 (read-buf-nonblock buf iostream))))
      (format t "~&RET == <~A>" ret))

    (mpd-client-disconnect client)))
