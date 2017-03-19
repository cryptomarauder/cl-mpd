;;;; vim: set fenc=utf-8 lisp ic wm=0 et tw=0 sm para=lpppipnpbp sect=shuh ai :
;;;; ---------------------------------------------------------------------------
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:      packages.lisp
;;;; Purpose:   Package definition for CL-MPD
;;;; Author:    Stephen P. Horner
;;;; Started:   2006-04-14
;;;;
;;;; $Id:$
;;;;
;;;; This file is Copyright © 2006 by Stephen P. Horner
;;;;
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defpackage :mpd
    (:use :common-lisp)
    (:export 
      ;; error constants
      #:+mpd-welcome-message+
      #:+mpd-error-not-list+
      #:+mpd-error-arg+
      #:+mpd-error-password+
      #:+mpd-error-permission+
      #:+mpd-error-unknown-cmd+
      #:+mpd-error-no-exist+
      #:+mpd-error-playlist-max+
      #:+mpd-error-system+
      #:+mpd-error-playlist-load+
      #:+mpd-error-update-already+
      #:+mpd-error-player-sync+
      #:+mpd-error-exist+

      ;; macros
      #:fprint
      #:debug-print

      ;; some utilities
      #:scrub-newline-chars
      #:append-newline-char

      ;; network related functions and friends
      #:lookup-hostname
      #:make-active-socket
      #:read-buf-nonblock
      #:mpd-get-return-elements
      #:split-mpd-key-value-pair
      #:mpd-get-port
      #:mpd-get-host
      #:mpd-make-connection

      ;; mpd-client and friends
      #:mpd-client
      #:with-mpd-client-slots
      #:mpd-client-disconnect

      ;; mpd-stats and friends
      #:mpd-stats
      #:with-mpd-stats-slots
      #:mpd-update-stats

      ;; mpd-status and friends
      #:mpd-status
      #:with-mpd-status-slots
      #:mpd-update-status

      ;; mpd-song and friends
      #:mpd-song
      #:with-mpd-song-slots
      #:mpd-update-song

      ;; mpd commands
      #:mpd-send-command
      #:mpd-send-add-command
      #:mpd-send-addid-command
      #:mpd-send-clear-command
      #:mpd-send-clearerror-command
      #:mpd-send-close-command
      #:mpd-send-commands-command
      #:mpd-send-crossfade-command
      #:mpd-send-currentsong-command
      #:mpd-send-delete-command
      #:mpd-send-deleteid-command
      #:mpd-send-find-command
      #:mpd-send-kill-command
      #:mpd-send-list-command
      #:mpd-send-listall-command
      #:mpd-send-listallinfo-command
      #:mpd-send-load-command
      #:mpd-send-lsinfo-command
      #:mpd-send-move-command
      #:mpd-send-moveid-command
      #:mpd-send-next-command
      #:mpd-send-notcommands-command
      #:mpd-send-outputs-command
      #:mpd-send-password-command
      #:mpd-send-pause-command
      #:mpd-send-ping-command
      #:mpd-send-play-command
      #:mpd-send-playid-command
      #:mpd-send-playlist-command
      #:mpd-send-playlistid-command
      #:mpd-send-playlistinfo-command
      #:mpd-send-plchanges-command
      #:mpd-send-prev-command
      #:mpd-send-random-command
      #:mpd-send-repeat-command
      #:mpd-send-rm-command
      #:mpd-send-save-command
      #:mpd-send-search-command
      #:mpd-send-seek-command
      #:mpd-send-seekid-command
      #:mpd-send-setvol-command
      #:mpd-send-shuffle-command
      #:mpd-send-stats-command
      #:mpd-send-status-command
      #:mpd-send-stop-command
      #:mpd-send-swap-command
      #:mpd-send-swapid-command
      #:mpd-send-update-command
      #:mpd-send-urlhandlers-command
      #:mpd-send-volume-command

      ;; command lists
      #:mpd-command-list
      #:mpd-send-command-list
      ))

  (pushnew :mpd *features*)
  )
