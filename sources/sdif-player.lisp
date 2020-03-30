(in-package :om)


#+(and macosx osc)
(enable-player :spat-sdif-player)

(add-player-for-object 'sdiffile :spat-sdif-player)

;==================
; APP
;==================

;(defvar *spat-sdif-player-app* nil)
;(defvar *spat-sdif-player-path* nil)

;(defun init-spat-sdif-player-app ()
;  (setf *spat-sdif-player-path* 
;        (probe-file (om-default-application-path '() "Spat-SDIF-Player"))))

;(om-add-init-func 'init-spat-sdif-player-app)

;(defun launch-spat-sdif-player-app ()
;  (unless (and *spat-sdif-player-app* (om-find-process *spat-sdif-player-app*))
;    (let ((path *spat-sdif-player-path*))
;      (if (or (null path) (not (probe-file path)))
;        (om-message-dialog "Spat-SDIF-Player application not found. Please set the application path in OM preferences.") 
;      (setf *spat-sdif-player-app* (om-run-application path))
;      ))))

;================
; PROTOCOL
;================

(defvar *spat-sdif-player-out-port* nil)
(setf *spat-sdif-player-out-port* 3002)
(defvar *spat-sdif-player-host* nil)
(setf *spat-sdif-player-host* "127.0.0.1")

(defvar *spat-sdif-player-file-to-play* nil)

(defmethod prepare-to-play  ((engine (eql :spat-sdif-player)) (player omplayer) (object sdiffile) at interval params)
  (setf *spat-sdif-player-file-to-play* (namestring (filepathname object)))
   (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/open" (namestring (filepathname object))))
   ;(sleep 0.1)
   ;(when interval
   ;  (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/start" (/ (car interval) 1000.0))))
   )

(defmethod player-start ((engine (eql :spat-sdif-player)) &optional play-list)
  (when *spat-sdif-player-file-to-play*
    (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/play" 1))))

(defmethod player-stop ((self (eql :spat-sdif-player)) &optional play-list)
   (declare (ignore view))
   (when *spat-sdif-player-file-to-play*
     (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/play" 0))))

(defmethod player-pause ((self (eql :spat-sdif-player)) &optional play-list)
  (when *spat-sdif-player-file-to-play*
    (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/pause" 1))))

(defmethod player-continue ((self (eql :spat-sdif-player)) &optional play-list)
  (when *spat-sdif-player-file-to-play*
    (om-send-osc-message *spat-sdif-player-out-port* *spat-sdif-player-host*  (list "/spatsdifplayer/pause" 0))))


;;;=================================
;;; OM INTERFACE
;;;=================================

(defmethod play-obj? ((self sdiffile)) (framesdesc self))

(defmethod get-obj-dur ((self sdiffile)) 
 (* 1000 (round (nth 1 (car (last (framesdesc self)))))))

(defmethod default-edition-params ((self sdiffile)) 
  (pairlis '(player) '(:spat-sdif-player)))

(defmethod players-for-object ((self sdiffile)) '(:spat-sdif-player))

