;;;===========================================================================
;;; OM-Spat
;;; Rendering of spatial scene descriptions 
;;; J. Bresson (IRCAM - 2010)
;;;===========================================================================

(in-package :om)


(add-external-pref-module 'spat)

(defmethod get-external-name ((module (eql 'spat))) "Spat")

(defmethod get-external-module-vals ((module (eql 'spat)) modulepref) (get-pref modulepref :spat-options))
(defmethod get-external-module-path ((module (eql 'spat)) modulepref) (get-pref modulepref :spat-path))
(defmethod set-external-module-vals ((module (eql 'spat)) modulepref vals) (set-pref modulepref :spat-options vals))
(defmethod set-external-module-path ((module (eql 'spat)) modulepref path) 
  (set-pref modulepref :spat-path path))


(defparameter *spat-renderer* "spat-sdif-renderer")
(defparameter *spat-default-hrtf* "IRC_9002_itd_24order_biquads_44100.hrtf")
(defparameter *spat-default-numout* 2)
(defparameter *spat-default-panning* "angular")
(defparameter *spat-default-decoding* nil)
(defparameter *spat-default-nchannels* 8)
(defparameter *spat-default-nreverb* 1)
(defparameter *spat-default-buffersize* 512)


(defun default-spat-renderer-folder ()
  (merge-pathnames 
   "Documents/Max 8/Packages/spat5/media/tools/" 
   (om::om-user-home)))

(defun check-spat-dependencies () 

  (when (probe-file *spat-renderer*)
  
    (let* ((current-folder (om-make-pathname :directory *spat-renderer*))
           (required-libs '("hdf5.dll" "hdf5_hl.dll" "libcurl.dll" "netcdf.dll" "zlib1.dll"))
           (spat-dependencies-folder "../../dependencies/" (merge-pathnames current-folder)))
      
      (loop for lib in required-libs
            unless (probe-file (merge-pathnames lib current-folder))
            do (if (probe-file (merge-pathnames lib spat-dependencies-folder))
                   (om-copy-file (merge-pathnames lib spat-dependencies-folder) current-folder)
                 (om-beep-msg (format nil "Spat dependency: ~A not found." lib))))
      )))

;;; (def-num-out panning-type decoding-type HRTF-file num-channels-internal num-reverbs buffersize)
(defun def-spat-options () 
  (list 2 "angular" nil 
        (om-make-pathname :directory (append (pathname-directory (lib-resources-folder (find-library "OM-Spat"))) '("hrtf"))
                          :name "IRC_9002_itd_24order_biquads_44100" :type "hrtf")
        8 1 32))

(defmethod get-external-def-vals ((module (eql 'spat))) 
   (let ((libpath (lib-pathname (find-library "OM-Spat"))))
     (list :spat-path (om-make-pathname :directory (default-spat-renderer-folder)
                                        :name "spat-sdif-renderer" #+win32 :type #+win32 "exe")
           :spat-options (def-spat-options))))

(defmethod save-external-prefs ((module (eql 'spat))) 
  `(:spat-path ,(om-save-pathname *spat-renderer*) 
    :spat-options (list ,*spat-default-numout*
                          ,*spat-default-panning*
                          ,*spat-default-decoding*
                          ,(om-save-pathname *spat-default-hrtf*)
                          ,*spat-default-nchannels*
                          ,*spat-default-nreverb*
                          ,*spat-default-buffersize*
                          )))


(defmethod put-external-preferences ((module (eql 'spat)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :spat-options)))
    (when list-prefs 
      (setf *spat-default-numout* (nth 0 list-prefs))
      (setf *spat-default-panning* (nth 1 list-prefs))
      (setf *spat-default-decoding* (nth 2 list-prefs))
      (setf *spat-default-hrtf* (nth 3 list-prefs))
      (setf *spat-default-nchannels* (nth 4 list-prefs))
      (setf *spat-default-nreverb* (nth 5 list-prefs))
      (setf *spat-default-buffersize* (nth 6 list-prefs))
      )
    (when (get-pref moduleprefs :spat-path)
      (setf *spat-renderer* (find-true-external (get-pref moduleprefs :spat-path)))
      #+mswindows(check-spat-dependencies)
      )
    ))

(put-external-preferences 'spat (find-pref-module :externals))

(defmethod show-external-prefs-dialog ((module (eql 'spat)) prefvals)

  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "Spat Renderer Options"
                                 :size (om-make-point 360 280)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         (channels '("8" "16"))
         (revs '("1" "2" "3" "4"))
         (pos 20)
         (chanlabel (om-make-dialog-item 'om-static-text  
                                         (om-make-point 20 pos) (om-make-point 200 17) "Number of internal channels"
                                                                                 :font *om-default-font2*))

         (chanmenu (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 230 pos)
                                             (om-make-point 80 20) ""
                                             :range channels
                                             :value (number-to-string (nth 4 prefvals)) 
                                             :font *om-default-font2*))
         (pos (+ pos 30))
         ;(revlabel (om-make-dialog-item 'om-static-text  
         ;                                (om-make-point 20 pos) (om-make-point 200 17) "Number of reverbs"
         ;                                :font *om-default-font2*))

         ;(revmenu (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 230 pos)
         ;                                    (om-make-point 80 20) ""
         ;                                    :range revs
         ;                                    :value (number-to-string (nth 5 prefvals)) 
         ;                                    :font *om-default-font2*))
         ;(pos (+ pos 30))
         
         (buflabel (om-make-dialog-item 'om-static-text  
                                         (om-make-point 20 pos) (om-make-point 200 17) "Buffer size"
                                         :font *om-default-font2*))

         (buftext (om-make-dialog-item 'om-editable-text (om-make-point 230 pos)
                                             (om-make-point 40 20) (number-to-string (nth 6 prefvals))
                                             :font *om-default-font2*))
         
         (pos (+ pos 40))
         (hrtflabel (om-make-dialog-item 'om-static-text  (om-make-point 20 pos) (om-make-point 200 20) 
                                          "Deafult HRTF file" :font *om-default-font2*))
         (pos (+ pos 25))
         (hrtftext (om-make-dialog-item 'om-static-text  (om-make-point 20 pos) (om-make-point 250 75) 
                                                           (om-namestring (nth 3 prefvals))
                                                           :font *om-default-font1*))
         (hrtfbut (om-make-dialog-item 'om-button (om-make-point 280 pos) (om-make-point 50 24) "..." 
                                       :font *om-default-font1*
                                       :di-action (om-dialog-item-act item
                                                    (declare (ignore item))
                                                    (let ((newpath (om-choose-file-dialog 
                                                                    :directory (namestring (nth 3 prefvals)))))
                                                      (when (and newpath (probe-file newpath))
                                                        (om-set-dialog-item-text hrtftext (namestring newpath))
                                                        )))))

                  (pos (+ pos 40))
         ;(spatplayerlabel (om-make-dialog-item 'om-static-text  (om-make-point 20 pos) (om-make-point 200 20) 
         ;                                 "Spat/SDIF Player" :font *om-default-font2*))
         ;(pos (+ pos 25))
         ;(spatplayertext (om-make-dialog-item 'om-static-text  (om-make-point 20 pos) (om-make-point 250 75) 
         ;                                                  (om-namestring (nth 3 prefvals))
         ;                                                  :font *om-default-font1*))
         ;(spatplayerbut (om-make-dialog-item 'om-button (om-make-point 280 pos) (om-make-point 50 24) "..." 
         ;                              :font *om-default-font1*
         ;                              :di-action (om-dialog-item-act item
         ;                                           (declare (ignore item))
         ;                                           (let ((newpath (om-choose-file-dialog 
         ;                                                           :directory (namestring (nth 3 prefvals)))))
         ;                                             (when (and newpath (probe-file newpath))
         ;                                               (om-set-dialog-item-text hrtftext (namestring newpath))
         ;                                               )))))
         )
    (setf pos (+ pos 50))
    (om-add-subviews dialog chanlabel chanmenu buflabel buftext hrtflabel hrtftext hrtfbut ; revlabel revmenu spatplayerlabel spatplayertext spatplayerbut

      ;;; boutons
      (om-make-dialog-item 'om-button (om-make-point 15 pos) (om-make-point 90 20) "Restore"
                           :di-action (om-dialog-item-act item
                                        (om-set-selected-item chanmenu (number-to-string (nth 4 (def-spat-options))))
                                        (om-set-selected-item revmenu (number-to-string (nth 5 (def-spat-options))))
                                        (om-set-dialog-item-text buftext (number-to-string (nth 6 (def-spat-options))))
                                        (om-set-dialog-item-text hrtftext (namestring (nth 3 (def-spat-options))))
                                        ))
      
      (om-make-dialog-item 'om-button (om-make-point 160 pos) (om-make-point 90 20) "Cancel"
                           :di-action (om-dialog-item-act item
                                        (om-return-from-modal-dialog dialog nil)))
      
      (om-make-dialog-item 'om-button (om-make-point 250 pos) (om-make-point 90 20) "OK"
                           :di-action (om-dialog-item-act item
                                        (let ((argerror nil)
                                              (btxt (om-dialog-item-text buftext)))
                                         
                                          ;;; buffer size
                                          (if (and (not (string= "" btxt))
                                                   (integerp (read-from-string btxt)))
                                            (setf (nth 6 rep-list) (read-from-string btxt))
                                            (setf argerror t))
                                          
                                          (setf (nth 3 rep-list) (om-dialog-item-text hrtftext))
                                          (setf (nth 4 rep-list) (read-from-string (om-get-selected-item chanmenu)))
                                          ;(setf (nth 5 rep-list) (read-from-string (om-get-selected-item revmenu)))

                                          (if argerror
                                            (om-message-dialog (format nil "Error in a Spat option.~% Preferences could not be recorded.")) 
                                            (om-return-from-modal-dialog dialog rep-list))
                                          ))
                           :default-button t :focus t)
      )
    (om-modal-dialog dialog)))

