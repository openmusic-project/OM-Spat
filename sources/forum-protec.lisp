;=========================================================
; FORUM PROTECT: TEST FORUM PROTECT WITH IRCAM SOFTWARE
;=========================================================

(in-package :om)

(defun forum-protec (exe-path &optional lib)
  (if (probe-file exe-path) 
        (let ((wait-time 10)
              (authorized nil))
            (let ((protec-process (mp:process-run-function "TEST FORUM PROTEC" '(:priority 10) #'(lambda (path) 
                                                                                                   (print "Checking Forum protec:")
                                                                                                   (setf authorized (om-cmd-line (format nil "~s" (namestring path)) t t))
                                                                                                   (print (format nil "=> OK (~A)" authorized))) exe-path))
                  (count 0) (abort nil))
              (unless authorized
                (loop while (and (not authorized) (not abort)) do
                      (sleep 0.2)
                      (setf count (+ count 1))
                      (when (> count (/ wait-time 0.2))
                        (om-message-dialog (format nil "The external ~A does not respond or it is not authorized.~%~%In order to authorize IRCAM Forum tools, run the \"ForumProtec\" application available on the ForumNet website."
                                                   (string-upcase (pathname-name exe-path))
                                                   ;(if lib (namestring (om-make-pathname :directory (pathname-directory (lib-pathname lib)))) "library")
                                                   ))
                        (om-kill-process protec-process)
                        (setf abort t)
                        )
                      )
                )
              authorized))
    (progn (om-message-dialog (format nil "The external ~A was not found. Please check the installation path in the OM Preferences ('Libraries' tab)."
                               (namestring exe-path)))
      nil)))
            


