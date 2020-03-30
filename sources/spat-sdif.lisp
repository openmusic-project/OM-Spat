;;;===========================================================================
;;; OM-Spat
;;; Representation and rendering of spatial scene descriptions
;;; J. Bresson (IRCAM - 2010)
;;;===========================================================================

;;;===========================================================================
;;; Save SPAT-MATRIX as SDIF
;;;===========================================================================

(in-package :om)


(defvar *spat-sdiftypes* nil)

(setf *spat-sdiftypes* 
      (reduce #'(lambda (s1 s2) (string+ s1 " " s2))
              (list "{"
                    "1MTD XCAR {x, y, z}" 
                    "1MTD XPOS {x, y, z}" 
                    "1MTD XAED {azimuth, elevation, distance}"
                    "1MTD XORI {yaw, pitch, roll}" 
                    "1MTD XAPE {aperture}" 
                    "1MTD PRES {presence}"
                    "1MTD WARM {warmth}"
                    "1MTD BRIL {brillance}"
                    "1MTD PRER {room_presence}"
                    "1MTD REVP {running_reverberance}"
                    "1MTD ENVP {envelopment}"
                    "1MTD OMNI {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD AXIS {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD XRID {room_index}"
                    
                    "1MTD XDEC {rt_global, rt_low, rt_mid, rt_high, freq_low, freq_High}"	
                    "1MTD XRIR {early_start, early_end, cluster_start, cluster_end, reverb_start, modal_density, early_distr, cluster_distr}"
                    
                    "1FTD XSRC {XCAR cartesian_coordinates; XAED navigational_coordinates; XORI orientation; XAPE aperture; PRES presence; WARM warmth; BRIL brillance; PRER room_presence; REVP running_reverberance; ENVP envelopment; OMNI omni_filter; AXIS axis_filter; XRID room_index;}"

                    "1FTD XRFX {XDEC decay_times; XRIR time_response;}"
                    
                    "}")))

 
(defmethod get-3D-pairs ((self BPC))
  (mapcar #'(lambda (pt) (list (car pt) (cadr pt) 0.0)) (point-pairs self)))

(defmethod get-3D-pairs ((self 3DC))
  (point-pairs self))

; ((t sid ((x y z))) ...)

(defun merge-trajectories (frames)
  (let ((newframes nil))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (caar newframes) (car fr)))
                (setf (caddr (car newframes)) (append (caddr (car newframes)) (caddr fr)))
              (push (list (car fr) 0 (caddr fr)) newframes))))
    (reverse newframes)))

;;; merge frames for same kind of (single) matrices (adds data in the matrix)
(defun merge-frame-data (frames)
  (let ((newframes nil))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (ftime (car newframes)) (ftime fr))
                     (string-equal (signature (car newframes)) (signature fr)))
                (loop for matrix in (lmatrix fr) do
                      (let ((fmat (find (signature matrix) (lmatrix (car newframes)) :test 'string-equal :key 'signature)))
                        (if fmat 
                            (setf (data fmat) (append (data fmat) (data matrix))
                                  (num-elts fmat) (1+ (num-elts fmat)))
                          (setf (lmatrix fr) (append (lmatrix fr) (list matrix))))))
              (push (make-instance 'SDIFFrame :ftime (ftime fr) :signature (signature fr)
                                   :streamID 0 :lmatrix (lmatrix fr))
                    newframes))))
    (reverse newframes)))

;;; merge frames for same kind of frames (appends matrices)
(defun merge-frames (frames)
  (let ((newframes nil))
    (setf frames (sort frames '< :key 'ftime))
    (loop while frames do
          (let ((fr (pop frames)))
            (if (and newframes (= (ftime (car newframes)) (ftime fr))
                     (= (streamID (car newframes)) (streamID fr))
                     (string-equal (signature (car newframes)) (signature fr)))
                (setf (lmatrix (car newframes))
                      (append (lmatrix (car newframes)) (lmatrix fr)))
              (push fr newframes))))
    (reverse newframes)))

(defun gen-trajectories-frames (trajectories)
  (let (xmin xmax ymin ymax zmin zmax data) 
    (setf data
          (sort (loop for tr in trajectories
                      for i = 1 then (+ i 1) 
                      when tr append
                      (let ;((real-traj (get-full-trajectory tr)))
                          ((real-traj tr))
                        (multiple-value-bind (list xn xx yn yx zn zx)
                            (loop for 3Dp in (point-pairs real-traj)
                                  for tim in (get-points-times real-traj) 
                                  minimize (car 3Dp) into xn
                                  maximize (car 3Dp) into xx
                                  minimize (cadr 3Dp) into yn
                                  maximize (cadr 3Dp) into yx
                                  minimize (caddr 3Dp) into zn
                                  maximize (caddr 3Dp) into zx
                                  collect (make-instance 'SDIFFrame :ftime tim :streamid i 
                                                         :signature "XSRC"
                                                         :lmatrix (list (make-instance 'raw-SDIFMatrix :signature "XCAR"
                                                                                       :num-elts 1 :num-fields 3
                                                                                       :data 3Dp)))
                                  into list
                                  finally (return (values list xn xx yn yx zn zx )))
                          (setf xmin (if xmin (min xmin xn) xn))
                          (setf xmax (if xmax (max xmax xx) xx))
                          (setf ymin (if ymin (min ymin yn) yn))
                          (setf ymax (if ymax (max ymax yx) yx))
                          (setf zmin (if zmin (min zmin zn) zn))
                          (setf zmax (if zmax (max zmax zx) zx))
                          list))
                      )
                '< :key 'ftime))
    (values data xmin xmax ymin ymax zmin zmax)))


(defmethod get-multi-values ((self number) onset dur)
  ;;; whatever dur is, only one value (yaw)
  (list (list (or onset 0) (list self))))

(defmethod get-multi-values ((self cons) onset dur)
  (cond ((list-subtypep self 'number)
         ;;; whatever dur is, only one setf of values (yaw pitch roll)
         (list (list (or onset 0) self)))
        ((list-subtypep self 'cons)
         ;;; each item is a value to scale on (onset->duration) (or 1.0 if no duration)
         (let* ((o (or onset 0))
                (d (or dur 1.0))
                (times (arithm-ser o (+ o dur) (/ dur (float (1- (length self)))))))
           (loop for tim in times 
                 for vals in self collect 
                 (list tim vals))))
        (t nil)))

(defmethod get-multi-values ((self bpf) onset dur)
  ;;; BPF points (ignores duration)
  (mapcar #'(lambda (point)
              (list (+ (car point) (or onset 0))
                    (list (cadr point))))
          (point-pairs self)))

(defmethod get-multi-values ((self bpf-lib) onset dur)
  ;;; BPF points (ignores duration)
  (let ((params (mapcar 'point-pairs (bpf-list self)))) 
    (sort (remove-duplicates 
           (loop for par in params 
                 for i = 0 then (+ i 1) append 
                 (loop for val in par collect
                       (list (+ (car val) (or onset 0))
                             (loop for p in params collect
                                   (or (second (find (car val) p :key 'car :test '=))
                                       (x-transfer p (car val)))))))
           :test '= :key 'car)
          '< :key 'car)))
            
(defmethod get-multi-values ((self bpc) onset dur) nil)
(defmethod get-multi-values ((self bpc-lib) onset dur) nil)
            

(defun get-multi-value-frames (list onsetlist durlist type)
  (sort (loop for vals in list
              for d in durlist
              for o in onsetlist
              for i = 1 then (+ i 1) 
              when vals append                
              (loop for one-point in (get-multi-values vals o d)
                    collect (make-instance 'SDIFFrame :ftime (car one-point) :streamid i 
                                           :signature "XSRC"
                                           :lmatrix (list (make-instance 'raw-SDIFMatrix :signature type
                                                                         :num-elts 1 :num-fields (length (second one-point))
                                                                         :data (second one-point))))
                    ))
        '< :key 'ftime))


(defmethod get-source-values ((self number) onset dur)
  ;;; whatever dur is, only one value
  (list (list (or onset 0) (list self))))

(defmethod get-source-values ((self cons) onset dur)
  (let* ((o (or onset 0))
         (d (or dur 1.0))
         (times (arithm-ser o (+ o dur) (/ dur (float (1- (length self)))))))
    (loop for tim in times 
          for val in self collect 
          (list tim val))))
        
(defmethod get-source-values ((self bpf) onset dur)
  ;;; BPF points (ignores duration)
  (mapcar #'(lambda (point)
              (list (+ (car point) (or onset 0))
                    (list (cadr point))))
          (point-pairs self)))

(defun get-simple-value-frames (vals onsetlist durlist matrix-type)
  (sort (loop for srcvals in vals
              for d in durlist
              for o in onsetlist
              for i = 1 then (+ i 1) 
              when srcvals append                
              (loop for val-p in (get-source-values srcvals o d)
                    collect (make-instance 'SDIFFrame :ftime (car val-p) :streamid i 
                                           :signature "XSRC"
                                           :lmatrix (list (make-instance 'raw-SDIFMatrix :signature matrix-type
                                                                         :num-elts 1 :num-fields 1
                                                                         :data (second val-p))))
                    ))
        '< :key 'ftime))



(defmethod! save-spat-sdif ((self class-array) &key out stream-mode export-sounds rooms)
   :icon '(638)
   :menuins '((2 (("separate streams" sep) ("merge streams" merge))))
   :indoc '("a SPAT-MATRIX" "output SDIF file name" "export stream format" "sources export mode" "room(s) descriptions")
   :outdoc '("sdif file pathname")
   :initvals '(nil nil sep nil)
   :doc "Saves a SPAT-MATRIX into an SDIF File.

- If <out> is not specified, a file chooser opens to choose the output file.
- <stream mode> determines if source trajectories are stored in separate SDIF streams ('sep) or in a single stream ('merge)
- If <export-sounds> is T, the source files (if provided in the SPAT-MATRIX) are copied and exported to the SDIF file location.
  If <export-sound> is :temp, they are exported and registered as 'temporary files' to delete after synthesis.
- <room> is an object (or a list of objects) of type SPAT-ROOM, determining reverbs and room parameters used in the spat process. 
"
   (let* ((error nil) time
          (filepath (or (and out (handle-new-file-exists out))
                        (om-choose-new-file-dialog :types (list (format nil (om-str :file-format) "SDIF") "*.sdif" ))))
          (columns (lcontrols self))
          (nstreams (length columns))
          (sounds (find-array-field self :sounds))
          (names (find-array-field self :src-names))
          (trajects (find-array-field self :trajectories))
          (durs (find-array-field self :durations))
          (onsets (find-array-field self :onsets))
          (orients (find-array-field self :orientations))
          (aperts (find-array-field self :apertures))
          (sndtable nil) (nametable nil) (trajdurs nil)
          (spatframes nil) (orientationframes nil) (apertureframes nil)
          (omnifilterframes nil) (axisfilterframes nil)
          (roomframes nil)
          (mode (or stream-mode 'sep)))
     (when filepath
       
       ;;; SOURCES
       (unless sounds
         (setf sounds (make-list (numcols self))))

       (setf sndtable (loop for s in sounds 
                            for i = 1 then (+ i 1) 
                            collect (let ((soundpath (cond ((and (typep s 'sound) (om-sound-file-name s))
                                                            (pathname (om-sound-file-name s)))
                                                           ((or (pathnamep s) (stringp s))
                                                            (pathname s))
                                                           (t nil))))
                                      (when (and soundpath export-sounds)
                                          ; (om-make-pathname :directory filepath :name (pathname-name soundpath) :type (pathname-type soundpath))
                                        (let ((tempfile (unique-pathname filepath (pathname-name soundpath) (pathname-type soundpath))))
                                          (om-copy-file soundpath tempfile)
                                          (when (equal export-sounds :temp)
                                            (add-tmp-file tempfile))
                                          ))
                                      (cons (integer-to-string i)
                                            (list (if soundpath (string+ (pathname-name soundpath) "." (pathname-type soundpath))
                                                    "unknown-source")))
                                      )))
       
       ;;; SOURCE NAMES
       (setf nametable (loop for name in names 
                             for i = 1 then (+ i 1)
                             when name
                             collect (list (integer-to-string i) 
                                           (if (stringp name) name (format nil "~A" name)))))

       ;;; TRAJECTORIES
       (if trajects
           (setf trajects (mapcar #'(lambda (obj) 
                                      (when obj
                                        (objfromobjs obj (make-instance '3D-trajectory))))
                                  trajects))
         (setf trajects (make-list (numcols self))))
     
       (setf trajdurs (loop for i from 0 to (1- (numcols self)) collect 
                            (or (and durs (nth i durs))
                                (if (and (nth i trajects) (find-if 'numberp (times (nth i trajects)) :from-end t))
                                    nil
                                  (and sounds (nth i sounds) (sound-dur (nth i sounds)))))))
     
       ;;; scale
       
       (setf trajects 
             (mapcar #'(lambda (traj d)
                         (let ((tr (get-full-trajectory traj)))
                           (when d (setf (times tr) (om+ (car (times tr)) (om-scale (times tr) 0 d))))
                           tr)
                         ) trajects trajdurs))
         
       (when onsets 
         (mapcar #'(lambda (traj o) 
                     (when traj (setf (times traj) (om+ (times traj) o))))
                 trajects onsets))
       
       ;;; ORIENTATION
       (when orients
         (setf orientationframes (get-multi-value-frames orients onsets trajdurs "XORI")))
       
       ;;; APERTURES
       (when aperts
         (setf apertureframes (get-simple-value-frames aperts onsets trajdurs "XAPE")))
     
       ;;; FILTERS (FOR REVERB AND PERCEPTUAL EFFECTS)
       (when (find-array-field self :omni-filter)
         (setf omnifilterframes (get-multi-value-frames (find-array-field self :omni-filter) onsets trajdurs "OMNI")))
       (when (find-array-field self :axis-filter)
         (setf axisfilterframes (get-multi-value-frames (find-array-field self :axis-filter) onsets trajdurs "AXIS")))


       ;;; PERCEPTUAL PARAMS
       (setf pereptualframes (remove nil (append
                                          (get-simple-value-frames (find-array-field self :presence) onsets trajdurs "PRES")
                                          (get-simple-value-frames (find-array-field self :warmth) onsets trajdurs "WARM")
                                          (get-simple-value-frames (find-array-field self :brillance) onsets trajdurs "BRIL")
                                          (get-simple-value-frames (find-array-field self :room-presence) onsets trajdurs "PRER")
                                          (get-simple-value-frames (find-array-field self :running-reverberance) onsets trajdurs "REVP")
                                          (get-simple-value-frames (find-array-field self :envelopment) onsets trajdurs "ENVP")
                                          )))
       
       (setf ridframes (get-simple-value-frames (find-array-field self :room) onsets trajdurs "XRID"))
       
       
       (when rooms
         (setf roomframes (sort (flat (mapcar 'gen-room-frames (list! rooms))) '< :key 'ftime)))
         
       (multiple-value-bind (spatframes xmin xmax ymin ymax zmin zmax) 
           (gen-trajectories-frames trajects)
         
         ;;; MERGE MODE
         (when (equal 'merge mode)
           (setf spatframes (merge-frame-data spatframes))
           (setf orientationframes (merge-frame-data orientationframes))
           (setf apertureframes (merge-frame-data apertureframes))
           (setf pereptualframes (merge-frame-data pereptualframes))
           (setf omnifilterframes (merge-frame-data omnifilterframes))
           (setf axisfilterframes (merge-frame-data axisfilterframes))
           (setf ridframes (merge-frame-data ridframes))

           (setf roomframes (merge-frame-data roomframes))
           )
     
         ;;; WRITE SDIF
         (let* ((outfile (sdif::sdif-open-file (namestring filepath) :eWriteFile))
                (datatype 4)
                (sdifvalues (om-make-pointer (* 3 datatype (if (equal 'merge mode) (numcols self) 1)))))
           (sdif::SdifFWriteGeneralHeader outfile)
           (write-nvt-tables outfile (remove nil 
                                             (list (default-om-NVT)
                                                   (make-instance 'SDIFNVT 
                                                                  :tablename "Sources"
                                                                  :ID 0
                                                                  :NV-pairs sndtable)
                                                   (make-instance 'SDIFNVT 
                                                                  :tablename "Dimensions"
                                                                  :ID 0
                                                                  :NV-pairs (list (list "Xmin" (format nil "~D" xmin))
                                                                                  (list "Xmax" (format nil "~D" xmax))
                                                                                  (list "Ymin" (format nil "~D" ymin))
                                                                                  (list "Ymax" (format nil "~D" ymax))
                                                                                  (list "Zmin" (format nil "~D" zmin))
                                                                                  (list "Zmax" (format nil "~D" zmax))
                                                                                  ))
                                                   (when t ; nametable 
                                                     (make-instance 'SDIFNVT 
                                                                    :tablename "SourceNames"
                                                                    :ID 0
                                                                    :NV-pairs nametable))
                                                   )))   
           (when *spat-sdiftypes*
             (write-sdif-types outfile *spat-sdiftypes*))
           (sdif::SdifFWriteAllASCIIChunks outfile)
     
           (loop for frame in (merge-frames (append spatframes orientationframes apertureframes pereptualframes omnifilterframes axisfilterframes ridframes roomframes)) do 
                 (save-sdif frame outfile))

           (sdif::sdif-close-file outfile)
           (om-free-pointer sdifvalues)
           )
         (probe-file (om-namestring filepath))
         ))))



