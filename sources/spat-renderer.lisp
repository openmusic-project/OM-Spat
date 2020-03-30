;;;===========================================================================
;;; OM-Spat
;;; Rendering of spatial scene descriptions 
;;; J. Bresson (IRCAM - 2010)
;;;===========================================================================

;;;===========================================================================
;;; spat renderer interface
;;;
;;; Requires perliminary installation of spatdif.renderer (protected Ircam)
;;; Spat renderer by T. Carpentier (IRCAM - 2010)
;;;=============================================================================

(in-package :om)

;;;===========================

(defmethod get-speakers-file ((self pathname)) self)
(defmethod get-speakers-file ((self string)) (pathname self))

(defmethod get-speakers-file ((self t)) 
  (let ((spk (get-speakers-list self))
        (spkfile (tmpfile "speakers.txt")))
    (when spk
      (save-params (cons '("xyz") spk) spkfile))))

(defmethod get-speakers-list ((self t)) nil)
(defmethod get-speakers-list ((self list)) self)

(defmethod get-speakers-list ((self textfile)) 
  (list-of-data (buffer-text self)))

(defmethod get-speakers-list ((self 3DC)) 
  (point-pairs self))

(defmethod get-speakers-list ((self BPC)) 
  (mapcar #'(lambda (2Dp) (append 2Dp (list 0))) (point-pairs self)))

;;; to deduce the number of channels...
(defmethod get-speakers-list ((self string)) 
  (get-speakers-list (pathname self)))


(defmethod get-speakers-list ((self pathname)) 
  (when (probe-file self)
    (with-open-file (in self :direction :input)
      (let ((line nil)
            (spk-list nil))
        (loop while (not (equal line :eof)) do 
              (setf line (om-correct-line (read-line in nil :eof)))
              ;(print (list line (type-of line) (string-equal line "xyz")))
              (when (and line 
                         (not (equal line :eof))
                         (not (string-equal line "xyz")))
                (setf spk-list (append spk-list (list (data-from-line line))))))
        spk-list))))

;;; HRTF 

(defmethod get-hrtf-file ((self string))
  (get-hrtf-file (pathname self)))

(defmethod get-hrtf-file ((self pathname))
  (if (and (probe-file self)
           (string-equal (pathname-type self) "hrtf"))
      self
    (progn 
     (om-beep-msg (string+ "HRTF file not found: " (namestring self)))
     (get-hrtf-file nil)
     )))
                                                                                
(defmethod get-hrtf-file ((self t))
  (om-print (string+ "Using default OM-Spat HRTF: " (namestring *spat-default-hrtf*)))
  *spat-default-hrtf*)


;;;===========================

(defmethod! spat-synth ((self string) out-config &key out-path (panning-type 'angular) rooms pos-interpol)
   :icon '(410)
   :initvals '(nil 2 nil angular nil 30)
   :menuins '((3 (("angular" angular) ("vbap" vbap) ("binaural" binaural))))
   :indoc '("sdif file or spat-matrix" "out/speakers config" "output file pathname" "type of spatialization" "room description(s)" "position interpolation")
   :outdoc '("sound file pathname")
   :doc "Synthesizes a multichannel sound file from a SPAT-MATRIX or an SDIF file using Spat 4.

Requires preliminary installation of Spat and path setup in OM prefernces.

<self> can be a pathname to the SDIF file, and SDIFFILE object, or a SPAT-MATRIX.

<out-config> is the positions for the speakers (positions in 3D - xyz), provided as a BPC, 3DC, as a list, a textfile or a text file pathname. 

Alternatively, <out-config> can be :
- An integer for the number of speakers (whose positions will be given default values)
- A pathname to an HRTF file (for 'binaural' panning type only). A default HRTF is used if not specified.

<out-path> is the pathname of the file written by Spat

<panning-type> determines the type of spatialization performed by Spat: angular, vbap, or binaural rendering.

<rooms> allows to connect one or a list of SPAT-ROOM object(s) in order to complement the SPAT-MATRIX with room descriptions.

<pos-interpol> determines the interpolation time (in ms) between successive positions (in order to avoid jumps and clicks).
If <pos-interpol> = T (or anything else instead of a duration in ms) the interpolation takes the time of the whole duration between the successive positions.


Spat by Thibaut Carpentier, Ircam Acoustic/Cognitive Spaces, 2010.
" 
   (let* ((outfile (handle-new-file-exists (or out-path (om-choose-new-file-dialog :types (list (om-str :all-files) "*.*" 
                                                                                                (format nil (om-str :file-format) "AIFF") "*.aiff" 
                                                                                                (format nil (om-str :file-format) "WAV") "*.wav")))))

          (mode (or panning-type *spat-default-panning*))
          (nch  (cond ((equal mode 'binaural) 2)
                      ((numberp out-config) out-config)
                      (t (or (length (get-speakers-list out-config))
                             *spat-default-numout*))))
          ;(nin (length (sdifstreams (load-sdif-file self))))
          (hrtffile (if (equal mode 'binaural) (string+ " -H \"" (namestring (get-hrtf-file out-config)) "\"") ""))
          (speakers "")
          (decoding (if (stringp *spat-default-decoding*) (string+ " -d " *spat-default-decoding*) ""))
          (buffersize (or *spat-default-buffersize* 512))
          ;(nrev (or *spat-default-nreverb* 1))
          (nrev (if rooms (if (numberp rooms) rooms (length (list! rooms))) 1))
          (outformat (if (equal *def-snd-format* 'wav) "wav" "aiff"))
          (nspatchannels (or *spat-default-nchannels* 8))
          (interpol (if (numberp pos-interpol) pos-interpol -1)))
     (when outfile
       (setf outfile (om-make-pathname :directory outfile :name (pathname-name outfile)
                                       :type outformat)) ;;; because Spat always create "aiff" extension (not "aif")
       (when (and out-config (not (equal mode 'binaural)))
         (let ((speakers-file (get-speakers-file out-config)))
           (when (and speakers-file (probe-file speakers-file))
             (add-tmp-file speakers-file)
             (setf speakers (string+ " -S \"" (namestring speakers-file) "\"")))))
     
       (when (forum-protec *spat-renderer* (find-library "OM-Spat"))
         (if (probe-file *spat-renderer*)
           (om-cmd-line (format nil "~s -p ~A ~A -o ~D -r ~D -I ~D -M 1 -f ~s -D ~s -s ~s -O ~s -F ~s -v 1 -b ~D ~A ~A -i ~A"
                                (namestring *spat-renderer*)
                                (string-downcase mode) decoding nch nrev nspatchannels
                                self
                                (namestring (om-make-pathname :directory outfile))
                                (namestring (tmpfile nil)) ;(om-make-pathname :directory outfile)) (tmpfile nil)
                                (pathname-name outfile)
                                outformat
                                buffersize
                                hrtffile
                                speakers
                                interpol)
                        *sys-console*)
         (om-beep-msg "Spat renderer not found. Set path in OM Preferences !!!")))
       (when *delete-inter-file* (clean-tmp-files)) 
       (and outfile (probe-file outfile)))))

(defmethod! spat-synth ((self pathname) out-config &key out-path (panning-type 'angular) rooms pos-interpol)
    (declare (ignore rooms))
    (spat-synth (namestring self) out-config :out-path out-path :panning-type panning-type :pos-interpol pos-interpol :rooms rooms))

(defmethod! spat-synth ((self sdiffile) out-config &key out-path (panning-type 'angular) rooms pos-interpol)
    (declare (ignore rooms))
    (spat-synth (filepathname self) out-config :out-path out-path :panning-type panning-type :pos-interpol pos-interpol :rooms rooms))

(defmethod! spat-synth ((self spat-matrix) out-config &key out-path (panning-type 'angular) rooms pos-interpol)
   (let* ((tmppath (handle-new-file-exists (tmpfile "spat.sdif")))
          (sdif (save-spat-sdif self :stream-mode 'sep :out tmppath :export-sounds :temp :rooms rooms)))
     (add-tmp-file tmppath)
     (spat-synth sdif out-config :out-path out-path :panning-type panning-type :pos-interpol pos-interpol :rooms rooms)
     ))

#|
spatdif.renderer options:          
spatdif.renderer options :         
   -f <sdif file fullpath>         
   -D <destination folder>         
   -O <output name>                            (default : spat_out)
   -F <output format>                          (default : aiff)
   -i <number of sources/inputs>               (default : 1)
   -o <number of speakers/outputs>             (default : 8)
   -I <number of internal channels>            (default : 8)
   -r <number of reverbs>                      (default : 1)
   -b <buffersize>                             (default : 512)
   -v <verbose>                                (default : 0)
   -M <multichannel output file>               (default : 0)
   -p <panning type>                           (default : angular)
       [ binaural, xy, ms, ab, surround, panr, angular, vbap2d, vbap, dbap, bformat, hoa2d, hoa3d, null ]
   -d <decoding type>                          (default : null)
       [ null, transaural, bformat, hoa2d, hoa3d, surround, bformat2uhj, uhj2bformat ]
   -H <hrtf file fullpath>      
   -S <speakers file>              
   File format is as follow:
    xyz
    1 -1 1 
    0.7 -0.7 0
    1 2 3

Ex: ./spatdif.renderer -p angular -i 1 -o 4 -d null -f ../test/spat-test-simple.sdif -r 1 -D ../test -v 1 -b 32 -M 1 -S ../test/speakers.txt -F aiff -O mySpatOutput
|#
