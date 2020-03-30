;;;===========================================================================
;;; OM-Spat
;;; Representation and rendering of spatial scene descriptions
;;; J. Bresson (IRCAM - 2011)
;;;===========================================================================

;;;===========================================================================
;;; Room description
;;;===========================================================================

(in-package :om)


(defclass! spat-room (time-array) 
           ((index :initform 1 :accessor index :documentation "a unique index identifying the room (int)")
           
            (early-start :initarg :early-start :initform 22.22 :documentation "starting time for the early reflections (ms) [1-120]")
            (early-end :initarg :early-end :initform 39.71 :documentation "ending time for the early reflections (ms) [1-120]")
            (early-dist :initarg :early-dist :initform 0.5 :documentation "distribution law for the early reflections [0.1-0.9]")
            (cluster-start :initarg :cluster-start :initform 42.47 :documentation "starting time for the cluster reflections (ms) [1-300]")
            (cluster-end :initarg :cluster-end :initform 105.64 :documentation "ending time for the cluster reflections (ms) [1-300]")
            (cluster-dist :initarg :cluster-dist :initform 0.5 :documentation "cluster distribution [0.1-0.9]")
            (reverb-start :initarg :reverb-start :initform 95.37 :documentation "starting time for the late reverb (ms) [1-500]")
            (modal-density :initarg :modal-density :initform 0.86 :documentation "modal density of the late reverberation [0.2-4.0]")        

            (global-decay :initarg :global-decay :initform 1.995 :documentation "global reverberation time (s)")
            (low-decay :initarg :low-decay :initform 1.0 :documentation "decay time in the low frequency band (relatively to the global decay time)")
            (mid-decay :initarg :mid-decay :initform 1.0 :documentation "decay time in the mid frequency band (relatively to the global decay time)")
            (high-decay :initarg :high-decay :initform 0.5 :documentation "decay time in the high frequency band (relatively to the global decay time)")
            (decay-fq-low :initarg :decay-fq-low :initform 250 :documentation "cross-over frequency between the low and mid bands (Hz)")
            (decay-fq-high :initarg :decay-fq-high :initform 8000 :documentation "cross-over frequency between the mid and high bands (Hz)")
            )
           (:icon 502)
           (:documentation "
SPAT-ROOM represents room parameters in a spatialization process. 
These parameters are adapted from IRCAM Spat~.

SPAT-ROOM is a subclass of TIME-ARRAY: all the slots are optionals and have an implicit default value, unless visible (ALT + ->) and initialized.

<TIMES> specifies the list of time positions to which all parameters' input lists will be matched.
Each parameter can be a number, a list, or a BPF (sampled according to the time-points).

<INDEX> is the room index as referred in the SPAT-MATRIX objects.

The time structure of the room response is described by <early-start>, <early-end>, <cluster-start>, <cluster-end>, <reverb-start> and <modal-density>, <early-dist> and <cluster-dist> define the parameters of the late, 'cluster' and early reverbs.

- <early-start> : starting time for the early reflections (ms) [1-120]
- <early-end> : ending time for the early reflections (ms) [1-120]
- <early-dist> : distribution law for the early reflections [0.1-0.9]
- <cluster-start> : starting time for the cluster reflections (ms) [1-300]
- <cluster-end> : ending time for the cluster reflections (ms) [1-300]
- <cluster-dist> : cluster distribution [0.1-0.9]
- <reverb-start> : starting time for the late reverb (ms) [1-500]
- <modal-density> : modal density of the late reverberation [0.2-4.0]      

The room (late) decay is described by a global reverberation time and the relative decay times in 3 frequency bands (<global-decay>, <low-decay>, <mid-decay>, <high-decay>, <decay-fq-low> and <decay-fq-high>).

- <global-decay> : global reverberation time (s)
- <low-decay> : decay time in the low frequency band (relatively to the global decay time)
- <mid-decay> : decay time in the mid frequency band (relatively to the global decay time)
- <high-decay> : decay time in the high frequency band (relatively to the global decay time)
- <decay-fq-low> : cross-over frequency between the low and mid bands (Hz)
- <decay-fq-high> : cross-over frequency between the mid and high bands (Hz)
"))

(defmethod fixed-slots-list ((self spat-room)) '(times index))

(defmethod get-slot-in-out-names ((self spat-room))
   (values '("self" "times" "index") '(nil (0) 1)
           '("time-array object" "sorted list of onsets [seconds]" "a unique index identifying the room (int)")
           '(nil nil nil)))

(defun gen-room-frames (spat-room)
  (loop for frtime in (butlast (times spat-room))
        for i = 0 then (+ i 1)
        collect
        (let ((comp (get-comp spat-room i)))
          (make-instance 'SDIFFrame :ftime frtime :streamid (index spat-room) 
                         :signature "XRFX"
                         :lmatrix (list (make-instance 'raw-SDIFMatrix :signature "XRIR"
                                                       :num-elts 1 :num-fields 8
                                                       :data (list (comp-field comp "early-start") (comp-field comp "early-end") 
                                                                   (comp-field comp "cluster-start") (comp-field comp "cluster-end") 
                                                                   (comp-field comp "reverb-start")
                                                                   (comp-field comp "modal-density") (comp-field comp "early-dist") (comp-field comp "cluster-dist")))
                                        (make-instance 'raw-SDIFMatrix :signature "XDEC"
                                                       :num-elts 1 :num-fields 6
                                                       :data (list (comp-field comp "global-decay") 
                                                                   (comp-field comp "low-decay") (comp-field comp "mid-decay") (comp-field comp "high-decay") 
                                                                   (comp-field comp "decay-fq-low") (comp-field comp "decay-fq-high")))))
                         )))





