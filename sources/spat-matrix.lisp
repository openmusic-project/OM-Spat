;;;===========================================================================
;;; OM-Spat
;;; Rendering of spatial scene descriptions 
;;; J. Bresson (IRCAM - 2010)
;;;===========================================================================

;;;===========================================================================
;;; SPAT-MATRIX
;;;===========================================================================

(in-package :om)



(defclass! spat-matrix (class-array) 
  ((sounds :initarg :sounds :initform nil)
   (src-names :initarg :src-names :initform nil)
   (trajectories :initarg :trajectories :initform nil)
   (durations :initarg :durations :initform nil)
   (onsets :initarg :onsets :initform nil)
   (orientations :initarg :orientations :initform nil)
   (apertures :initarg :apertures :initform nil)
   (presence :initarg :presence :initform 90)
   (warmth :initarg :warmth :initform 30)
   (brillance :initarg :brillance :initform 30)
   (room-presence :initarg :room-presence :initform 48)
   (running-reverberance :initarg :running-reverberance :initform 34)
   (envelopment :initarg :envelopment :initform 24)
   (omni-filter :initarg :omni-filter :initform '((0 1.7 0 -3.8 177 5657)))
   (axis-filter :initarg :axis-filter :initform '((0 0 0 0 177 5657)))
   (room :initarg :room :initform 1)
   )
  (:documentation "
SPAT-MATRIX is a structured representation of a spatial sound scene. It is a subclass and works like a CLASS-ARRAY: optional parameters can be added and selected.

The current available parameters are 

- <sounds> : list of sound sources  [SOUND object(s) or pathnames] 
- <src-names> : list of source names  [strings]

- <trajectories> : list of trajectories  (BPC, 3DC or 3D-trajectory) [m]
- <durations> : list of durations on which trajectories are eventually mapped [s]
- <onsets> : list of onset times for beginning of sources palyback and movement [s]
- <orientation> : lis of values or BPS for yaws [deg], or lists (yaw, pitch, roll), or BPF-libs
- <apertures> : list of values or BPFs for source aperture [deg]

PERCEPTUAL PARAMETERS (From IRCAM Spat~):
- <presence> : Source presence (energy of direct sound and early room effect) [0-120] (Spat~ default = 90)
- <warmth> : Source warmth (variation of early sound at low frequencies) [0-60] (Spat~ default = 30)
- <brillance> : Source brilance (variation of early sound at high frequencies) [0-60] (Spat~ default = 30)
- <room-presence> : Energy of later reflections and reverberation [0-120] (Spat~ default = 48)
- <running-reverberance> : Early decay time [0-50] (Spat~ default = 34)
- <envelopment> Energy of early room effect relative to direct sound [0-50] (Spat~ default = 24)

RADIATION FILTERS (From IRCAM Spat~):
The filters are described by a global gain, the gain in the 3 bands, and the cross-over frequencies between the low/mid bands and the mid/high bands. 
(global_gain [dB], gain_low [dB], gain_mid [dB], gain_high [dB], freq_low [Hz], freq_high [Hz])
These values are optional and can be specified as constants, BPFs, or BPF-libs
- <omni-filter> : 3-bands parametric filter applied to the reverberated sound before room effect processing (Spat~ default = [0 1.7 0 -3.8 177 5657] ).
- <axis-filter> : 3-bands parametric filter applied to the direct sound (Spat~ default = [0 0 0 0 177 5657]).

ROOM: The sources are sent to a reverb processor to generate the room effects.
- <room> = integer or list of integers: room index for the different sources (rooms parameters are supposed to be provided separately, see SPAT-ROOM and SPAT-SYNTH).
")
  (:icon :planets))
   
(defmethod array-data-from-control ((controlvalue BPC) numcols)
  (loop for i from 0 to (- numcols 1)
        collect (eval (omng-copy controlvalue))))

(defmethod get-slot-in-out-names ((self spat-matrix))
   (values '("self" "numcols") '(nil 1)
           '("Synthesis event" "Number of components [int]")
           '(nil nil)))



