;;;===================================================
;;;
;;; OM-Spat
;;; Representation and rendering of spatial scene descriptions
;;;
;;; IRCAM - Respresentations Musicales 
;;;
;;; Authors/Contributors :	Jean Bresson 
;;;			      Thibaut Carpentier (IRCAM - EAC)
;;;  				Marlon Schumacher (CIRMMT/IDMIL/DCS, McGill University) 
;;; 				Gilbert Nouno, Carlos Agon
;;;===================================================


(in-package :om)

(compile&load (namestring (om-relative-path '("sources") "forum-protec")))
(compile&load (namestring (om-relative-path '("sources") "spat-matrix")))
(compile&load (namestring (om-relative-path '("sources") "spat-sdif")))
(compile&load (namestring (om-relative-path '("sources") "spat-room")))
(compile&load (namestring (om-relative-path '("sources") "spat-prefs")))
(compile&load (namestring (om-relative-path '("sources") "spat-renderer")))
(compile&load (namestring (om-relative-path '("sources") "sdif-player")))


(om::fill-library '((nil nil (spat-matrix spat-room) (save-spat-sdif spat-synth) nil)))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(om::set-lib-release 2.10)

;; v 2.4: player adapted for OM 6.7
;; v 2.5: player adapted for OM 6.8
;; v 2.6: Linux compat
;; v 2.7: fix in spat-room gen frames + new binaries
 
(doc-library "
OM-Spat is a library for the creation and rendering of spatial scenes in OpenMusic.

The scenes are represented as matrix objects (class SPAT-MATRIX) containing the information and spatial data of a number of sources.

OM-Spat implements a storage of the source trajectories and spatial attributes using the SDIF format. Generated SDIF files can be rendered by the Spat kernel or streamed to real-time environments via the Spat-SDIF-Player (applications to be installed separately).

Authors/Contributors :	
- Jean Bresson (IRCAM - Representations Musicales)
- Thibaut Carpentier (IRCAM - EAC)
- Marlon Schumacher (CIRMMT/IDMIL/DCS, McGill University) 
- OM-Spat is inspired from previous works by Carlos Agon and Gilbert Nouno. 
")


(om-print "
;;;============================================================
;;; OM-Spat 2.10
;;; Representation and rendering of spatial scene descriptions
;;; (c) IRCAM - Representations Musicales + Acoustic and Cognitive Spaces
;;;============================================================
")

;;; (gen-lib-reference (find-library "OM-Spat"))


