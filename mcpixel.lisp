(cl:defpackage :mcpixel
  (:use :clim-lisp :clim)
  (:shadow :frame :frame-p :frame-name))

(in-package :mcpixel)

;;;; FIXME: This program would be improved by proper use of accept
;;;; methods. Currently it often requires the user to point with the
;;;; mouse, even where it seems reasonable to enter text.

(defvar *pattern-cache* (make-hash-table :test 'equalp))

;;;;

(defstruct frame pattern name ox oy)
(defun frame-width (frame) (array-dimension (frame-pattern frame) 1))
(defun frame-height (frame) (array-dimension (frame-pattern frame) 0))

;;;;

(define-presentation-type frame ())
(define-presentation-type color ())

;;;;

(defclass paint-pane (basic-gadget)
  ((fr :accessor fr :initform nil :initarg :fr)
   (button-pressed :accessor button-pressed :initform nil)
   (drawbrush :accessor drawbrush :initform nil :initarg :drawbrush)
   (zoom :accessor zoom :initform 10 :initarg :zoom)))

(defun fill-pixel (pane frame x y design)
  (let* ((zoom (zoom pane))
         (y0 (* y zoom))
         (x0 (* x zoom)))
    (draw-rectangle* pane x0 y0 (+ x0 zoom) (+ y0 zoom) :filled t :ink (pane-background pane))
    (cond
      ((eql design +transparent-ink+)
       (draw-rectangle* pane x0 y0 (+ x0 zoom -1) (+ y0 zoom -1) :filled nil :ink +gray90+))
      (t 
       (draw-rectangle* pane x0 y0 (+ x0 zoom -1) (+ y0 zoom -1) :filled t :ink design)))
    (when (and (eql x (frame-ox frame))
               (eql y (frame-oy frame)))
      (draw-text* pane "*" (+ x0 (floor zoom 2)) (+ y0 (floor zoom 2))
                  :align-x :center :align-y :center
                  :ink +flipping-ink+))))


(defmethod handle-repaint ((pane paint-pane) region) 
  ;;(draw-design pane (compose-in (pane-background pane) region))
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    (draw-rectangle* pane x0 y0 x1 y1 :ink (pane-background pane)))
  (when (fr pane)
    (let* ((frame (fr pane))
           (pattern (frame-pattern frame))
           (palette (coerce (palette *application-frame*) 'vector))
           (width (frame-width frame))
           (height (frame-height frame)))
      (loop for y from 0 below height do
            (loop for x from 0 below width 
                  as color-index = (aref pattern y x)
                  as design = (if (< color-index (length palette))
                                  (elt palette color-index)
                                  +transparent-ink+)
                  do
                  (fill-pixel pane frame x y design))))))

(defun paint-motion (pane event)
  (when (fr pane)
    (let* ((mx (pointer-event-x event))
           (my (pointer-event-y event))
           (cx  (floor mx (zoom pane)))
           (cy  (floor my (zoom pane)))
           (color (current-color *application-frame*))
           (frame (fr pane))
           (pattern (frame-pattern frame))
           (width (frame-width frame))
           (height (frame-height frame))
           (brush (or (drawbrush pane) (get-current-brush)))
           (brush-pat (frame-pattern brush))
           (brush-width (frame-width brush))
           (brush-height (frame-height brush)))
      (loop for x upfrom (- cx (frame-ox brush))
            for bx from 0 below brush-width do
            (loop for y upfrom (- cy (frame-oy brush))
                  for by from 0 below brush-height do

                  (when (and (< x width) (< y height) (>= x 0) (>= y 0))
                    (let ((bcol (aref brush-pat by bx)))
                      (unless (zerop bcol) 
                        (incf bcol (1- color))
                        (setf (aref pattern y x) bcol)                      
                        (fill-pixel pane (fr pane) x y (or (nth bcol (palette *application-frame*)) +transparent-ink+))))))))))

(defmethod handle-event ((pane paint-pane) (event pointer-motion-event))
  (when (button-pressed pane)
    (paint-motion pane event)))

(defmethod handle-event ((pane paint-pane) (event pointer-button-press-event))
  (when (eql (pointer-event-button event) 1)
    (setf (button-pressed pane) t)
    (paint-motion pane event)))

(defmethod handle-event ((pane paint-pane) (event pointer-button-release-event))
  (when (eql (pointer-event-button event) 1)
    (setf (button-pressed pane) nil)
    (paint-motion pane event)))

(defvar *brushes* (make-hash-table :test 'equal))

(defun make-default-brush ()
  (let ((pattern (make-array '(7 7) :initial-element 0)))
    (setf (aref pattern 3 3) 1)
    (make-frame :pattern pattern :ox 3 :oy 3 :name "Brush")))

(define-application-frame mcpixel ()
  ((palette :accessor palette
            :initform (list +transparent-ink+
                            +black+
                            +gray40+
                            +blue+
                            +red+
                            +gray80+
                            +white+)
            :initarg :palette)
   (frames :accessor frames
           :initform nil
           :initarg :frames)
   (anims :accessor anims
          :initform nil
          :initarg :seqs)
   (filename :accessor filename
             :initform nil
             :initarg :filename)

   ;;;; Editor state
   (animating-stack :accessor animating-stack :initform nil)
   (animating-countdown :accessor animating-countdown :initform nil)
   (animation-rate :accessor animation-rate
                   :initform 15
                   :initarg :animation-rate)
   (current-color :accessor current-color
                  :initform 1
                  :initarg :current-color)
   (current-anim   :accessor current-anim
                   :initform nil
                   :initarg :current-seq)
   
   (current-frame :accessor current-frame
                  :initform nil
                  :initarg :current-frame))

  (:panes
   (palette     :application-pane :display-function 'display-palette :display-time t :max-height 37 :min-height 37 :height 37)
   (frames-list :application-pane :display-function 'display-frames  :display-time t :end-of-line-action :allow)
   (sequence-list :application-pane :display-function 'display-sequence :display-time nil :end-of-line-action :allow)
   (editor  (make-pane 'paint-pane))  
   (brush-editor (make-pane 'paint-pane
                            :fr (make-default-brush)
                            :zoom 20 
                            :drawbrush (make-default-brush)))
   (hue-slider (make-pane 'hue-slider))
   (intensity-slider (make-pane 'intensity-slider))
   (saturation-slider (make-pane 'saturation-slider))
   (animation-preview (make-pane 'animation-preview :max-width 300))
   (interactor :interactor-pane))
  
  (:layouts
   (default
    (vertically (:width 1300 :height 900)
      palette      
      (horizontally ()
        (3/4 (scrolling () editor))
        (1/4 (vertically ()
               (horizontally ()
                 (labelling (:label "Frames")
                   (clim-extensions:lowering ()
                     (scrolling () frames-list)))
                 (labelling (:label "Sequence")
                   (clim-extensions:lowering ()
                     (scrolling () sequence-list))))
               hue-slider
               intensity-slider
               saturation-slider
               (labelling (:label "Brush Editor")
                 brush-editor))))
      (200 (horizontally () 
             (scrolling () interactor)
             animation-preview))))))

(defun get-current-brush (&optional (frame *application-frame*))
  (or (fr (find-pane-named frame 'brush-editor))
      (make-default-brush)))

(defun curframe () (current-frame *application-frame*))

(defun display-palette (frame stream)
  (format-items (alexandria:iota (length (palette frame)))
                :stream stream
                :presentation-type 'color
                :printer (lambda (number stream)
                           (let ((design (elt (palette frame) number)))
                             (if (eql design +transparent-ink+)
                                 (draw-text* stream "TR" 16 16 :align-x :center :align-y :center)
                                 (draw-rectangle* stream 0 0 32 32 :filled t :ink design))
                             (draw-rectangle* stream 0 0 32 32 :filled nil :ink +black+
                                              :line-thickness (if (eql number (current-color frame))
                                                                  3
                                                                  1))))))

(defun display-frames (frame stream)
  (loop for frame in (frames frame) do
    (with-output-as-presentation (stream frame 'frame)
      (with-text-face (stream (if (eql frame (curframe)) :bold :roman))
        (format stream "~A (~:Dx~:D)~%"
                (frame-name frame)
                (frame-width frame)
                (frame-height frame))))))

(defmethod frame-standard-output ((frame mcpixel)) (find-pane-named frame 'interactor))
(defmethod frame-standard-input  ((frame mcpixel)) (find-pane-named frame 'interactor))

(define-presentation-method present (object (type frame) stream (view textual-view) &key)
  (format stream "[Frame ~A]" (frame-name object)))

(defun find-editor () (find-pane-named *application-frame* 'editor))
(defun repaint-editor () (repaint! 'editor))

(defun redisplay-animation ()
  (redisplay-frame-pane *application-frame* 'sequence-list :force-p t))

(defun redisplay-frames-list ()
  (redisplay-frame-pane *application-frame* 'frames-list :force-p t)
  (redisplay-animation))

(defun repaint! (gadget)
  (when (symbolp gadget)
    (setf gadget (find-pane-named *application-frame* gadget)))
  (repaint-sheet gadget (sheet-region gadget)))

(defun redisplay-palette ()
  (redisplay-frame-pane *application-frame* 'palette :force-p t)
  (repaint! 'hue-slider)
  (repaint! 'intensity-slider)
  (repaint! 'saturation-slider))

(defun resize-editor-for-frame (frame)
  (let* ((ed (find-editor))
         (width  (max 100 (* (zoom ed) (frame-width  frame))))
         (height (max 100 (* (zoom ed) (frame-height frame)))))
    (change-space-requirements ed
                               :min-width width :width width 
                               :min-height height :height height)))

(define-mcpixel-command (com-select-frame :name t)
    ((frame 'frame :gesture :select))
  (setf (current-frame *application-frame*) frame
        (fr (find-editor)) frame)
  (repaint-editor)
  (resize-editor-for-frame frame)
  (redisplay-frames-list))

(defun add-new-frame (frame)
  (setf (frames *application-frame*)
        (append (frames *application-frame*) (list frame)))
  (com-select-frame frame))

(define-mcpixel-command (com-new-frame :name t)
    ((name 'string :prompt "Name") 
     (width 'integer :prompt "Width"
            :default (if (curframe) (frame-width (curframe)) 32))
     (height 'integer :prompt "Height"
             :default (if (curframe) (frame-height (curframe)) 32)))
  (let ((new (make-frame :pattern (make-array (list height width) :initial-element 0)
                         :ox (if (curframe) 
                                 (min (frame-ox (curframe)) (1- width))
                                 (floor width 2))
                         :oy (if (curframe)
                                 (min (frame-oy (curframe)) (1- height))
                                 (floor height 2))
                         :name name)))
    (add-new-frame new)))

(define-mcpixel-command (com-rename-frame :name t)
    ((name 'string))
  (when (curframe)
    (setf (frame-name (curframe)) name)
    (redisplay-frames-list)))

(define-mcpixel-command (com-clone-frame :name t)
    ((name 'string :prompt "Name for new frame"))
  (cond
    ((not (curframe))
     (format t "~&No current frame to clone!~%"))
    (t (let ((copy (clone-frame (curframe))))
         (setf (frame-name copy) name)
         (add-new-frame copy)))))

(define-mcpixel-command (com-delete-frame :name t)
    ((frame 'frame))
  (setf (frames *application-frame*)
        (delete frame (frames *application-frame*)))
  (when (eql (curframe) frame)
    (setf (current-frame *application-frame*) nil
          (fr (find-editor)) nil)
    (repaint-editor))
  (redisplay-frames-list))

(define-mcpixel-command (com-zoom :name t)
    ((zoom 'integer))
  (cond
    ((< zoom 6) (format t "~&Zoom must be at least 6 pixels.~%"))
    (t 
     (setf (zoom (find-pane-named *application-frame* 'editor)) zoom)
     (when (current-frame *application-frame*)
       (resize-editor-for-frame (current-frame *application-frame*)))
     (repaint-editor))))

(define-mcpixel-command (com-color :name t)
    ((color 'color :gesture :select))
  (setf (current-color *application-frame*) color)
  (redisplay-palette))

(define-mcpixel-command (com-set-origin :name t)
    ((x 'integer) (y 'integer))
  (when (curframe)
    (setf (frame-ox (curframe)) x
          (frame-oy (curframe)) y)
    (repaint-editor)))

;;;; Brushes

(defun copy-matrix (matrix)
  (let ((copy (make-array (array-dimensions matrix))))
    (loop for i from 0 below (array-total-size copy)
          do (setf (row-major-aref copy i) (row-major-aref matrix i))
          finally (return copy))))

(defun clone-frame (frame)
  (let ((copy (copy-frame frame)))
    (setf (frame-pattern copy) (copy-matrix (frame-pattern copy)))
    copy))

(define-mcpixel-command (com-save-brush :name t)
    ((name 'string))
  (setf (gethash name *brushes*) (clone-frame (get-current-brush))))  

;;; This handling of brushes by name is utterly un-CLIM-like, and
;;; usability suffers, but I just don't feel like mucking with accept
;;; methods and translators right now.

(define-mcpixel-command (com-restore-brush :name t)
    ((name 'string))
  (let ((brush (gethash name *brushes*))
        (b-ed (find-pane-named *application-frame* 'brush-editor)))
    (cond
      (brush
       (setf (fr b-ed) (clone-frame brush))
       (repaint! b-ed))
      (t (format t "No brush named \"~A\" defined. Try \"Show Brushes\" to list defined brushes.~%" name)))))

(define-presentation-type brush ())

(define-mcpixel-command (com-show-brushes :name t)
    ()
  (format-items (loop for name being the hash-keys of *brushes* collect name)
                :presentation-type 'string))

;;;; Colors

(defun nontransparent-color (ink &optional default)
  (if (eql ink +transparent-ink+) default ink))

(define-mcpixel-command (com-add-color :name t)
    ()
  (symbol-macrolet ((palette (palette *application-frame*)))
    (setf palette (append palette (list (nontransparent-color (elt palette (current-color *application-frame*)) +black+)))))
  (redisplay-palette))

(defclass colorslider (basic-gadget) ())

(defmethod compose-space ((gadget colorslider) &key &allow-other-keys)
  (make-space-requirement 
   :min-height 24 :height 24 :max-height 24
   :min-width 256 :width 256 :max-width 256))

(defclass hue-slider (colorslider) ())

(defun current-color/ink ()
  (values (current-color *application-frame*) 
          (elt (palette *application-frame*) (current-color *application-frame*))))

(defmethod handle-repaint ((gadget hue-slider) region)  
  (let ((ink (elt (palette *application-frame*) (current-color *application-frame*)))
        (y (bounding-rectangle-max-y region)))
    (cond
      ((eql ink +transparent-ink+)
       (draw-rectangle* gadget 0 0 256 y :ink (pane-background gadget)))
      (t
       (loop with step = 1 
             with height = (bounding-rectangle-max-y region)
             for hue from 0 below 256 by step
             as design = (make-ihs-color 1.5 (/ hue 255.0) 1.0)
             do (draw-rectangle* gadget hue 0 (+ hue step) height :ink design))
       (multiple-value-bind (i h s) (color-ihs ink)
         (declare (ignore i s))
         (draw-line* gadget (* h 255) 0 (* h 255) y :ink +flipping-ink+))))))

(defmethod handle-event ((gadget hue-slider) (event pointer-button-press-event))
  (modify-current-color nil (min 1.0 (max 0.0 (/ (pointer-event-x event) 255.0))) nil))

(defun modify-current-color (new-i new-h new-s)
  (multiple-value-bind (color ink) (current-color/ink)
    (unless (eql ink +transparent-ink+)
      (multiple-value-bind (i h s) (color-ihs ink)
        (setf (elt (palette *application-frame*) color)
              (make-ihs-color (or new-i i) (or new-h h) (or new-s s))))))
  (clrhash *pattern-cache*)
  (redisplay-palette)
  (repaint-editor))

;;; Applying the OAOTM principle:    
    
(defclass intensity-slider (colorslider) ())

(defparameter *intensity-scale* 4.0)

(defmethod handle-repaint ((gadget intensity-slider) region)  
  (let ((ink (elt (palette *application-frame*) (current-color *application-frame*)))
        (scale *intensity-scale*)
        (y (bounding-rectangle-max-y region)))
    (cond
      ((eql ink +transparent-ink+)
       (draw-rectangle* gadget 0 0 256 y :ink (pane-background gadget)))
      (t
       (multiple-value-bind (i h s) (color-ihs (nth-value 1 (current-color/ink)))
         (loop with step = 1
               with height = (bounding-rectangle-max-y region)
               for ins from 0 below 256 by step
               as design = (make-ihs-color (* scale (/ ins 255.0)) h s)
               do (draw-rectangle* gadget ins 0 (+ ins step) height :ink design))
         (draw-line* gadget (* i (/ scale) 255) 0 (* i (/ scale) 255) y :ink +flipping-ink+))))))

(defmethod handle-event ((gadget intensity-slider) (event pointer-button-press-event))
  (modify-current-color (max 0.0 (* *intensity-scale* (/ (pointer-event-x event) 255.0))) nil nil))


(defclass saturation-slider (colorslider) ())

(defparameter *saturation-scale* 1.0)

(defmethod handle-repaint ((gadget saturation-slider) region)  
  (let ((ink (elt (palette *application-frame*) (current-color *application-frame*)))
        (scale *saturation-scale*)
        (y (bounding-rectangle-max-y region)))
    (cond
      ((eql ink +transparent-ink+)
       (draw-rectangle* gadget 0 0 256 y :ink (pane-background gadget)))
      (t
       (multiple-value-bind (i h s) (color-ihs (nth-value 1 (current-color/ink)))
         (loop with step = 1
               with height = (bounding-rectangle-max-y region)
               for sat from 0 below 256 by step
               as design = (make-ihs-color i h (* scale (/ sat 255.0)))
               do (draw-rectangle* gadget sat 0 (+ sat step) height :ink design))
         (draw-line* gadget (* s (/ scale) 255) 0 (* s (/ scale) 255) y :ink +flipping-ink+))))))

(defmethod handle-event ((gadget saturation-slider) (event pointer-button-press-event))
  (modify-current-color nil nil (max 0.0 (* *saturation-scale* (/ (pointer-event-x event) 255.0)))))

;;;; Graphics manipulation

(define-mcpixel-command (com-flip-horizontal :name t)
    ()
  (let ((frame (curframe)))
    (when frame
      (let ((array (frame-pattern frame))
            (width (frame-width frame)))
        (dotimes (y (frame-height frame))
          (dotimes (x (floor width 2))
            (rotatef (aref array y x) (aref array y (- width 1 x))))))
      (repaint-editor))))

(define-mcpixel-command (com-flip-vertical :name t)
    ()
  (let ((frame (curframe)))
    (when frame
      (let ((array (frame-pattern frame))
            (height (frame-height frame)))
        (dotimes (x (frame-width frame))
          (dotimes (y (floor height 2))
            (rotatef (aref array y x) (aref array (- height 1 y) x)))))
      (repaint-editor))))

(define-mcpixel-command (com-fill-image :name t)
    ((color 'color))
  (let ((frame (curframe)))
    (when frame
      (let ((array (frame-pattern frame)))
        (loop for index from 0 below (array-total-size array)
              do (setf (row-major-aref array index) color)))
      (repaint-editor))))

(define-mcpixel-command (com-remap-color :name t)
    ((original-color 'color :prompt "Color to be remapped")
     (new-color 'color :prompt "Color to map to"))
  (let ((frame (curframe)))
    (when frame
      (let ((array (frame-pattern frame)))
        (loop for index from 0 below (array-total-size array)
              as color = (row-major-aref array index)
              when (eql color original-color)
              do (setf (row-major-aref array index) new-color)))
      (repaint-editor))))

(define-mcpixel-command (com-silhouette :name t)
    ((new-color 'color :prompt "Color to map to"))
  (let ((frame (curframe)))
    (when frame
      (let ((array (frame-pattern frame)))
        (loop for index from 0 below (array-total-size array)
              as color = (row-major-aref array index)
              unless (zerop color)
              do (setf (row-major-aref array index) new-color)))
      (repaint-editor))))

;;;; Animation

(defstruct anim name seq)

(define-presentation-type anim ())
(define-presentation-type akey ())

(define-presentation-method present (object (type anim) stream (view textual-view) &key)
  (format stream "[Animation ~A]" (anim-name object)))

(define-presentation-method present (object (type akey) stream (view textual-view) &key)
  (format stream "[Key ~:D x ~A]" (first object) (frame-name (second object))))

(define-mcpixel-command (com-new-animation :name t)
    ((name 'string))
  (let ((anim (make-anim :name name)))
    (push anim (anims *application-frame*))
    (setf (current-anim *application-frame*) anim))
  (redisplay-animation))

(define-mcpixel-command (com-select-animation :name t)
    ((animation 'anim))
  (when (typep animation 'anim)
    (setf (current-anim *application-frame*) animation)
    (redisplay-animation)))

(define-mcpixel-command (com-show-animations :name t)
    ()
  (dolist (anim (anims *application-frame*))
    (with-text-face (t (if (eql anim (current-anim *application-frame*)) :bold :roman))
      (with-output-as-presentation (t anim 'anim)
        (format t "~&\"~A\" (~A keys, length ~:D)~%"
                (anim-name anim)
                (length (anim-seq anim))
                (reduce #'+ (anim-seq anim) :key #'first))))))

(define-mcpixel-command (com-key :name t)
    ((frame 'frame)
     (time 'integer :prompt "duration"))
  (cond
    ((not (current-anim *application-frame*))
     (format t "~&Not editing an animation. Create one with the \"New Animation\" command.~%"))
    ((< time 1)
     (format t "~&Frame display time must be greater than zero.~%"))
    (t (symbol-macrolet ((keys (anim-seq (current-anim *application-frame*))))
         (setf keys (append keys (list (list time frame)))))
       (redisplay-animation))))

(define-mcpixel-command (com-delete-key :name t)
    ((key 'akey :prompt "Key"))
  (let ((anim (current-anim *application-frame*)))
    (cond
      ((not anim)
       (format t "~&Select an animation.~%"))
      ((find key (anim-seq anim))
       (setf (anim-seq anim) (remove key (anim-seq anim)))
       (redisplay-animation))
      (t (format t "~&Key isn't part of animation currently being edited.~%")))))

(define-mcpixel-command (com-set-duration :name t)
    ((key 'akey :prompt "Key")
     (duration 'integer :prompt "Duration"))
  (when (>= duration 0)
    (setf (first key) duration)
    (redisplay-animation)))

(defun display-sequence (frame stream)
  (when (current-anim frame)
    (surrounding-output-with-border (stream :shape :underline :move-cursor nil)
      (format stream "~A~%" (anim-name (current-anim frame))))
    (loop with time = 0
          for key in (anim-seq (current-anim *application-frame*))
          do
          (with-output-as-presentation (stream key 'akey)
            (destructuring-bind (count frame) key
              (with-text-face (stream (if (eql frame (curframe)) :bold :roman))
                (format stream "~:D: ~A (~:D)~%" time (frame-name frame) count)
                (incf time count)))))))

(define-mcpixel-command (com-set-animation-rate :name t)
    ((rate '(integer 1 60) :prompt "Rate in HZ"))
  (setf (animation-rate *application-frame*) rate))


(defclass animation-preview (basic-gadget)
  ((current-frame :accessor current-frame :initform nil)))

;;;; Return of the animation thread hack

(defclass interrupt-event (climi::standard-event)
 ((fn :initarg :fn :reader fn-of))
 (:default-initargs :sheet nil))

(defmethod handle-event (sheet (event interrupt-event))
 (funcall (fn-of event)))

(defun doublesize (array)
  (let* ((w (array-dimension array 1))
         (h (array-dimension array 0))
         (new-array (make-array (list (* h 2) (* w 2)))))
    (dotimes (i h)
      (dotimes (j w)
        (let ((x (aref array i j)))
          (setf (aref new-array (+ (* 2 i) 0) (+ (* 2 j) 0)) x
                (aref new-array (+ (* 2 i) 1) (+ (* 2 j) 0)) x
                (aref new-array (+ (* 2 i) 0) (+ (* 2 j) 1)) x
                (aref new-array (+ (* 2 i) 1) (+ (* 2 j) 1)) x))))
    new-array))

(defun frame->clim-pattern (frame)
  (make-pattern (doublesize (frame-pattern frame)) (palette *application-frame*)))

(defun cached-frame-image (frame)
  (or (gethash (copy-matrix (frame-pattern frame)) *pattern-cache*)
      (setf (gethash (frame-pattern frame) *pattern-cache*)
            (frame->clim-pattern frame))))

(defun update-for-animation ()
  (let ((pane (find-pane-named *application-frame* 'animation-preview))
        (stack (animating-stack *application-frame*)))
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
      (when (and (not stack) (current-anim *application-frame*))
        (setf (animating-stack *application-frame*)
              (map 'list #'copy-list (anim-seq (current-anim *application-frame*))))        
        (setf stack (animating-stack *application-frame*)))
      (when stack
        (let ((frame (second (first stack))))
          (when frame
            (draw-rectangle* pane x0 y0 x1 y1 :ink (pane-background pane))
            (draw-pattern* pane (cached-frame-image frame)
                           (- (floor (+ x0 x1) 2) (frame-ox frame))
                           (- (floor (+ y0 y1) 2) (frame-oy frame)))))
        (decf (first (first stack)))
        (when (< (first (first stack)) 1) (pop (animating-stack *application-frame*)))))))

(defmethod run-frame-top-level :around ((app mcpixel) &key)
 (let ((running t))
   (clim-sys:make-process
    (lambda ()
      (loop while running do
            (sleep (/ 1.0 (animation-rate app)))
            (climi::event-queue-append
             (climi::frame-event-queue app)
             (make-instance 'interrupt-event :fn 'update-for-animation)))))
   (unwind-protect (call-next-method)
     (setf running nil))))

;;;; File IO

(defun file (filename)
  (with-open-file (in filename :external-format :latin1)
    (with-standard-io-syntax ()
      (let ((*read-eval* nil))
        (read in)))))

(defsetf file (filename) (object)
  `(let ((filename ,filename)
         (object ,object))
     (with-open-file (out filename :direction :output :if-exists :supersede)
       (with-standard-io-syntax ()
         (let ((*print-circle* t))
           (pprint object out))))))

(defun serialize-palette (palette)
  (loop for color in palette
        collect (if (eql color +transparent-ink+)
                    :transparent
                    (multiple-value-list (color-rgb color)))))

(defun unserialize-palette (form)
  (loop for color in form
        collect (if (eql color :transparent)
                    +transparent-ink+
                    (apply #'make-rgb-color color))))

(defun [un]serialize (mode name object)
  (if (eql name 'palette)
      (funcall (if (eq mode :serialize) #'serialize-palette #'unserialize-palette) object)
      object))

(defparameter *state-syms* '(palette frames anims animation-rate current-color current-anim current-frame))

(defun state-form (app)
  (loop for sym in *state-syms*
        collect (list sym ([un]serialize :serialize sym (funcall (fdefinition sym) app)))))

(defun apply-state-form (app form)
  (loop for (name value) in form
        do (funcall (fdefinition `(setf ,name))  ; Unportable. Works on SBCL.
                    ([un]serialize :unserialize name value)
                    app)))

(defun save-state (app)
  (handler-case (setf (file (filename app)) (state-form app))
    (file-error (c) (princ c))))

(define-mcpixel-command (com-save-file :name t)
    ()
  (cond
    ((not (filename *application-frame*))
     (format t "Filename not yet set. Use \"Save As\".~%"))
    (t (save-state *application-frame*))))

(define-mcpixel-command (com-save-as :name t)
    ((filename 'pathname))
  (setf (filename *application-frame*) filename)
  (save-state *application-frame*))

(define-mcpixel-command (com-load :name t)
    ((filename 'pathname))
  (handler-case
      (progn 
        (apply-state-form *application-frame* (file filename))
        (setf (fr (find-editor)) (current-frame *application-frame*))
        (redisplay-animation)
        (redisplay-frames-list)
        (repaint-editor)
        (redisplay-palette))
    (file-error (c)
      (princ c))))

