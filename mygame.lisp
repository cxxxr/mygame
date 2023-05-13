(defpackage :mygame
  (:use :cl
        :alexandria
        :lem
        :lem-sdl2)
  (:export :launch))
(in-package :mygame)

(defparameter +floor+ 0)
(defparameter +wall+ 1)

(defparameter *initial-map*
  '("#############################################################################################"
    "#..@........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#...........................................................................................#"
    "#############################################################################################"))

(defun parse-map-lines (lines)
  (let (player-x
        player-y)
    (let ((map
            (loop :for line :in lines
                  :for y :from 0
                  :collect (loop :for c :across line
                                 :for x :from 0
                                 :for v := (ecase c
                                             (#\# +wall+)
                                             (#\@ (setf player-x x
                                                        player-y y)
                                                  nil)
                                             (#\. +floor+))
                                 :if v
                                 :collect v
                                 :else
                                 :collect +floor+))))
      (print (cons (length (first map))
                   (length map)))

      (values (make-array (list (length map)
                                (length (first map)))
                          :initial-contents map)
              player-x
              player-y))))

(defun map-get (map x y)
  (aref map y x))

(defun map-width (map)
  (array-dimension map 1))

(defun map-height (map)
  (array-dimension map 0))

(defclass world ()
  ((map :initarg :map
        :reader world-map)
   (player-x :initarg :player-x
             :accessor world-player-x)
   (player-y :initarg :player-y
             :accessor world-player-y)))

(defun make-world (map-lines)
  (multiple-value-bind (map player-x player-y)
      (parse-map-lines map-lines)
    (make-instance 'world
                   :map map
                   :player-x player-x
                   :player-y player-y)))

(defun can-move-player-p (world vx vy)
  (let ((x (+ (world-player-x world) vx))
        (y (+ (world-player-y world) vy)))
    (= +floor+ (map-get (world-map world) x y))))

(defun move-player (world vx vy)
  (let ((x (+ (world-player-x world) vx))
        (y (+ (world-player-y world) vy)))
    (setf (world-player-x world) x
          (world-player-y world) y)))

(defvar *world*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass camera ()
  ((start-x :initarg :start-x
            :accessor camera-start-x)
   (start-y :initarg :start-y
            :accessor camera-start-y)
   (width :initarg :width
          :accessor camera-width)
   (height :initarg :height
           :accessor camera-height)))

(defun make-player-camera (player-x player-y)
  (let ((width 30)
        (height 10))
    (make-instance 'camera
                   :start-x (- player-x (floor width))
                   :start-y (- player-y (floor height))
                   :width width
                   :height height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *player-sprite* nil)

(defclass mygame-buffer (mygame/lib:game-buffer) ())

(defmethod render (texture window (buffer mygame-buffer))
  (sdl2:set-render-draw-color (current-renderer) 0 0 0 0)
  (sdl2:render-fill-rect (current-renderer) nil)
  (mygame/lib:all-render-sprites))

(defparameter *sprite-size* 64)
(defparameter *sprite-default-speed* 3)

(defclass <basictiles-sprite-spec> (mygame/lib:sprite-spec) ()
  (:default-initargs
   :image-file (asdf:system-relative-pathname :mygame "resources/basictiles.png")
   :source-width 16
   :source-height 16
   :dest-width *sprite-size*
   :dest-height *sprite-size*
   :count-animations 1))

(defclass field-sprite-spec (<basictiles-sprite-spec>) ()
  (:default-initargs
   :source-x-index 6
   :source-y-index 1))

(defclass wall-sprite-spec (<basictiles-sprite-spec>) ()
  (:default-initargs
   :source-x-index 7
   :source-y-index 1))

(defclass <player-sprite-spec> (mygame/lib:sprite-spec) ()
  (:default-initargs
   :image-file (asdf:system-relative-pathname :mygame "resources/characters.png")
   :source-x-index 3
   :source-y-index 0
   :source-width 16
   :source-height 16
   :dest-width *sprite-size*
   :dest-height *sprite-size*
   :count-animations 3
   :animation-interval-seconds 0.5))

(defclass field-sprite (mygame/lib:sprite) ()
  (:default-initargs
   :sprite-specs (list :default (make-instance 'field-sprite-spec))))

(defclass wall-sprite (mygame/lib:sprite) ()
  (:default-initargs
   :sprite-specs (list :default (make-instance 'wall-sprite-spec))))

(defclass player-down-sprite-spec (<player-sprite-spec>) () (:default-initargs :source-y-index 0))
(defclass player-left-sprite-spec (<player-sprite-spec>) () (:default-initargs :source-y-index 1))
(defclass player-right-sprite-spec (<player-sprite-spec>) () (:default-initargs :source-y-index 2))
(defclass player-up-sprite-spec (<player-sprite-spec>) () (:default-initargs :source-y-index 3))

(defclass player (mygame/lib:sprite)
  ((speed :initform *sprite-default-speed*
          :accessor player-speed))
  (:default-initargs
   :sprite-specs (list :down (make-instance 'player-down-sprite-spec)
                       :left (make-instance 'player-left-sprite-spec)
                       :right (make-instance 'player-right-sprite-spec)
                       :up (make-instance 'player-up-sprite-spec))))

(define-major-mode mygame-mode ()
    (:name "MyGame"
     :keymap *mygame-mode-keymap*))

(define-key *mygame-mode-keymap* "Down" 'mygame-down)
(define-key *mygame-mode-keymap* "Up" 'mygame-up)
(define-key *mygame-mode-keymap* "Left" 'mygame-left)
(define-key *mygame-mode-keymap* "Right" 'mygame-right)
(define-key *mygame-mode-keymap* "q" 'mygame-quit)

(defun wait (seconds)
  (let ((start (get-internal-real-time)))
    (loop :while (< (float (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second))
                    seconds)
          :do (lem::receive-event seconds))))

(defun move-to (sprite allow-type offset-x offset-y speed)
  (check-type allow-type (member :down :up :left :right))
  (let* ((start-x (mygame/lib:sprite-x sprite))
         (start-y (mygame/lib:sprite-y sprite))
         (goal-x (+ start-x offset-x))
         (goal-y (+ start-y offset-y))
         (vx (cond ((plusp offset-x)
                    speed)
                   ((minusp offset-x)
                    (- speed))
                   (t
                    0)))
         (vy (cond ((plusp offset-y)
                    speed)
                   ((minusp offset-y)
                    (- speed))
                   (t
                    0))))
    (loop
      (incf (mygame/lib:sprite-x sprite) vx)
      (incf (mygame/lib:sprite-y sprite) vy)
      (redraw-display)
      (wait 0.005)
      (when (cond ((eq allow-type :down)
                   (< goal-y (mygame/lib:sprite-y sprite)))
                  ((eq allow-type :up)
                   (< (mygame/lib:sprite-y sprite) goal-y))
                  ((eq allow-type :left)
                   (< (mygame/lib:sprite-x sprite) goal-x))
                  ((eq allow-type :right)
                   (< goal-x (mygame/lib:sprite-x sprite))))
        (setf (mygame/lib:sprite-x sprite) goal-x)
        (setf (mygame/lib:sprite-y sprite) goal-y)
        (return)))))

(defun move-down (sprite &optional (world *world*) (speed *sprite-default-speed*))
  (mygame/lib:update-sprite-spec sprite :down)
  (when (can-move-player-p world 0 1)
    (move-player world 0 1)
    (move-to sprite :down 0 *sprite-size* speed)))

(defun move-up (sprite &optional (world *world*) (speed *sprite-default-speed*))
  (mygame/lib:update-sprite-spec sprite :up)
  (when (can-move-player-p world 0 -1)
    (move-player world 0 -1)
    (move-to sprite :up 0 (- *sprite-size*) speed)))

(defun move-left (sprite &optional (world *world*) (speed *sprite-default-speed*))
  (mygame/lib:update-sprite-spec sprite :left)
  (when (can-move-player-p world -1 0)
    (move-player world -1 0)
    (move-to sprite :left (- *sprite-size*) 0 speed)))

(defun move-right (sprite &optional (world *world*) (speed *sprite-default-speed*))
  (mygame/lib:update-sprite-spec sprite :right)
  (when (can-move-player-p world 1 0)
    (move-player world 1 0)
    (move-to sprite :right *sprite-size* 0 speed)))

(define-command mygame-down () ()
  (move-down *player-sprite*))

(define-command mygame-left () ()
  (move-left *player-sprite*))

(define-command mygame-right () ()
  (move-right *player-sprite*))

(define-command mygame-up () ()
  (move-up *player-sprite*))

(define-command mygame-launch () ()
  (let ((buffer (make-buffer "*MyGame*")))
    (change-class buffer 'mygame-buffer)
    (change-buffer-mode buffer 'mygame-mode)
    (display-buffer buffer)

    (let ((world (make-world *initial-map*)))

      (setf *world* world)

      (setf *player-sprite*
            (make-instance 'player
                           :x (* (world-player-x world) *sprite-size*)
                           :y (* (world-player-y world) *sprite-size*)))

      (mygame/lib:register-sprite *player-sprite*)

      (let ((map (world-map world)))
        (loop :for y :from 0 :below (map-height map)
              :do (loop :for x :from 0 :below (map-width map)
                        :do (let ((object (map-get map x y)))
                              (cond ((= object +wall+)
                                     (mygame/lib:register-sprite
                                      (make-instance 'wall-sprite
                                                     :x (* x *sprite-size*)
                                                     :y (* y *sprite-size*))))
                                    (t
                                     (mygame/lib:register-sprite
                                      (make-instance 'field-sprite
                                                     :x (* x *sprite-size*)
                                                     :y (* y *sprite-size*))))))))))))

(defun find-game-buffer ()
  (get-buffer "*MyGame*"))

(define-command mygame-quit () ()
  (let ((buffer (find-game-buffer)))
    (delete-buffer buffer)
    (mygame/lib:remove-all-sprite)))
