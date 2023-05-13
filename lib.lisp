(defpackage :mygame/lib
  (:use :cl
        :alexandria
        :lem
        :lem-sdl2)
  (:export :game-buffer
           :all-render-sprites
           :sprite-spec
           :sprite
           :sprite-x
           :sprite-y
           :update-sprite-spec
           :register-sprite
           :remove-all-sprite))
(in-package :mygame/lib)

;; TODO: free texture
(defvar *image-cache* (make-hash-table :test 'equal))

(defclass game-buffer (text-buffer) ())

(defmethod render :before (texture window (buffer game-buffer))
  (sdl2:set-render-target (current-renderer) texture))

(defclass sprite-spec ()
  ((image-file :initarg :image-file
               :reader sprite-spec-image-file)
   (image-texture :initarg :image-texture
                  :accessor sprite-spec-image-texture)
   (source-x-index :initarg :source-x-index
                   :reader sprite-spec-source-x-index)
   (source-y-index :initarg :source-y-index
                   :reader sprite-spec-source-y-index)
   (source-width :initarg :source-width
                 :reader sprite-spec-source-width)
   (source-height :initarg :source-height
                  :reader sprite-spec-source-height)
   (dest-width :initarg :dest-width
               :reader sprite-spec-dest-width)
   (dest-height :initarg :dest-height
                :reader sprite-spec-dest-height)
   (count-animations :initform 1
                     :initarg :count-animations
                     :reader sprite-spec-count-animations)
   (animation-interval-seconds :initform 0
                               :initarg :animation-interval-seconds
                               :reader sprite-spec-animation-interval-seconds)))

(defmethod initialize-instance :after ((sprite-spec sprite-spec) &key image-file &allow-other-keys)
  (setf (sprite-spec-image-texture sprite-spec)
        (or (gethash image-file *image-cache*)
            (setf (gethash image-file *image-cache*)
                  (sdl2:create-texture-from-surface (current-renderer)
                                                    (sdl2-image:load-image image-file))))))

(defvar *all-sprites* '())

(defclass sprite ()
  ((spec :initarg :sprite-spec
         :accessor sprite-spec)
   (specs :initarg :sprite-specs
          :reader sprite-specs)
   (x :initarg :x
      :initform 0
      :accessor sprite-x)
   (y :initarg :y
      :initform 0
      :accessor sprite-y)
   (counter :initform 0
            :accessor sprite-counter)
   (timer :initform nil
          :accessor sprite-timer)))

(defmethod initialize-instance :after ((sprite sprite) &key &allow-other-keys)
  (unless (slot-boundp sprite 'spec)
    (destructuring-bind (key value &rest rest) (sprite-specs sprite)
      (declare (ignore key rest))
      (setf (sprite-spec sprite) value))))

(defun register-sprite (sprite)
  (when (and (sprite-spec-animation-interval-seconds (sprite-spec sprite))
             (< 0 (sprite-spec-animation-interval-seconds (sprite-spec sprite))))
    (let ((timer (start-timer (make-timer (lambda ()
                                            (incf (sprite-counter sprite))
                                            (redraw-display)))
                              (floor (* (sprite-spec-animation-interval-seconds (sprite-spec sprite))
                                        1000))
                              t)))
      (setf (sprite-timer sprite) timer)
      (values)))
  (push sprite *all-sprites*))

(defun stop-sprite (sprite)
  (when (sprite-timer sprite)
    (stop-timer (sprite-timer sprite))))

(defun remove-sprite (sprite)
  (stop-sprite sprite)
  (setf *all-sprites* (delete sprite *all-sprites*)))

(defun remove-all-sprite ()
  (mapc #'stop-sprite *all-sprites*)
  (setf *all-sprites* '()))

(defun render-sprite (sprite)
  (let ((spec (sprite-spec sprite)))
    (sdl2:with-rects ((src-rect (* (sprite-spec-source-width spec)
                                   (+ (sprite-spec-source-x-index spec)
                                      (mod (sprite-counter sprite)
                                           (sprite-spec-count-animations spec))))
                                (* (sprite-spec-source-height spec)
                                   (sprite-spec-source-y-index spec))
                                (sprite-spec-source-width spec)
                                (sprite-spec-source-height spec))
                      (dest-rect (sprite-x sprite)
                                 (sprite-y sprite)
                                 (sprite-spec-dest-width spec)
                                 (sprite-spec-dest-height spec)))
      (sdl2:render-copy (current-renderer)
                        (sprite-spec-image-texture spec)
                        :source-rect src-rect
                        :dest-rect dest-rect))))

(defun all-render-sprites ()
  (dolist (sprite *all-sprites*)
    (render-sprite sprite)))

(defun update-sprite-spec (sprite key)
  (setf (sprite-spec sprite) (getf (sprite-specs sprite) key)))
