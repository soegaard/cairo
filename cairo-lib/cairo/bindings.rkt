#lang racket/base
(provide (all-defined-out)
         (all-from-out racket/draw/unsafe/cairo))

;;;
;;; Cairo Bindings
;;;

; This file contains bindings for a bunch of Cairo functions not in draw/unsafe/cairo.
; Maybe they should go there?


;;;
;;; Module Imports
;;;

(require racket/class)
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/vector
         ffi/cvector
         ffi/unsafe/cvector ; for make-cvector*
         racket/draw/unsafe/cairo
         racket/draw/unsafe/cairo-lib

         (only-in racket/draw
                  bitmap%
                  make-bitmap
                  make-platform-bitmap))

;;;
;;; From draw/unsafe/cairo
;;;

; This section contains a few definitions already present
; in draw/unsafe/cairo - but they aren't exported, so we need
; our own definitions.

(define-ffi-definer define-cairo cairo-lib
  #:provide provide-protected)

(define _cairo_surface_t/null (_cpointer/null 'cairo_surface_t))

; this struct isn't export from racket/draw/unsafe/cairo.rkt
(define-cstruct _cairo_path_t ([status _int]
                               [data _pointer]
                               [num_data _int])
  #:malloc-mode 'atomic-interior)
; otherwise we could do
;     (define-cpointer-type _cairo_path_t-pointer #:tag 'cairo_path_t)

(define-cairo cairo_path_destroy (_cfun _cairo_path_t-pointer -> _void)
  #:wrap (deallocator))

(define-cairo cairo_copy_path (_cfun _cairo_t -> _cairo_path_t-pointer)
  #:wrap (allocator cairo_path_destroy))


;; Use `_cfun' for Cairo functions and `_cbfun' for callbacks:
(define-syntax-rule (_cfun . rest)
  (_fun . rest))
(define-syntax-rule (_cbfun . rest)
  (_fun #:atomic? #t . rest))

;;;
;;; Cairo
;;;

;;; This is an attempt to make bindings for Cairo that matches the C API directly.

;;;
;;; VERSION INFORMATION
;;;

; int cairo_version (void);
; https://www.cairographics.org/manual/cairo-Version-Information.html#cairo-version

(define-cairo cairo_version (_cfun -> _int))

(provide cairo-version)
(define (cairo-version)
  (define v (cairo_version))
  (define major             (quotient v 10000))
  (define minor  (remainder (quotient v 100)   100))
  (define micro  (remainder           v 100))
  (list major minor micro))

; https://www.cairographics.org/manual/cairo-Version-Information.html#cairo-version-string
(define-cairo cairo_version_string (_cfun -> _string))
(define cairo-version-string cairo_version_string)


;;;
;;; Context
;;;

;; typedef enum _cairo_content {
;;     CAIRO_CONTENT_COLOR		= 0x1000,
;;     CAIRO_CONTENT_ALPHA		= 0x2000,
;;     CAIRO_CONTENT_COLOR_ALPHA	= 0x3000
;; } cairo_content_t;
(define _cairo_content_t _int)

; https://www.cairographics.org/manual/cairo-Error-handling.html#cairo-status-t
; values for enums: https://gitlab.freedesktop.org/cairo/cairo/-/blob/master/src/cairo.h#L314
; TODO: add more status error codes to racket/draw
(define-cairo cairo_status                  (_cfun _cairo_t                   -> _int))
(define-cairo cairo_status_to_string        (_cfun _int                       -> _string))
(define-cairo cairo_push_group              (_cfun _cairo_t                   -> _void))
(define-cairo cairo_pop_group               (_cfun _cairo_t                   -> _void))
(define-cairo cairo_pop_group_to_source     (_cfun _cairo_t                   -> _void))
(define-cairo cairo_push_group_with_content (_cfun _cairo_t _cairo_content_t  -> _void))
(define-cairo cairo_get_group_target        (_cfun _cairo_t                   -> _cairo_surface_t/null))
(define-cairo cairo_get_antialias           (_cfun _cairo_t                   -> _int))
(define-cairo cairo_get_dash_count          (_cfun _cairo_t                   -> _int))
(define-cairo cairo_get_fill_rule           (_cfun _cairo_t                   -> _int))
(define-cairo cairo_get_line_cap            (_cfun _cairo_t                   -> _int))
(define-cairo cairo_get_line_join           (_cfun _cairo_t                   -> _int))
(define-cairo cairo_get_line_width          (_cfun _cairo_t                   -> _double))
(define-cairo cairo_set_miter_limit         (_cfun _cairo_t  _double*         -> _void))
(define-cairo cairo_get_miter_limit         (_cfun _cairo_t                   -> _double))
(define-cairo cairo_set_tolerance           (_cfun _cairo_t  _double*         -> _void))
(define-cairo cairo_get_tolerance           (_cfun _cairo_t                   -> _double))
(define-cairo cairo_clip_preserve           (_cfun _cairo_t                   -> _void))
(define-cairo cairo_in_clip                 (_cfun _cairo_t _double* _double* -> _bool))
(define-cairo cairo_in_stroke               (_cfun _cairo_t _double* _double* -> _bool))
(define-cairo cairo_copy_page               (_cfun _cairo_t                   -> _void))
;; Paths

(define-cairo cairo_copy_path_flat          (_cfun _cairo_t -> _cairo_path_t-pointer)
  #:wrap (allocator cairo_path_destroy))
(define-cairo cairo_append_path             (_cfun _cairo_t _cairo_path_t-pointer -> _void))

(define-cairo cairo_has_current_point       (_cfun _cairo_t                       -> _bool))
(define-cairo cairo_get_current_point       (_cfun _cairo_t
                                                   (x : (_ptr o _double)) 
                                                   (y : (_ptr o _double))
                                                   -> _void
                                                   -> (values x y)))
(define-cairo cairo_new_sub_path            (_cfun _cairo_t                       -> _void))
(define-cairo cairo_rel_curve_to            (_cfun _cairo_t _double* _double* _double* _double* _double* _double*
                                                   -> _void))


; 1.4 and later:
(define-cairo cairo_pattern_get_color_stop_count (_cfun _cairo_pattern_t (_ptr o _int)  -> _int)
  #:make-fail make-not-available)
; 1.4 and later:
(define-cairo cairo_pattern_get_color_stop_rgba
  (_cfun _cairo_pattern_t _int  ; pattern and index
         (offset : (_ptr o _double*))
         (red : (_ptr o _double*))  (green : (_ptr o _double*))  (blue : (_ptr o _double*)) (alpha : (_ptr o _double*))
         -> (status : _int)
         -> (values status offset red green blue alpha))
  #:make-fail make-not-available)

;; void
;; cairo_pattern_add_color_stop_rgb (cairo_pattern_t *pattern,
;;                                   double offset,
;;                                   double red,
;;                                   double green,
;;                                   double blue);

; 1.4 and later:
(define-cairo cairo_pattern_create_rgb (_cfun _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
; 1.4 and later:
(define-cairo cairo_pattern_create_rgba (_cfun _double* _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
; 1.4 and later:
(define-cairo cairo_pattern_get_rgba (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*)
                                            (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available) ;; not an allocator
; 1.4 and later:
(define-cairo cairo_pattern_get_surface (_cfun _cairo_pattern_t (_ptr o _cairo_surface_t) -> _int)
  #:make-fail make-not-available) ;; not an allocator
; 1.4 and later:
(define-cairo cairo_pattern_get_linear_points
  (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available)
(define-cairo cairo_pattern_create_radial
  (_cfun _double* _double* _double* _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
; 1.4 and later:
(define-cairo cairo_pattern_get_radial_circles
  (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*) (_ptr o _double*)
         (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available)

;; (define-cairo cairo_pattern_get_color_stop_count (_cfun _cairo_pattern_t 

;; cairo_status_t
;; cairo_pattern_get_color_stop_count (cairo_pattern_t *pattern, int *count);

;;; Matrix

(define-cpointer-type _cairo_matrix_t-pointer #:tag 'cairo_matrix_t)

(define-cairo cairo_matrix_init_identity      (_cfun _cairo_matrix_t-pointer                   -> _void))
(define-cairo cairo_matrix_init_scale         (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_init_rotate        (_cfun _cairo_matrix_t-pointer _double*          -> _void))
(define-cairo cairo_matrix_invert             (_cfun _cairo_matrix_t-pointer                   -> _int))
(define-cairo cairo_matrix_transform_distance (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_transform_point    (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))

(define-cairo cairo_user_to_device
  (_cfun _cairo_t (x : (_ptr io _double*)) (y : (_ptr io _double*)) -> _void -> (values x y)))
(define-cairo cairo_user_to_device_distance
  (_cfun _cairo_t (x : (_ptr io _double*)) (y : (_ptr io _double*)) -> _void -> (values x y)))
(define-cairo cairo_device_to_user
  (_cfun _cairo_t (x : (_ptr io _double*)) (y : (_ptr io _double*)) -> _void -> (values x y)))
(define-cairo cairo_device_to_user_distance
  (_cfun _cairo_t (x : (_ptr io _double*)) (y : (_ptr io _double*)) -> _void -> (values x y)))

;;; Rectangle

(define-cstruct _cairo_rectangle_int_t ([x      _int]
                                        [y      _int]
                                        [width  _int]
                                        [height _int])
  #:malloc-mode 'atomic-interior)


;; (define-cstruct _cairo_rectangle_list_t ([status _int]
;;                                          [rectangles _cairo_rectangle_t-pointer/null]
;;                                          [num_rectangles _int]))
(provide (struct-out cairo_rectangle_int_t) _cairo_rectangle_int_t
         #;(struct-out cairo_rectangle_int_list_t))


;;; Region

; Note: The fields of the region struct are private, so we don't use a define-cstruct here.
(define-cpointer-type _cairo_region_t-pointer #:tag 'cairo_region_t)


(define-cairo cairo_region_destroy             (_cfun _cairo_region_t-pointer             -> _void)
  #:wrap (deallocator))

(define-cairo cairo_region_create              (_cfun                                     -> _cairo_region_t-pointer)
  #:wrap (allocator cairo_region_destroy))
(define-cairo cairo_region_copy                (_cfun _cairo_region_t-pointer             -> _cairo_region_t-pointer)
  #:wrap (allocator cairo_region_destroy))
(define-cairo cairo_region_create_rectangle    (_cfun _cairo_rectangle_int_t-pointer      -> _cairo_region_t-pointer)
  #:wrap (allocator cairo_region_destroy))
(define-cairo cairo_region_create_rectangles   (_cfun _cairo_rectangle_int_t-pointer _int -> _cairo_region_t-pointer)
  #:wrap (allocator cairo_region_destroy))


(define-cairo cairo_region_status              (_cfun _cairo_region_t-pointer             -> _int))
(define-cairo cairo_region_get_extents         (_cfun _cairo_region_t-pointer
                                                      (extents : (_ptr o  _cairo_rectangle_int_t))
                                                      -> _void -> extents))
(define-cairo cairo_region_num_rectangles      (_cfun _cairo_region_t-pointer            -> _int))

(define-cairo cairo_region_get_rectangle       (_cfun _cairo_region_t-pointer _int
                                                      (rectangle : (_ptr o _cairo_rectangle_int_t))
                                                      -> _void -> rectangle))
(define-cairo cairo_region_is_empty            (_cfun _cairo_region_t-pointer                                 -> _bool))
(define-cairo cairo_region_contains_point      (_cfun _cairo_region_t-pointer _int _int                       -> _bool))
(define-cairo cairo_region_contains_rectangle  (_cfun _cairo_region_t-pointer _cairo_rectangle_int_t-pointer  -> _int))
(define-cairo cairo_region_equal               (_cfun _cairo_region_t-pointer _cairo_region_t-pointer         -> _bool))
(define-cairo cairo_region_translate           (_cfun _cairo_region_t-pointer _int _int                       -> _void))
(define-cairo cairo_region_intersect           (_cfun _cairo_region_t-pointer _cairo_region_t-pointer         -> _int))
(define-cairo cairo_region_intersect_rectangle (_cfun _cairo_region_t-pointer _cairo_rectangle_int_t-pointer  -> _int))
(define-cairo cairo_region_subtract            (_cfun _cairo_region_t-pointer _cairo_region_t-pointer         -> _int))
(define-cairo cairo_region_subtract_rectangle  (_cfun _cairo_region_t-pointer _cairo_rectangle_int_t-pointer  -> _int))
(define-cairo cairo_region_union               (_cfun _cairo_region_t-pointer _cairo_region_t-pointer         -> _int))
(define-cairo cairo_region_union_rectangle     (_cfun _cairo_region_t-pointer _cairo_rectangle_int_t-pointer  -> _int))
(define-cairo cairo_region_xor                 (_cfun _cairo_region_t-pointer _cairo_region_t-pointer         -> _int))
(define-cairo cairo_region_xor_rectangle       (_cfun _cairo_region_t-pointer _cairo_rectangle_int_t-pointer  -> _int))


(define-cairo cairo_pattern_create_mesh        (_cfun                                    -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))

(define-cairo cairo_mesh_pattern_begin_patch   (_cfun _cairo_pattern_t                   -> _void))
(define-cairo cairo_mesh_pattern_end_patch     (_cfun _cairo_pattern_t                   -> _void))
(define-cairo cairo_mesh_pattern_move_to       (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_line_to       (_cfun _cairo_pattern_t _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_curve_to      (_cfun _cairo_pattern_t
                                                      _double* _double*
                                                      _double* _double*
                                                      _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_control_point (_cfun _cairo_pattern_t _uint _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_corner_color_rgb  (_cfun _cairo_pattern_t _uint _double* _double* _double* -> _void))
(define-cairo cairo_mesh_pattern_set_corner_color_rgba  (_cfun _cairo_pattern_t _uint _double* _double* _double* _double*
                                                               -> _void))
(define-cairo cairo_mesh_pattern_get_patch_count  (_cfun _cairo_pattern_t
                                                         (count : (_ptr o _uint))
                                                         -> (status : _int)
                                                         -> (values status count)))
(define-cairo cairo_mesh_pattern_get_path  (_cfun _cairo_pattern_t _uint -> _cairo_path_t-pointer))
(define-cairo cairo_mesh_pattern_get_control_point
  (_cfun _cairo_pattern_t _uint _uint (x : (_ptr o _double)) (y : (_ptr o _double))
         -> (status : _int)
         -> (values status x y)))
(define-cairo cairo_mesh_pattern_get_corner_color_rgba
  (_cfun _cairo_pattern_t _uint _uint
         (r : (_ptr o _double))
         (g : (_ptr o _double))
         (b : (_ptr o _double))
         (a : (_ptr o _double))
         -> (status : _int)
         -> (values status r g b a)))


;;;

(define-cpointer-type _cairo_surface_t-pointer #:tag 'cairo_surface_t)

(define-cairo cairo_surface_mark_dirty_rectangle (_cfun _cairo_surface_t-pointer _int _int _int _int -> _void))

;;; Image Surfaces

(define-cairo cairo_format_stride_for_width       (_cfun _int _int                    -> _int))
(define-cairo cairo_image_surface_create_for_data (_cfun _pointer _int _int _int _int -> _cairo_surface_t)
  #:wrap (allocator cairo_surface_destroy))

;;; Recording Surfaces

(define-cairo cairo_recording_surface_ink_extents (_cfun _cairo_surface_t-pointer
                                                         (x : (_ptr o _double*))
                                                         (y : (_ptr o _double*))
                                                         (w : (_ptr o _double*))
                                                         (h : (_ptr o _double*))
                                                         -> _void
                                                         -> (values x y w h)))

(define-cpointer-type _cairo_rectangle_t-pointer #:tag 'cairo_rectangle_t)

(define-cairo cairo_recording_surface_get_extents (_cfun _cairo_surface_t-pointer
                                                         _cairo_rectangle_t-pointer
                                                         -> (out : _bool)
                                                         -> out))


(define-cairo cairo_surface_create_similar_image (_cfun _cairo_surface_t _int _int _int -> _cairo_surface_t))


;;; Cairo Devices

(define-cpointer-type _cairo_device_t-pointer #:tag 'cairo_device_t)

;;; Script Surface

(define-cairo cairo_script_create                 (_cfun _string*/utf-8                          -> _cairo_device_t-pointer))
(define-cairo cairo_script_from_recording_surface (_cfun _cairo_device_t-pointer _cairo_surface_t -> _int))
(define-cairo cairo_script_get_mode               (_cfun _cairo_device_t-pointer                  -> _int))
(define-cairo cairo_script_set_mode               (_cfun _cairo_device_t-pointer _int             -> _void))
(define-cairo cairo_script_surface_create         (_cfun _cairo_device_t-pointer _int _double _double -> _cairo_surface_t))
(define-cairo cairo_script_surface_create_for_target (_cfun _cairo_device_t-pointer _cairo_surface_t-pointer
                                                            -> _cairo_surface_t))
(define-cairo cairo_script_write_comment           (_cfun _cairo_device_t-pointer _string/utf-8 _int -> _void))
