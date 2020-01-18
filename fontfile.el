;;; fontfile.el --- Pure-Elisp font-file reader -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Will Dey

;; Author: Will Dey
;; Maintainer: Will Dey
;; Keywords: 
;; Version: 1.0.0
;; Created: November 2018
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

Pure-Elisp font-file reader

;;; Code:

;; TODO copy README.org into Commentary.

;; Tutorial:	http://stevehanov.ca/blog/index.php?id=143
;; TTF Reference:	https://developer.apple.com/fonts/TrueType-Reference-Manual/
;; SVG Reference:	https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths

(defgroup fontfile nil
  "")

(defun fontfile--read-uint8 ()
  ""
  (string-to-char (buffer-substring-no-properties (point) (progn (forward-char)
                                                                 (point)))))

(defun fontfile--read-uint16 ()
  ""
  (logior (lsh (fontfile--read-uint8) 8)
          (fontfile--read-uint8)))

(defun fontfile--read-int16 ()
  ""
  (let ((result (fontfile--read-uint16)))
    (if (/= (logand result (eval-when-compile (lsh 1 15))) 0)
        (- result (eval-when-compile (lsh 1 16)))
      result)))

;; Requires bignum support (added in Emacs 27.1)
(defun fontfile--read-uint32 ()
  ""
  (logior (lsh (fontfile--read-uint16) 16)
	  (fontfile--read-uint16)))

(defun fontfile--read-int32 ()
  ""
  (let ((result (fontfile--read-uint32)))
    (if (/= (logand result (eval-when-compile (lsh 1 31))) 0)
	(- result (eval-when-compile (lsh 1 32)))
      result)))

(defun fontfile--read-uint64 ()
  (logior (lsh (fontfile--read-uint32) 32)
	  (fontfile--read-uint32)))

(defun fontfile--read-int64 ()
  (let ((result (fontfile--read-uint64)))
    (if (/= (logand result (eval-when-compile (lsh 1 63))) 0)
	(- result (eval-when-compile (lsh 1 64)))
      result)))

(defun fontfile--read-2dot14 ()
  ""
  (/ (float (fontfile--read-int16)) (eval-when-compile (lsh 1 14))))

(defun fontfile--read-fixed ()
  (/ (float (fontfile--read-int32)) (eval-when-compile (lsh 1 16))))

(defun fontfile--read-string (length)
  ""
  (buffer-substring-no-properties (point) (progn (forward-char length)
                                                 (point))))

(defun fontfile--read-date ()
  ""
  ;; Normalize
  (encode-time (decode-time (+ (fontfile--read-int64)
			       (encode-time '(0 0 0 1 1 1904 nil nil 0) 'integer)))))

(defun fontfile--midpoint (point1 point2)
  (cons (/ (+ (car point1) (car point2))
	   2.0)
	(/ (+ (cdr point1) (cdr point2))
	   2.0)))

(defun fontfile--ttf-read-offset-tables ()
  ""
  (let* ((scalar-type    (fontfile--read-uint32))
         (num-tables     (fontfile--read-uint16))
         (search-range   (fontfile--read-uint16))
         (entry-selector (fontfile--read-uint16))
         (range-shift    (fontfile--read-uint16))
         (tables (make-hash-table :test 'equal :size num-tables))
         table tag checksum)
    (dotimes (i num-tables)
      (setq tag (fontfile--read-string 4)
            checksum (fontfile--read-uint32)
            table (cons (fontfile--read-uint32) ;; Offset.
			(fontfile--read-uint32) ;; Length.
			))
      (puthash tag table tables))
    tables))

(defun fontfile--ttf-init ()
  ""
  (let ((tables (fontfile--ttf-read-offset-tables)))
    (goto-char (1+ (car (or (gethash "head" tables)
                            (error "No font header table found"))) ;; Font header table offset.
		   ))
    (let* ((version (fontfile--read-fixed))
           (font-revision (fontfile--read-fixed))
           (checksum-adjustment (fontfile--read-uint32))
           (magic-number (fontfile--read-uint32))
           (flags (fontfile--read-uint16))
           (units-per-em (fontfile--read-uint16))
           (created-date (fontfile--read-date))
           (modified-date (fontfile--read-date))
           (x-min (fontfile--read-int16))
           (y-min (fontfile--read-int16))
           (x-max (fontfile--read-int16))
           (y-max (fontfile--read-int16))
           (mac-style (fontfile--read-uint16))
           (lowest-rec-ppem (fontfile--read-uint16))
           (font-direction-hint (fontfile--read-int16))
           (index-to-loc-format (fontfile--read-int16)))
      (unless (= magic-number #x5f0f3cf5)
	(error "Not a TrueType font file"))
      (goto-char (1+ (car (or (gethash "maxp" tables)
			      (error "No maximum profile table found"))) ;; Maximum profile table offset.
		     ))
      (let* ((version (fontfile--read-fixed))
	     (num-glyphs (fontfile--read-uint16)))
        (list 'fontfile
              :type 'ttf
              :version version
              :revision font-revision
	      :units-per-em units-per-em
	      :created created-date
	      :modified modified-date
              :tables tables
              :long-offsets (= index-to-loc-format 1)
	      :glyphs (make-vector num-glyphs nil)
	      :char-table (make-char-table nil 0))))))

(defun fontfile--ttf-find-glyph-offset (fontfile index)
  (let ((tables (plist-get (cdr fontfile) :tables)))
    (save-excursion
      (goto-char (1+ (car (or (gethash "loca" tables)
                              (error "No glyph location table found"))) ;; Location table offset.
		     ))
      (+ (if (plist-get (cdr fontfile) :long-offsets)
             (progn
               (forward-char (* index 4))
               (fontfile--read-uint32))
           (forward-char (* index 2))
           (* (fontfile--read-uint16) 2))
	 (car (or (gethash "glyf" tables)
                  (error "No glyph table found")) ;; Glyph table offset.
              )))))

(defun fontfile--ttf-read-coord-delta (flag byte-flag delta-flag)
  (if (/= (logand flag byte-flag) 0)
      (* (fontfile--read-uint8)
	 (if (/= (logand flag delta-flag) 0)
	     1
	   -1))
    (if (/= (logand flag delta-flag) 0)
        0
      (fontfile--read-int16))))

(defun fontfile--ttf--read-simple-glyph (num-contours)
  (let ((on-curve  (eval-when-compile (lsh 1 0)))
	(x-is-byte (eval-when-compile (lsh 1 1)))
	(y-is-byte (eval-when-compile (lsh 1 2)))
	(repeat    (eval-when-compile (lsh 1 3)))
	(x-delta   (eval-when-compile (lsh 1 4)))
	(y-delta   (eval-when-compile (lsh 1 5)))
        (contour-ends (make-vector num-contours 0))
        num-points
	points
	flags)
    ;; End points of contours:
    (dotimes (i num-contours)
      (aset contour-ends i (setq num-points (fontfile--read-uint16))))
    (setq num-points (1+ num-points))
    ;; Skip instructions:
    (let ((a (fontfile--read-uint16)))
      (forward-char a))
    (setq flags  (make-vector num-points 0)
	  points (make-vector num-points nil))
    ;; Flags:
    (let ((i 0))
      (while (< i num-points)
	(let ((flag (fontfile--read-uint8)))
	  (aset flags i flag)
	  (when (/= (logand flag repeat) 0)
	    (let ((repeat-count (fontfile--read-uint8)))
              (dotimes (j repeat-count)
		(aset flags (+ i j 1) flag))
	      (setq i (+ i repeat-count)))))
	(setq i (1+ i))))
    ;; `points' format: '((on-curve? . (x . y)) ...)
    (let ((x 0)
	  (y 0))
      ;; X:
      (dotimes (i num-points)
	(setq x (+ x (fontfile--ttf-read-coord-delta (aref flags i)
							x-is-byte
							x-delta)))
	(aset points i (cons (/= (logand (aref flags i) on-curve) 0)
			     (list x))))
      ;; Y:
      (dotimes (i num-points)
	(setq y (+ y (fontfile--ttf-read-coord-delta (aref flags i)
							y-is-byte
							y-delta)))
	(setf (cddr (aref points i)) y)))
    ;; SVG Path:
    (with-temp-buffer
      (let ((j 0)
	    contour-start contour-end contour-length
	    current-point
	    next-point)
        (dotimes (i num-contours)
	  (setq contour-start j
		contour-end (aref contour-ends i)
		contour-length (- contour-end contour-start -1))
	  (while (<= j contour-end)
	    (setq current-point (aref points j))
	    (if (car current-point) ;; On-curve:
		(insert (format (if (= j contour-start)
				    " M%d %d"
				  " L%d %d")
				(cadr current-point) (cddr current-point)))
	      ;; Off-curve control point:
	      (when (= j contour-start)
		(setq next-point (aref points contour-end)
		      next-point (if (car next-point) ;; Last point on-curve:
                                     (cdr next-point)
				   (fontfile--midpoint (cdr current-point) (cdr next-point))))
		(insert (format " M%d %d" (car next-point) (cdr next-point))))
              (insert (format " Q%d %d" (cadr current-point) (cddr current-point)))
	      (setq next-point (aref points (+ contour-start (% (- j contour-start -1) contour-length)))
		    next-point (if (car next-point) ;; Next point on-curve:
				   (cdr next-point)
				 (fontfile--midpoint (cdr current-point) (cdr next-point))))
	      (insert (format " %d %d" (car next-point) (cdr next-point))))
	    (setq j (1+ j)))))
      (insert " Z")
      (buffer-substring-no-properties 1 ;; Skip initial space.
				      (point-max)))))

(defun fontfile--ttf-read-simple-glyph (num-contours x-min y-min x-max y-max)
  ""
  (let ((midheight (+ y-min (/ (- y-max y-min) 2.0))))
    (format "<path d=\"%s\" transform=\"translate(0 %d)scale(1 -1)translate(0 -%d)\"/>"
            (fontfile--ttf--read-simple-glyph num-contours)
            midheight midheight)))

(defun fontfile--ttf-read-compound-glyph (fontfile)
  (let ((arg-1-and-2-are-words    (eval-when-compile (lsh 1  0)))
	(args-are-xy-values       (eval-when-compile (lsh 1  1)))
	(round-xy-to-grid         (eval-when-compile (lsh 1  2)))
	(we-have-a-scale          (eval-when-compile (lsh 1  3)))
	(more-components          (eval-when-compile (lsh 1  5)))
	(we-have-an-x-and-y-scale (eval-when-compile (lsh 1  6)))
	(we-have-a-two-by-two     (eval-when-compile (lsh 1  7)))
	(we-have-instructions     (eval-when-compile (lsh 1  8)))
	(use-my-metrics           (eval-when-compile (lsh 1  9)))
	(overlap-component        (eval-when-compile (lsh 1 10)))
        flag
	arg1 arg2
	glyph-index
	(a 1)
	(b 0)
	(c 0)
	(d 1)
	(e 0)
	(f 0)
	dst-point-index
	src-point-index
	result)
    (setq flag more-components)
    (while (/= (logand flag more-components) 0)
      (setq flag (fontfile--read-uint16)
	    glyph-index (fontfile--read-uint16))
      (if (/= (logand flag arg-1-and-2-are-words) 0)
	  (setq arg1 (fontfile--read-int16)
		arg2 (fontfile--read-int16))
	(setq arg1 (fontfile--read-uint8)
	      arg2 (fontfile--read-uint8)))
      (if (/= (logand flag args-are-xy-values) 0)
	  (setq e arg1
		f arg2)
	;; TODO
        (display-warning 'fontfile "Anchor and matching points for compound glyphs not yet supported")
	(setq dst-point-index arg1
	      src-point-index arg2))
      (cond ((/= (logand flag we-have-a-scale)          0) (setq a (fontfile--read-2dot14)
								 d a)
	     (/= (logand flag we-have-an-x-and-y-scale) 0) (setq a (fontfile--read-2dot14)
								 d (fontfile--read-2dot14))
	     (/= (logand flag we-have-a-two-by-two)     0) (setq a (fontfile--read-2dot14)
								 b (fontfile--read-2dot14)
								 c (fontfile--read-2dot14)
								 d (fontfile--read-2dot14))))
      (save-excursion
	(goto-char (1+ (fontfile--ttf-find-glyph-offset fontfile glyph-index)))
	(setq result (concat result
			     (format "<svg transform=\"matrix(%d %d %d %d %d %d)\">%s</svg>"
				     a b c d e f
				     (fontfile--ttf-read-glyph fontfile))))))
    (when (/= (logand flag we-have-instructions) 0)
      (forward-char (fontfile--read-uint16)))
    result))

(defun fontfile--ttf-read-glyph (fontfile)
  ""
  (let* ((num-contours (fontfile--read-int16))
	 (x-min (fontfile--read-int16))
	 (y-min (fontfile--read-int16))
	 (x-max (fontfile--read-int16))
	 (y-max (fontfile--read-int16))
	 (width  (- x-max x-min))
	 (height (- y-max y-min)))
    (cons (list x-min y-min width height)
	  (cond ((< num-contours 0) (fontfile--ttf-read-compound-glyph fontfile))
		((> num-contours 0) (fontfile--ttf-read-simple-glyph num-contours x-min y-min x-max y-max))))))

(defun fontfile--ttf-read-char-table-segmap4 (char-table)
  (let* ((seg-count-x2 (fontfile--read-uint16))
	 (seg-count (lsh seg-count-x2 -1))
	 (search-range (fontfile--read-uint16))
	 (entry-selector (fontfile--read-uint16))
	 (range-shift (fontfile--read-uint16))
	 (end-codes (make-vector seg-count 0))
	 (start-codes (make-vector seg-count 0))
	 (id-deltas (make-vector seg-count 0))
         glyph-indicies)
    (dotimes (i seg-count)
      (aset end-codes i (fontfile--read-uint16)))
    (fontfile--read-uint16)
    (dotimes (i seg-count)
      (aset start-codes i (fontfile--read-uint16)))
    (dotimes (i seg-count)
      (aset id-deltas i (fontfile--read-uint16)))
    (dotimes (i seg-count)
      (let ((id-range-offset (fontfile--read-uint16))
	    (j (aref start-codes i))
	    (end-code (aref end-codes i)))
	(while (<= j end-code)
	  ;; TODO
	  (set-char-table-range char-table j
				(if (= id-range-offset 0)
				    (% (+ (aref id-deltas i) j)
				       65536)
				  (save-excursion
				    (forward-char (% (+ (lsh id-range-offset -1)
							(- j (aref start-codes i))
							-1)
						     65536))
				    (fontfile--read-uint16))))
	  (setq j (1+ j)))))))

(defun fontfile--ttf-read-char-table-subtable (char-table)
  (let* ((format (fontfile--read-uint16))
	 (length (fontfile--read-uint16))
	 (language (fontfile--read-uint16)))
    (pcase format
      (4
       ;; Segment mapping to delta values:
       (fontfile--ttf-read-char-table-segmap4 char-table))
      (_
       (display-warning 'fontfile (format-message "Character map format %d is currently unsupported" format))))))

(defun fontfile--ttf-read-char-table (fontfile)
  (let* ((cmap-point (goto-char (1+ (car (or (gethash "cmap" (plist-get (cdr fontfile) :tables))
					     (error "No character to glyph table found"))) ;; Character to glyph mapping table offset.
				    )))
	 (version (fontfile--read-uint16))
	 (num-subtables (fontfile--read-uint16))
	 (subtables (make-vector num-subtables nil))
	 (char-table (make-char-table nil 0))
	 found)
    (dotimes (i num-subtables)
      (let* ((platform-id (fontfile--read-uint16))
	     (platform-specific-id (fontfile--read-uint16))
	     (offset (fontfile--read-uint32)))
	(aset subtables i (cons (cons platform-id platform-specific-id)
				offset))))
    (dolist (supported-platform-pair '((3 . 1) ;; Windows, Unicode BMP (UCS-2)
				       ))
      (dotimes (i num-subtables)
	(when (equal supported-platform-pair (car (aref subtables i)))
	  (save-excursion
	    (goto-char (+ (cdr (aref subtables i)) cmap-point))
	    (fontfile--ttf-read-char-table-subtable char-table))
	  (setq found t))))
    (if found
        char-table
      (display-warning 'fontfile "No supported character table found")
      nil)))

;;;###autoload
(defun fontfile-read-ttf (filename)
  "All glyphs"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (let* ((fontfile (fontfile--ttf-init))
	   (glyphs       (plist-get (cdr fontfile) :glyphs))
	   (warning-series t)
	   (next-glyph-point (1+ (fontfile--ttf-find-glyph-offset fontfile 0))))
      (dotimes (i (length glyphs))
	(goto-char next-glyph-point)
	(setq next-glyph-point (1+ (fontfile--ttf-find-glyph-offset fontfile (1+ i))))
	(when (> next-glyph-point (point))
	  (aset glyphs i (fontfile--ttf-read-glyph fontfile))))
      (setcdr fontfile (plist-put (cdr fontfile) :char-table (fontfile--ttf-read-char-table fontfile)))
      fontfile)))

;;;###autoload
(defun fontfile-read (filename)
  (let* ((extension (downcase (file-name-extension filename)))
	 (read-function (intern (concat "fontfile-read-" extension))))
    (unless (fboundp read-function)
      (error "Extension %s is unsupported" extension))
    (funcall read-function filename)))



(defun fontfile-find-character-index (fontfile character)
  ;; TODO
  (let ((char-table (plist-get (cdr fontfile) :char-table)))
    (if char-table
	(aref char-table character)
      0)))

(defun fontfile-get-glyph (fontfile index height color &optional ascent)
  "as SVG"
  (unless (image-type-available-p 'svg)
    (display-warning 'fontfile "Your build of Emacs does not have SVG support, which is necessary to render font glyphs"))
  (let* ((glyph (aref (plist-get (cdr fontfile) :glyphs) index))
	 (x-min           (nth 0 (car glyph)))
	 (y-min           (nth 1 (car glyph)))
	 (original-width  (nth 2 (car glyph)))
	 (original-height (nth 3 (car glyph))))
    (if glyph
	(list 'image
	      :type 'svg
	      :data (format "<svg width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\" version=\"2\"  xmlns=\"http://www.w3.org/2000/svg\" fill=\"%s\">%s</svg>"
			    (* (/ (float height) original-height) original-width) height
			    x-min y-min original-width original-height
			    color
			    (cdr glyph))
	      :ascent (or ascent
			  'center))
      (list 'space))))

(defun fontfile-get-character (fontfile character height color &optional ascent)
  (fontfile-get-glyph fontfile (fontfile-find-character-index fontfile character) height color ascent))

(defun fontfile-propertize-string (fontfile string height color &optional ascent)
  (dotimes (i (length string))
    (put-text-property i (1+ i) 'display (fontfile-get-character fontfile (aref string i) height color ascent) string))
  string)

(provide 'fontfile)
