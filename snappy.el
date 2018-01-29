(defun snappy-open-blob (file)
  (let ((buf (generate-new-buffer " *snappy*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (if file (insert-file-contents-literally file))
      (goto-char (point-min)))
    buf))

(defun snappy-close-blob (blob)
  (if blob (kill-buffer blob)))

(defun snappy-read-varint (blob)
  (with-current-buffer blob
    (let (vari curc (go t) (value 0) shift)
      (while go
        (setq curc   (char-after)
              vari   (cons (logand curc #b01111111) vari)
              go     (>    (logand curc #b10000000) 0   ))
        (forward-char))
      (while (> (length vari) 0)
        (setq shift (* (1- (length vari)) 7)
              value (logior value (lsh (car vari) shift))
              vari  (cdr vari)))
      value)))

(defun snappy-read-le-int (blob width)
  (let ((int 0) (nth 0) curc)
    (with-current-buffer blob
      (while (< nth width)
        (setq curc (char-after)
              int  (logior int (lsh curc (* nth 8)))
              nth  (1+ nth))
        (forward-char)))
    int))

(defun snappy-read-literal (blob tag)
  (let ((length (lsh (logand #b11111100 tag) -2)) bytes start)
    (with-current-buffer blob
      (setq length (1+ (if (<= length 59)
                           length
                         (snappy-read-le-int blob (- length 59))))
            start  (point)
            bytes  (buffer-substring start (+ start length)))
      (forward-char length))
      bytes))

(defun snappy-read-cspec-1 (blob tag)
  (let (len off b1)
    (with-current-buffer blob
      (setq len (+ (lsh (logand #b00011100 tag) -2) 4)
            off (lsh (logand #b11100000 tag) 3)
            b1  (logand #b11111111 (char-after))
            off (logand #b11111111111 (logior off b1)))
      (forward-char))
    (cons off len)))

(defun snappy-read-cspec-2 (blob tag)
  (let (len off)
    (with-current-buffer blob
      (setq len (+ (lsh (logand #b11111100 tag) -2) 1)
            off (snappy-read-le-int blob 2)))
    (cons off len)))

(defun snappy-read-cspec-4 (blob tag)
  (let (len off)
    (with-current-buffer blob
      (setq len (+ (lsh (logand #b11111100 tag) -2) 1)
            off (snappy-read-le-int blob 4)))
    (cons off len)))

(defun snappy-debug-tag (pos type byte)
  (message "snappy tag at %d is %s (%d)"
           pos (cond ((eq type #b00) "literal")
                     ((eq type #b01) "cspec-1")
                     ((eq type #b10) "cspec-1")
                     ((eq type #b11) "cspec-1")) byte))

(defun snappy-read-tag (blob)
  (let (tag-byte tag-type pos)
    (with-current-buffer blob
      (setq pos      (point)
            tag-byte (char-after)
            tag-type (logand #b011 tag-byte))
      (forward-char))
    ;;(snappy-debug-tag pos tag-type tag-byte)
    (cond ((eq tag-type #b00) (snappy-read-literal blob tag-byte))
          ((eq tag-type #b01) (snappy-read-cspec-1 blob tag-byte))
          ((eq tag-type #b10) (snappy-read-cspec-2 blob tag-byte))
          ((eq tag-type #b11) (snappy-read-cspec-4 blob tag-byte))) ))

(defun snappy-make-varint (value)
  (let (bytes bits lsb)
    (while (> value 0)
      (setq bits  (logand value #b01111111)
            value (lsh    value -7)
            bytes (cons bits bytes)))
    (if (not bytes)
        (string ?\0)
      (setq lsb   (car bytes)
            bytes (cdr bytes)
            bytes (cons lsb (mapcar (lambda (b) (logior b #b10000000)) bytes)))
      (concat (nreverse bytes))) ))

(defun snappy-rle-pad (str pad-len)
  (let ((base-len (length str)) target-len)
    (setq target-len (+ base-len pad-len))
    (while (< (length str) target-len)
      (setq str (concat str (substring str 0 (min base-len pad-len)))) )
    (substring str 0 target-len)))

(defun snappy-read-offset (inflate-marker offset-data)
  (let (rval len offs unpacked i-pos from to)
    (setq unpacked (marker-buffer   inflate-marker)
          i-pos    (marker-position inflate-marker) ;; 1-indexed
          offs     (or (car-safe offset-data) 0)
          len      (or (cdr-safe offset-data) 0)
          from     (- i-pos offs)
          rle-len  (if (< offs len) (- len offs) 0))
    (with-current-buffer unpacked
      (if (not (< 0 from i-pos))
          (setq rval (format "offset not valid at stream position %S" i-pos)
                rval (cons rval offset-data))
        (setq to (+ from len))
        (if (zerop rle-len)
            (setq rval (buffer-substring from to))
          (setq rval (buffer-substring from i-pos)
                rval (snappy-rle-pad rval rle-len)) )))
    rval))

(defun snappy-unpack (packed)
  (cond ((stringp packed)
         (setq packed (snappy-open-blob packed)))
        ((buffer-live-p packed) t)
        (t (error "snappy-unpack: argument must be string or live buffer")))
  (let ((unpacked (generate-new-buffer " *snappy-inflated*"))
        (size     0)
        (start    0)
        (i-data   nil)
        (inflated nil))
    ;; set up the inflate buffer
    (set-buffer unpacked)
    (set-buffer-multibyte nil)
    (setq inflated (set-marker (make-marker) 1))
    ;; switch back to the deflated buffer
    (set-buffer packed)
    (goto-char (point-min))
    (set-buffer-multibyte nil)
    (while (not (eobp))
      ;; unpacked size of the current snappy frame
      ;; and the position of the inflation marker
      (setq size  (snappy-read-varint packed)
            start (marker-position  inflated))
      (while (and (< (- (marker-position inflated) start) size)
                  (not (eobp)))
        (setq i-data (snappy-read-tag packed))
        (if (or (stringp i-data)
                (stringp (setq i-data (snappy-read-offset inflated i-data))))
            (princ i-data inflated)
          (error "Malformed data: decompression returned %S" i-data))
        ))
    unpacked))

;;(let ((payload (snappy-open-blob "/tmp/data-19")))
;;  (message "loaded blob %S" payload)
;;  (message "size of uncompressed %S is %S"
;;           payload (snappy-read-varint payload))
;;  (snappy-close-blob payload))

(provide 'snappy)
