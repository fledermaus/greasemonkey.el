(require 'json)
(require 'sql)
(require 'snappy)

(defvar gm-mozilla-directory "~/.mozilla")
(defvar gm-mozilla-component "firefox")
(defvar gm-mozilla-profiles  nil)

(defun gm-jump-to-next-profile-i ()
  (while (and (not (looking-at "^\\[\\(.*?\\)\\]")) (< (forward-line) 1)))
  (if (eobp) nil (match-string 1)))

(defun gm-mozilla-profiles (&optional refresh)
  (if refresh (setq gm-mozilla-profiles nil))
  (or gm-mozilla-profiles
      (let ((topd (expand-file-name gm-mozilla-component gm-mozilla-directory))
            profile-name profile-path profile-label go)
        (with-temp-buffer
          (insert-file-contents (expand-file-name "profiles.ini" topd))
          (goto-char (point-min))
          (while (setq profile-label (gm-jump-to-next-profile-i))
            (forward-line)
            (if (string-equal profile-label "General")
                nil
              (setq go t profile-name nil profile-path nil)
              (while (and go
                          (looking-at "^\\(.*?\\)\\s-*=\\s-*\\(.*?\\)\\s-*$"))
                (if (string-equal (match-string 1) "Path")
                    (setq profile-path (match-string 2)))
                (if (string-equal (match-string 1) "Name")
                    (setq profile-name (match-string 2)))
                (if (and profile-name profile-path)
                    (setq go nil
                          gm-mozilla-profiles
                          (cons (cons profile-name profile-path)
                                gm-mozilla-profiles))
                  (setq go (< (forward-line) 1)))) )) ))
      (setq gm-mozilla-profiles (nreverse gm-mozilla-profiles))
      gm-mozilla-profiles))

(defun gm-get-profile-dir (&optional profile)
  (let (pf-data pf-path moz-path)
    (setq pf-data (cond
                   ((integerp profile) (nth profile gm-mozilla-profiles))
                   ((stringp  profile) (assoc "default" gm-mozilla-profiles))
                   (t (nth 0 gm-mozilla-profiles)))
          pf-path (cdr pf-data))
    (when pf-path
      (setq moz-path
            (expand-file-name gm-mozilla-component gm-mozilla-directory)
            pf-path
            (expand-file-name pf-path moz-path)))
    pf-path))

(defun gm-get-extensions (&optional profile)
  (gm-mozilla-profiles)
  (let ((pf-path (gm-get-profile-dir profile)))
    (if pf-path
        (with-temp-buffer
          (insert-file-contents (expand-file-name "extensions.json" pf-path))
          (goto-char (point-min))
          (json-read)))))

(defun gm-get-extension-data (&optional extension profile)
  (or extension (setq extension "Greasemonkey"))
  (let ((edata (gm-get-extensions profile)) e id )
    (setq edata (cdr (assq 'addons edata)))
    (mapc (lambda (x &optional dl name)
            (if (equal (assq 'name (cdr (assq 'defaultLocale x)))
                       `(name . ,extension))
                (setq e x)))
          edata)
    e))

(defun gm-get-script-storage (&optional profile)
  (let ((extension (gm-get-extension-data nil profile))
        db-file id edir match)
    (setq id    (cdr (assq 'id extension))
          edir  (gm-get-profile-dir profile)
          edir  (expand-file-name "storage/default" edir)
          match (concat "^"  (regexp-quote "moz-extension+++") ".*"
                        "="  (url-hexify-string id) "$"))
    (apply 'nconc
           (mapcar
            (lambda (d) (directory-files (concat d "/idb") :full "\\.*.sqlite"))
            (directory-files edir :full match :no-sort))) ))

(defun gm-copy-sqlite-storage (dest)
  (copy-file (car (gm-get-script-storage)) dest))

(defconst gm-extract-all-sql
  "select object_store_id, key, hex(data) as \"hex-data\" from object_data")

(defvar gm-hexchar-map nil)

(defun gm-hexchar-map-lookup (c)
  (when (not gm-hexchar-map)
    (setq gm-hexchar-map (make-vector ?G -1))
    (let (x)
      (setq x ?0)
      (while (<= x ?9)
        (aset gm-hexchar-map x (- x ?0))
        (setq x (1+ x)))
      (setq x ?A)
      (while (<= x ?F)
        (aset gm-hexchar-map x (+ 10 (- x ?A)))
        (setq x (1+ x)))))
  (aref gm-hexchar-map c))

(defun gm-sync-storage-write-blob (staging id key hex-data)
  (let ((offs 0)
        (max (length hex-data))
        blob-dir key-dir data-file written unpacked)
    (setq blob-dir  (expand-file-name id     staging )
          key-dir   (expand-file-name key    blob-dir)
          data-file (expand-file-name "data" key-dir )
          unpacked  (expand-file-name "blob" key-dir ))
    (mkdir key-dir t)
    (with-temp-file data-file
      (let ((coding-system-for-write 'no-conversion) o0 o1)
        (set-buffer-multibyte nil)
        (while (< offs max)
          (setq o0 (gm-hexchar-map-lookup (aref hex-data offs))
                o1 (gm-hexchar-map-lookup (aref hex-data (1+ offs))))
          (if (and (>= o0 0) (>= o1 0))
              (insert (+ (* o0 16) o1))
            (message "Invalid hex number %s at offset %d"
                     (substring hex-data offs (1+ offs)) offs)
            (insert ?0))
          (setq offs (+ offs 2)))
        (setq written (point-max)))
      (with-current-buffer (snappy-unpack (current-buffer))
        (write-region (point-min) (point-max) unpacked)))
    written))

(defun gm-sync-storage-to-staging-internal (staging buf)
  (mkdir staging t)
  (let (id key data k v)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\s-*\\(\\S-*\\)\\s-*=\\s-*\\(.*\\)")
          (setq k (match-string 1)
                v (match-string 2))
          (cond ((equal "object_store_id" k) (setq id   v))
                ((equal "key"             k) (setq key  v))
                ((equal "hex-data"        k) (setq data v)))
          (when (and id key data)
            (gm-sync-storage-write-blob staging id key data)
            (setq id nil key nil data nil)))
        (forward-line)) )))

(defun gm-sync-storage-to-staging (&optional staging profile)
  (let ((storage (car (gm-get-script-storage)))
        (std-out (generate-new-buffer "gm-sqlite-out"))
        (std-err (generate-new-buffer "gm-sqlite-err"))
        (cmd     (format "echo '%s' | %s -line -batch"
                         gm-extract-all-sql sql-sqlite-program))
        err-str
        rval)
    ;;(setq storage "/tmp/dummygm.sqlite") ;; switch to debug target
    (setq staging (expand-file-name (or staging "~/.emacs.d/greasemonkey"))
          rval    (shell-command (concat cmd " " storage) std-out std-err)
          err-str (with-current-buffer std-err (buffer-string)))
    (if (and (eq rval 0) (eq 0 (length err-str)))
        (setq rval (gm-sync-storage-to-staging-internal staging std-out))
      (message "Error while extracting greasemonkey scripts:\n%s:\n"
               cmd
               (if (eq (length err-str) 0) (format "Exit %d" rval) err-str))
      (setq rval nil))
    (kill-buffer std-out)
    (kill-buffer std-err)
    rval))

(provide 'greasemonkey)
