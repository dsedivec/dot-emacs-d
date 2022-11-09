;; -*- lexical-binding: t; -*-

;; Temporary patch for
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48461, though I have
;; since discovered my initial report needs more contributions
;; (keyword: upstream), due to not realizing at the time that
;; `file-in-directory-p' can't be used in one or two places, now fixed
;; below.
;;
;; #48461's problem is that, if you rename a directory in dired, it
;; doesn't also change the directory of all buffers with files within
;; that directory.  Rather, dired definitely tries to do it (as you
;; can see below), but fails due to some bugs.
;;
;; UPDATE 2022-04-20: I think e71c7a7c600 tried to address some/all of
;; this.  I have a call to `file-truename' in `dired-rename-file', and
;; I don't know why I [thought I] needed that.  I'm leaving my patch
;; here for that, for now.  I also prefer my
;; `my:file-in-directory-textually-p' to `dired-in-this-tree-p', which
;; upstream used, so I'm keeping my patches for that.

(require 'el-patch)

(el-patch-feature dired-aux)

(with-eval-after-load 'dired-aux
  (el-patch-defun dired-rename-file (file newname ok-if-already-exists)
    "Rename FILE to NEWNAME.
Signal a `file-already-exists' error if a file NEWNAME already exists
unless OK-IF-ALREADY-EXISTS is non-nil."
    (let ((file-is-dir-p (file-directory-p file))
          (el-patch-add (old-truename (file-truename file))))
      (dired-handle-overwrite newname)
      (dired-maybe-create-dirs (file-name-directory newname))
      (if (and dired-vc-rename-file
               (vc-backend file)
               (ignore-errors (vc-responsible-backend newname)))
          (vc-rename-file file newname)
        ;; error is caught in -create-files
        (rename-file file newname ok-if-already-exists))
      ;; Silently rename the visited file of any buffer visiting this file.
      (and (get-file-buffer file)
           (with-current-buffer (get-file-buffer file)
             (set-visited-file-name newname nil t)))
      (dired-remove-file file)
      ;; See if it's an inserted subdir, and rename that, too.
      (when file-is-dir-p
        (dired-rename-subdir (el-patch-swap file old-truename) newname))))

  (el-patch-validate 'dired-rename-file 'defun t)

  ;; This is a version of `file-in-directory-p' which does *not*
  ;; require the named file and directory to exist.
  ;;
  ;; Note: no (el-patch-feature files) because files.el doesn't
  ;; (provide 'files).

  (el-patch-defun (el-patch-swap file-in-directory-p my:file-in-directory-textually-p) (file dir)
    "Return non-nil if DIR is a parent directory of FILE.
Value is non-nil if FILE is inside DIR or inside a subdirectory of DIR.
A directory is considered to be a \"parent\" of itself.
DIR must be an existing directory, otherwise the function returns nil."
    ;; I'm leaving the handlers in because why not.
    (let ((handler (or (find-file-name-handler file 'file-in-directory-p)
                       (find-file-name-handler dir  'file-in-directory-p))))
      (if handler
          (funcall handler 'file-in-directory-p file dir)
        (el-patch-splice 2 0
          (when (file-directory-p dir) ; DIR must exist.
            ;; `file-truename' apparently just returns its
            ;; argument unmodified if the file doesn't exist, so
            ;; no need to patch it.
            (setq file (file-truename file)
                  dir  (file-truename dir))
            (let ((ls1 (split-string file "/" t))
                  (ls2 (split-string dir  "/" t))
                  (root
                   (cond
                     ;; A UNC on Windows systems, or a "super-root" on Apollo.
                     ((string-match "\\`//" file) "//")
                     ((string-match "\\`/" file) "/")
                     (t "")))
                  (mismatch nil))
              (while (and ls1 ls2 (not mismatch))
                (if (string-equal (car ls1) (car ls2))
                    (setq root (concat root (car ls1) "/"))
                  (setq mismatch t))
                (setq ls1 (cdr ls1)
                      ls2 (cdr ls2)))
              (unless mismatch
                (el-patch-swap
                  (file-equal-p root dir)
                  (string= (file-name-as-directory root)
                           (file-name-as-directory dir))))))))))

  ;; I actually have no idea if this works for el-patch "forks".  It
  ;; at least doesn't error.
  (el-patch-validate 'my:file-in-directory-textually-p 'defun t)

  ;; Now patch `my:file-in-directory-textually-p' into these two
  ;; dired-aux functions.

  (el-patch-defun dired-rename-subdir (from-dir to-dir)
    (setq from-dir (file-name-as-directory from-dir)
          to-dir (file-name-as-directory to-dir))
    (dired-fun-in-all-buffers from-dir nil
                              #'dired-rename-subdir-1 from-dir to-dir)
    ;; Update visited file name of all affected buffers
    (let ((expanded-from-dir (expand-file-name from-dir))
          (blist (buffer-list)))
      (while blist
        (with-current-buffer (car blist)
          (if (and buffer-file-name
                   ((el-patch-swap dired-in-this-tree-p
                                   my:file-in-directory-textually-p)
                    buffer-file-name expanded-from-dir))
              (let ((modflag (buffer-modified-p))
                    (to-file (replace-regexp-in-string
                              (concat "^" (regexp-quote from-dir))
                              to-dir
                              buffer-file-name)))
                (set-visited-file-name to-file)
                (set-buffer-modified-p modflag))))
        (setq blist (cdr blist)))))

  (el-patch-validate 'dired-rename-subdir 'defun t)

  (el-patch-defun dired-rename-subdir-1 (dir to)
    ;; Rename DIR to TO in headerlines and dired-subdir-alist, if DIR or
    ;; one of its subdirectories is expanded in this buffer.
    (let ((expanded-dir (expand-file-name dir))
          (alist dired-subdir-alist)
          (elt nil))
      (while alist
        (setq elt (car alist)
              alist (cdr alist))
        (if ((el-patch-swap dired-in-this-tree-p
                            my:file-in-directory-textually-p)
             (car elt) expanded-dir)
            ;; ELT's subdir is affected by the rename
            (dired-rename-subdir-2 elt dir to)))
      (if (equal dir default-directory)
          ;; if top level directory was renamed, lots of things have to be
          ;; updated:
          (progn
            (dired-unadvertise dir)     ; we no longer dired DIR...
            (setq default-directory to
                  dired-directory (expand-file-name;; this is correct
                                   ;; with and without wildcards
                                   (file-name-nondirectory (if (stringp dired-directory)
                                                               dired-directory
                                                             (car dired-directory)))
                                   to))
            (let ((new-name (file-name-nondirectory
                             (directory-file-name (if (stringp dired-directory)
                                                      dired-directory
                                                    (car dired-directory))))))
              ;; try to rename buffer, but just leave old name if new
              ;; name would already exist (don't try appending "<%d>")
              (or (get-buffer new-name)
                  (rename-buffer new-name)))
            ;; ... we dired TO now:
            (dired-advertise)))))

  (el-patch-validate 'dired-rename-subdir-1 'defun t))
