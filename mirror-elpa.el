;;; mirror-elpa.el --- Create a local copy of Emacs Lisp Package Archives.

;; Copyright (C) 2017 Per Weijnitz

;; Author: Per Weijnitz <per.weijnitz@gmail.com>
;; Version: 0.1
;; Package-Requires: ((package "1.1"))
;; Keywords: package, mirror, elpa
;; URL: https://github.com/perweij/mirror-elpa
;; Created: 04 May 2017
;; License: GPLv3

;; This file is not part of GNU Emacs.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
     
;; This package mirrors a complete ELPA repository, for offline use.
;;
;; Configure the package archives as usual, then call
;; (package-mirror-elpa ROOT) where ROOT is the root directory
;; for your local mirror.
;;
;; An example usage:
;;     (require 'package)
;;     (mapc (lambda(p) (push p package-archives))
;;         '(("marmalade" . "http://marmalade-repo.org/packages/")
;;           ("melpa" . "http://melpa.org/packages/")
;;           ("org" ."http://orgmode.org/elpa/")))
;;     (package-mirror-elpa "~/elpa")
;;
;;
;; An example of how to run it in batch mode:
;;    emacs -Q --batch -l package.el -l ./mirror-elpa.el --eval='(progn (push (quote ("marmalade" . "http://marmalade-repo.org/packages/")) package-archives)(package-mirror-elpa "~/your-mirror-root"))'



(defun package-mirror-from-archive (root-dir pkg-desc)
  "Mirror the package pkg-desc, and store in root-dir. 
pkg-desc is a package description from `package-archive-contents'."
  (when (not (eq (package-desc-kind pkg-desc) 'dir))
    (let* ((location (package-archive-base pkg-desc))
           (file (concat (package-desc-full-name pkg-desc)
                         (package-desc-suffix pkg-desc)))
           (name (package-desc-name pkg-desc))
           (dir (expand-file-name (package-desc-archive pkg-desc) root-dir))
           (local-file (expand-file-name file dir))
           (archive-file (concat dir "/archive-contents")))
      (message (format "Mirroring %s to %s/%s" (concat location file) dir name))
      (make-directory dir t)
      ;; Download the archive index unless already done.
      (if (not (file-exists-p archive-file))
          (package-mirror-downloader archive-file (concat location "archive-contents")))
      (package-mirror-downloader local-file (concat location file)))))



(defun package-mirror-downloader (local-file url)
  "Download a file and store to disk."
  (let ((coding-system-for-read 'no-conversion)   ; to handle tar file contents
        (coding-system-for-write 'no-conversion))
    (package--with-response-buffer url
      (let* ((content (buffer-string)))
        (when (listp (read-from-string content))
          (write-region content nil local-file nil 'silent))))))



;;;###autoload
(defun package-mirror-elpa (root-dir)
  "Mirror the configured package repositories, and use root-dir
as a local root directory for the mirror."
  (interactive)
  (unless package--initialized
    (package-initialize t))
  (package-refresh-contents)
  (mapcar (lambda (elt)
            (package-mirror-from-archive root-dir (car (cdr elt))))
          package-archive-contents))



(provide 'mirror-elpa)



;;; mirror-elpa.el ends here
