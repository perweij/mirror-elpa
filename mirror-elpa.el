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
;; (mirror-elpa ROOT) where ROOT is the root directory
;; for your local mirror.
;;
;; An example usage:
;;     (require 'package)
;;     (mapc (lambda(p) (push p package-archives))
;;         '(("marmalade" . "http://marmalade-repo.org/packages/")
;;           ("melpa" . "http://melpa.org/packages/")
;;           ("org" ."http://orgmode.org/elpa/")))
;;     (mirror-elpa "~/elpa")
;;
;;
;; An example of how to run it in batch mode:
;;    emacs -Q --batch -l package.el -l ./mirror-elpa.el --eval='(progn (push (quote ("marmalade" . "http://marmalade-repo.org/packages/")) package-archives)(mirror-elpa "~/your-mirror-root"))'



(defun mirror-elpa--get-pkg-from-archive (root-dir pkg-desc)
  "Mirror the package pkg-desc, and store in root-dir. 
pkg-desc is a package description from `package-archive-contents'."
  (when (not (eq (package-desc-kind pkg-desc) 'dir))
    (let* ((location (mirror-elpa--add-slash (package-archive-base pkg-desc)))
           (file (concat (package-desc-full-name pkg-desc)
                         (package-desc-suffix pkg-desc)))
           (name (package-desc-name pkg-desc))
           (dir (expand-file-name (package-desc-archive pkg-desc) root-dir))
           (local-file (expand-file-name file dir))
           (archive-file (concat dir "/archive-contents")))
      ;; Download the archive index unless already done.
      (if (not (file-exists-p archive-file))
          (mirror-elpa--downloader archive-file (concat location "archive-contents")))
      (when (not (file-exists-p local-file))
        (make-directory dir t)
        (message (format "Mirroring %s to %s" (concat location file) local-file))
        (mirror-elpa--downloader local-file (concat location file))
        (sit-for 1))))) ; don't DOS-attack the repo!



(defun mirror-elpa--downloader (local-file url)
  "Download a file and store to disk."
  (let ((coding-system-for-read 'no-conversion)   ; to handle tar file contents
        (coding-system-for-write 'no-conversion))
    (package--with-response-buffer url
      (let* ((content (buffer-string)))
        (when (listp (read-from-string content))
          (write-region content nil local-file nil 'silent))))))



(defun mirror-elpa--add-slash (str)
  (if (string-suffix-p "/" str)
      str
      (concat str "/")))



;;;###autoload
(defun mirror-elpa (root-dir)
  "Mirror the configured package repositories, and use root-dir
as a local root directory for the mirror."
  (interactive)
  (unless package--initialized
    (package-initialize t))
  (package-refresh-contents)
  (mapcar (lambda (elt)
            (mirror-elpa--get-pkg-from-archive root-dir (car (cdr elt))))
          package-archive-contents))



(provide 'mirror-elpa)



;;; mirror-elpa.el ends here
