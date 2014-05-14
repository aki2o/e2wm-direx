;;; e2wm-direx.el --- Plugin to direx.el for e2wm.el

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: tools, window manager, convenience
;; URL: https://github.com/aki2o/e2wm-direx
;; Version: 0.0.1
;; Package-Requires: ((e2wm "1.2") (direx "0.1alpha"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; see <https://github.com/aki2o/e2wm-direx/blob/master/README.md>

;;; Dependency:
;; 
;; - e2wm.el ( see <https://github.com/kiwanami/emacs-window-manager> )
;; - direx.el ( see <https://github.com/m2ym/direx-el> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'e2wm-direx)

;;; Configuration:
;; 
;; (setq e2wm:c-code-recipe
;;       '(| (:left-max-size 40)
;;           (- (:upper-size-ratio 0.6)
;;              tree history)
;;           (- (:lower-max-size 150)
;;              (| (:right-max-size 40)
;;                 main imenu)
;;              sub)))
;; 
;; (setq e2wm:c-code-winfo
;;       '((:name main)
;;         (:name tree    :plugin direx)
;;         (:name history :plugin history-list)
;;         (:name imenu   :plugin imenu :default-hide nil)
;;         (:name sub     :buffer "*info*" :default-hide t)))

;;; Customization:
;; 
;; Nothing

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "e2wm-direx:[^:]" :docstring t)
;; `e2wm-direx:start-sync-timer'
;; Not documented.
;; `e2wm-direx:stop-sync-timer'
;; Not documented.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-08-22 on chindi02, modified by Debian
;; - e2wm.el ... Version 1.2
;; - direx.el ... Version 0.1alpha


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'direx)
(require 'direx-project)
(require 'hl-line)

(defvar e2wm-direx::winfo nil)
(defvar e2wm-direx::sync-timer nil)
(defvar e2wm-direx::current-path nil)

(defun e2wm-direx:def-plugin (frame wm winfo)
  (setq e2wm-direx::winfo winfo)
  (setq e2wm-direx::current-path nil)
  (e2wm-direx:start-sync-timer))

(defun e2wm-direx:start-sync-timer ()
  (interactive)
  (when (not e2wm-direx::sync-timer)
    (setq e2wm-direx::sync-timer
          (run-with-idle-timer idle-update-delay
                               t
                               'e2wm-direx:do-sync))
    (e2wm:message "DirEX timer started.")))

(defun e2wm-direx:stop-sync-timer ()
  (interactive)
  (when (timerp e2wm-direx::sync-timer)
    (cancel-timer e2wm-direx::sync-timer))
  (setq e2wm-direx::sync-timer nil)
  (e2wm:message "DirEX timer stopped."))

(defun e2wm-direx:do-sync ()
  (let* ((wm (e2wm:pst-get-wm))
         (wname (wlf:window-name e2wm-direx::winfo))
         (mbuf (e2wm:history-get-main-buffer))
         (mpath (buffer-file-name mbuf))
         (buf (when (and mpath
                         (not (string= mpath e2wm-direx::current-path)))
                (e2wm:message "DirEX update current path : %s"
                              (setq e2wm-direx::current-path mpath))
                (with-current-buffer mbuf
                  (or (ignore-errors
                        (direx-project:find-project-root-noselect (or buffer-file-name
                                                                      default-directory)))
                      (direx:find-directory-reuse-noselect default-directory)))))
         (ptpath (when buf
                   (e2wm:message "DirEX update active buffer")
                   (wlf:set-buffer wm wname buf)
                   (with-current-buffer buf
                     (hl-line-mode 1)
                     (direx:aif (direx:item-at-point)
                         (direx:file-full-name (direx:item-tree it))
                       "")))))
    (when (and ptpath
               (not (string= mpath ptpath)))
      (e2wm:message "DirEX move point to %s" mpath)
      (with-current-buffer buf
        (direx:refresh-whole-tree))
      (with-current-buffer mbuf
        (direx:maybe-goto-current-buffer-item buf))
      (with-current-buffer buf
        (hl-line-highlight)
        (set-window-point (wlf:get-window wm wname) (point))))))

(e2wm:plugin-register 'direx "DirEX" 'e2wm-direx:def-plugin)


(provide 'e2wm-direx)
;;; e2mw-direx.el ends here
