;;; ciao-org.el --- Ciao interaction with Emacs Org Mode
;; Copyright (C) 1986-2012 Free Software Foundation, Inc. and

;; Authors: 2012      Jose F. Morales <jfran@clip.dia.fi.upm.es>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'ciao-config) ; ciao-get-config

;; TODO: Missing documentation. This mode is not enabled by default.

;; Features:
;;
;;  - 'ciao-src' hyperlinks for Org files (e.g.,
;;    [[ciao-src:ide/emacs-mode/ciao-org.el]] points here)

(require 'org)

(org-add-link-type "ciao-src" 'ciao-org-src-open)

(defun ciao-org-src-open (path)
  "Visit the PATH relative to the Ciao source"
  (find-file (concat (ciao-get-config :bundledir-core) "/../" path)))

