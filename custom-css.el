;;; custom-css.el --- Load custom CSS snippets -*- lexical-binding: t -*-

;; Copyright (C) 2023 Florian Rommel

;; Author: Florian Rommel <mail@florommel.de>
;; Maintainer: Florian Rommel <mail@florommel.de>
;; Url: https://github.com/florommel/custom-css
;; Created: 2023-12-17
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces css

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Style your Emacs with custom CSS snippets.
;; Works with GTK only.

;;; Code:

(defgroup custom-css nil
  "Style your Emacs with custom CSS snippets."
  :group 'faces)

(defcustom custom-css-make-program "make"
  "The make program that is used to build the `custom-css-module'."
  :type 'string)

(defcustom custom-css-compile-buffer-name "*Compile custom-css-module*"
  "The name of the compile buffer for the `custom-css-module'."
  :type 'string)

(defvar custom-css-available nil
  "Determines if the module could be built and `custom-css' can be used.")

(defun custom-css-module-compile ()
  "Compile the custom-css-module."
  (interactive)
  (let* ((buffer (get-buffer-create custom-css-compile-buffer-name))
         (wdir (shell-quote-argument
                (file-name-directory (locate-library "custom-css.el" t))))
         (inhibit-read-only t))
    (pop-to-buffer buffer)
    (compilation-mode)
    (if (zerop
         (call-process custom-css-make-program nil buffer t "-C" wdir))
        (message "Compilation of `custom-css-module' successful")
      (warn "`custom-css': Could not build `custom-css-module'."))))

(defun custom-css--after-make-frame-function (frame)
  "Hook function for `after-make-frame-functions'.
Initializes the native module and removes itself from the hook when
the initialization was successful.  Argument FRAME is not used."
  (ignore frame)
  (when (custom-css-try-init)
    (remove-hook 'after-make-frame-functions
                 #'custom-css--after-make-frame-function)))

(autoload 'custom-css-load "custom-css-module"
  "Load and apply a CSS snippet into Emacs." nil nil)
(autoload 'custom-css-unload "custom-css-module"
  "Unload and remove a previously loaded CSS snippet" nil nil)
(autoload 'custom-css-unload-all "custom-css-module"
  "Unload all loaded custom CSS snippets." nil nil)
(autoload 'custom-css-try-init "custom-css-module"
  "Try to initialize custom-css." nil nil)

(let* ((wdir (shell-quote-argument
              (file-name-directory (locate-library "custom-css.el" t))))
       (fname (expand-file-name "custom-css-module" wdir)))
  (when (or
         (load fname t t)
         (progn
           (custom-css-module-compile)
           (load fname t nil)))
    (setq custom-css-available t)
    (unless (custom-css-try-init)
      (add-hook 'after-make-frame-functions
                #'custom-css--after-make-frame-function))))

(provide 'custom-css)

;;; custom-css.el ends here
