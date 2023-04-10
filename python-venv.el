;;; python-venv.el --- Module for managing python virtualenvs. ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2018,2019,2020,2021,2022,2023
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Saturday 04 February 2023 00:32:38 AM IST>
;; Package-Requires: ((emacs "25.1"))
;; URL: 	https://github.com/akshaybadola/python-venv.el
;; Keywords:	tools, python, venv, virtualenv
;; Version: 0.1.0

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Simple functions with only built-in dependencies for managing python virtualenvs.
;; Has functions to check if a virtualenv exists for a given path, if correct python
;; version is installed in the system, creation and update of a virtualenv path.
;; Also to check an update from pypi for a module exists and install/update if
;; required.
;;
;; The library is intended to be used with packages that interface with python
;; libraries, where you need to create and maintain python virtual environments
;; for those libraries.
;;
;; Only supported on POSIX like systems with "pip" and "grep" present on the system.

;;; Code:

(defun python-venv-or-env-python (env-or-python)
  "Return possible python executable for ENV-OR-PYTHON.

ENV-OR-PYTHON can be a virtualenv dir or python executable."
  (let* ((env-or-python (expand-file-name (string-remove-suffix "/" env-or-python)))
         (python (if (and (file-directory-p env-or-python)
                          (file-exists-p (concat env-or-python "/bin/python"))
                          (file-executable-p (concat env-or-python "/bin/python")))
                     (concat env-or-python "/bin/python")
                   (and (file-executable-p env-or-python) env-or-python))))
    (if python python
      (error "%s is not a valid executable" python))))

(defsubst python-venv-source-dir-valid-p (source-dir)
  "Check if SOURCE-DIR is a valid python package source dir.

Only setup.py like source dirs are supported for now."
  (and source-dir
       (file-directory-p source-dir)
       (file-exists-p (concat (string-remove-suffix "/" source-dir) "/setup.py"))))

(defun python-venv-get-python-version (python)
  "Return the version for a python executable PYTHON."
  (let ((python (python-venv-or-env-python python)))
    (cadr (split-string (shell-command-to-string (format "%s --version" python))))))

(defun python-venv-mod-in-venv-p (python modname)
  "Check if venv with python path PYTHON has MODNAME module installed."
  (let ((python (python-venv-or-env-python python)))
    (not (string-empty-p (python-venv-installed-mod-version-string python modname)))))

(defun python-venv-get-pypi-version (modname)
  "Get version info from pypi for module MODNAME."
  (let* ((url (format "https://pypi.org/pypi/%s/json" modname))
         (pkg-info (with-current-buffer (url-retrieve-synchronously url)
                     (goto-char (point-min))
                     (re-search-forward "\r?\n\r?\n")
                     (json-read))))
    (alist-get 'version (alist-get 'info pkg-info))))

(defun python-venv-installed-mod-version-string (python modname)
  "Return string version of the installed MODNAME module.

PYTHON is the env dir or python exectubale.

Requires \"pip\" and \"grep\"."
  (let* ((python (python-venv-or-env-python python))
         (pip (and python (concat (file-name-parent-directory python) "pip")))
         (module (and pip (string-trim
                           (shell-command-to-string
                            (format "%s freeze | grep -i %s" pip modname)))))
         (editable-string (and module
                               (string-match (format "^-e \\(.+%s@.+\\)" modname) module)
                               (match-string 1 module)))
         (version-string (and module
                              (not editable-string)
                              (string-match (format "^%s==\\(.+\\)" modname) module)
                              (match-string 1 module))))
    (or editable-string version-string)))

(defun python-venv-env-uninstall-module (env modname)
  "Uninstall the MODNAME module from virtualenv ENV."
  (let* ((python (python-venv-or-env-python env))
         (pip (concat (file-name-parent-directory python) "pip")))
    (shell-command
     (format "%s uninstall %s" pip modname)
     (format "*%s-uninstall-cmd*" modname) (format "*%s-uninstall-cmd*" modname))))

(defun python-venv-env-install-module (env modname &optional source-dir)
  "Install MODNAME module in virtualenv ENV.

If we have a source directory in optional SOURCE-DIR then
install from that else, fetch from pypi."
  (if (and source-dir (python-venv-source-dir-valid-p source-dir))
      (python-venv-env-install-module-from-source env modname source-dir)
    (python-venv-env-install-module-from-pypi env modname)))

(defun python-venv-env-install-module-from-source (env modname source-dir &optional editable)
  "Install a module MODNAME from SOURCE-DIR in virtualenv ENV from source.
With optional non-nil EDITABLE, install as editable with \"pip install -e .\"."
  (let* ((python (python-venv-or-env-python env))
         (pip (concat (file-name-parent-directory python) "pip"))
         (flag (if editable "-e" "-U")))
    (shell-command
     (format "cd %s && %s -m pip install %s ." source-dir pip flag)
     (format "*%s-install-cmd*" modname) (format "*%s-install-cmd*" modname))))

(defun python-venv-env-install-module-from-pypi (env modname &optional update)
  "Install the MODNAME module in virtualenv ENV with pypi.

Update the module with optional UPDATE."
  (let* ((python (python-venv-or-env-python env))
         (pip (concat (file-name-parent-directory python) "pip")))
    (shell-command
     (format "%s install %s %s" pip (if update "-U" "") modname)
     (format "*%s-install-cmd*" modname) (format "*%s-install-cmd*" modname))))

(defun python-venv-get-system-python ()
  "Get system python3 executable.

Detect if we are in a virtual environment and ask confirmation
from user if we are."
  (let* ((python (executable-find "python3"))
         (in-venv (string= "False" (string-trim
                                    (shell-command-to-string
                                     (format "%s -c 'import sys; print(sys.prefix == sys.base_prefix)'"
                                             python))))))
    (unless python
      (error "No python3 executable found in current paths"))
    (if in-venv
        (if (y-or-n-p
             (format "Python executable %s seems to be in a virtualenv.  Really use? " python))
            python
          (ido-read-file-name "Enter python executable to use: "))
      python)))

;; TODO: Add `venv' alternative to `virtualenv' in case it doesn't exist
(defun python-venv-create-venv (python path)
  "Create a new `ref-man' virtual env in directory PATH.
The python executable to use is defined by PYTHON."
  (let* ((python (python-venv-or-env-python python))
         (python-version (file-name-nondirectory (file-truename python))))
    (if (and (string-match-p "no.*module.*virtualenv.*"
                             (shell-command-to-string
                              (format "%s -m virtualenv -p %s %s"
                                      python python-version path)))
             (string-match-p "no.*module.*virtualenv.*"
                             (shell-command-to-string
                              (format "%s -m virtualenv -p %s %s"
                                      python python-version path))))
        nil (message "Create venv with %s in %s" python-version path))))

(defun python-venv-maybe-create-venv (python env reinstall)
  "Maybe create python virtualenv in directory ENV.

PYTHON is the system python exectutable to use.

REINSTALL removes the virtualenv and installs everything again
if ENV exists already."
  (cond ((not (file-exists-p env))
         (make-directory env t)
         (unless (python-venv-create-venv python env)
           (error "Could not install venv.\n
Make sure package 'virtualenv' exists in current python environment")))
        ((and reinstall (file-exists-p env)
              (y-or-n-p (format "Clean and reinstall virtualenv %s? " env)))
         (delete-directory env t)
         (make-directory env t)
         (unless (python-venv-create-venv python env)
           (error "Could not install venv.\n
Make sure package 'virtualenv' exists in current python environment")))
        (t (message "Not creating new venv in %s." env))))

(defun python-venv-setup-env (env &optional python min-python-version reinstall requirements)
  "Setup python virtualenv in directory ENV.

If ENV doesn't exist, then make directory and create the virtual environment.

Clean the directory ENV and create virtualenv if optional REINSTALL is non-nil.

Optional PYTHON specifies the python executable to use.  Defaults to system
python3.

Optional MIN-PYTHON-VERSION specifies the minimum python version for sanity
check.

Optional REQUIREMENTS is a space separated requirements spec for pip."
  (let* ((python (or python (python-venv-get-system-python)))
         (py-version (and python (python-venv-get-python-version python))))
    (when (and min-python-version (version< py-version min-python-version))
      (error "Your default python3 executable is < %s.
The setup requires at least %s"
             min-python-version min-python-version))
    (python-venv-maybe-create-venv python env reinstall)
    (let ((python (python-venv-or-env-python env))
          (pip (concat (file-name-parent-directory python) "pip")))
      ;; try once again with reinstall if python not found
      (when (and (not python) (python-venv-maybe-create-venv python env t))
        (error "Could not install venv.\n
Make sure package 'virtualenv' exists in current python environment"))
      (when requirements
        (shell-command
         (format "%s install %s" pip requirements)
         (format "*%s-requirements-install-cmd*" env)
         (format "*%s-requirements-install-cmd*" env))))))

(provide 'python-venv)

;;; python-venv.el ends here
