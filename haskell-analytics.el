;;; haskell-analytics.el --- Collect useful stats about usage -*- lexical-binding: t -*-

;; Copyright (c) 2016 Haskell Mode contributors. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'calc-comb)
(require 'advice)

(defvar haskell-analytics--cid)

(defun haskell-analytics--compose-payload (symbol value)
  "Create a payload string.

SYMBOL is name of the function called, VALUE is how many times it
was called."
  (concat "v=1"
          "&tid=" "UA-72475079-2"
          "&cid=" haskell-analytics--cid
          "&t=event"
          "&ec=call"
          "&ea=" (symbol-name symbol)
          "&ev=" (number-to-string value)))

;; Do not want to force require request due to how currently testing
;; is configured.
(declare-function request "ext:request")

(defun haskell-analytics--send-payload (payload)
  "Send PAYLOAD to analytics."

  (require 'request)
  (request "https://www.google-analytics.com/collect"
           :type "POST"
           :data payload))

(defun haskell-analytics--send-call-counts (call-counts)
  "Send all information about call counts.

CALL-COUNTS should be hash mapping function symbol names to call counts."
  (maphash (lambda (key value)
             (haskell-analytics--send-payload
              (haskell-analytics--compose-payload key value))
             ) call-counts))

(defvar haskell-analytics--hash nil
  "Call counts of each function inside haskell-mode since last
sendout.")

(defvar haskell-analytics--timer-id nil
  "Timer that will send data.")

(defun haskell-analytics--run-timer ()
  "Function that sends data to google analytics."
  (unwind-protect
      (haskell-analytics--send-call-counts haskell-analytics--hash)
    (setq haskell-analytics--timer-id nil)
    (setq haskell-analytics--hash nil)))

(defun haskell-analytics--function-before (symbol)
  "Wrapper that record stats about function usage."
  (unless haskell-analytics--timer-id
    (setq haskell-analytics--timer-id
          (run-at-time 10 nil #'haskell-analytics--run-timer)))
  (unless haskell-analytics--hash
    (setq haskell-analytics--hash (make-hash-table :test 'eq)))
  (puthash symbol (1+ (gethash symbol haskell-analytics--hash 0)) haskell-analytics--hash))

(defun haskell-analytics--instrument-function (symbol &optional disable)
  "Instrument function in SYMBOL."
  (unless (member symbol '(haskell-analytics--function-before
                           haskell-analytics--instrument-function))
    (if disable
        ;; The `advice-add' and `advice-remove' are supported since 24.4
        ;; but not 24.3.  Therefore we use the old mechanism also. But
        ;; since the old mechanism is implemented using the new one on
        ;; 24.4 we would like to skip layrs of indirectness due to both
        ;; efficiency and more direct debugging.

        (if (fboundp 'advice-remove)
            (advice-remove symbol 'haskell-analytics)
          (ignore-errors
            ;; `ad-remove-advice' throws errors if advice is not there
            (ad-remove-advice symbol 'before 'haskell-analytics)))
      (if (fboundp 'advice-add)
          (advice-add symbol :before (lambda (&rest _ignore_arguments)
                                       (haskell-analytics--function-before symbol))
                      '((name . 'haskell-analytics)))
        (ad-add-advice symbol `(haskell-analytics nil t
                                                  (lambda (&rest _ignore_arguments)
                                                    (haskell-analytics--function-before ',symbol))) 'before 0)
        (ad-activate symbol)))))

(defvar haskell-analytics--load-file-name
  (or load-file-name (buffer-file-name))
  "Full path to current file. Haskell analytics instruments only
  files in the same directory.")

(defun haskell-analytics--is-haskell-mode-file (file-name)
  (and file-name
       ;; module is in the same directory as haskell-analytics
       (equal (file-name-directory file-name)
              (file-name-directory haskell-analytics--load-file-name))))

(defun haskell-analytics--instrument-load-history-item (load-history-item &optional disable)
  "FILE-NAME is used as key to lookup info in `load-history' alist.

All pairs of the form '(defun . function-name)' will be
instrumented."
  (dolist (item load-history-item)
    (when (and (consp item)
               (equal 'defun (car item)))
      (haskell-analytics--instrument-function (cdr item) disable))))

(defun haskell-analytics--instrument-file (file-name)
  "FILE-NAME is used as key to lookup info in `load-history' alist.

All pairs of the form '(defun . function-name)' will be
instrumented."
  (haskell-analytics--instrument-load-history-item (assoc file-name load-history)))

(defun haskell-analytics--instrument-haskell-mode-load-history (&optional disable)
  "Enumerate all loaded functions and instrument those that
belong to haskell-mode."
  (dolist (item load-history)
    (when (haskell-analytics--is-haskell-mode-file (car item))
      (haskell-analytics--instrument-load-history-item item disable))))

(defun haskell-analytics--after-load (file-name)
  "FILE-NAME is used as key to lookup info in `load-history' alist.

All pairs of the form '(defun . function-name)' will be
instrumented."
  (when (haskell-analytics--is-haskell-mode-file file-name)
    (haskell-analytics--instrument-file file-name)))

(defun haskell-analytics--setup (&optional disable)
  "Enable or disable haskell-analytics."
  (if disable
      (progn
        (remove-hook 'after-load-functions 'haskell-analytics--after-load)
        (haskell-analytics--instrument-haskell-mode-load-history disable)
        (when haskell-analytics--timer-id
          (cancel-timer haskell-analytics--timer-id)
          (setq haskell-analytics--timer-id nil)))
    (add-hook 'after-load-functions 'haskell-analytics--after-load)
    (haskell-analytics--instrument-haskell-mode-load-history)))

(defun haskell-analytics--cid-set (symbol value)
  (set-default symbol value)
  (haskell-analytics--setup (not value)))

(defun haskell-analytics--uuid-create ()
  "Create a pseudo-random UUID string."
  (format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
          ;; Note that `math-random-three-digit-number' gives numbers
          ;; in range 0..999, which is almost 0..1023. This is good
          ;; enough for our purpose.
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)
          (logand (math-random-three-digit-number) 255)))

(defun haskell-analytics-enable ()
  "Enable telemetry statistics.

Haskell Mode optionally can sample usage statistics and report
them back to central server. Samples track function call counts,
variable access count, key bindings, modes used alongside with
`haskell-mode', exception information, some customization
selections used, versions of Emacs, haskell-mode and basic
operating system information. Full information available in
function `haskell-analytics--send-call-counts'.

Disable telemetry with M-x `haskell-analytics-disable'.

Samples tracked pertain only to Haskell Mode buffers and do not
contain any personal information. As analytics provider Haskell
Mode uses Google Analytics. Privacy concerns are handled in the
same manner as analytics for web pages."
  (interactive)
  (cond
   (haskell-analytics--cid
    (message "Haskell Mode analytics already enabled."))
   (t
    (customize-save-variable 'haskell-analytics--cid (haskell-analytics--uuid-create))
    (custom-save-all)
    (message "Haskell Mode analytics enabled. Disable with M-x haskell-analytics-disable."))))

(defun haskell-analytics-disable ()
  "Disable telemetry statistics."
  (interactive)
  (cond
   (haskell-analytics--cid
    (customize-save-variable 'haskell-analytics--cid nil)
    (custom-save-all)
    (message "Haskell Mode analytics disabled. Enable with M-x haskell-analytics-enable."))
   (t
    (message "Haskell Mode analytics already disabled."))))

(defcustom haskell-analytics--cid nil
  "Client ID for Google Analytics."
  :type `(choice (const :tag "Do not use telemetry" nil)
                 (string :tag "This machine instance telemetry ID" :value ,(haskell-analytics--uuid-create)))
  :set 'haskell-analytics--cid-set
  :group 'haskell-analytics)

(provide 'haskell-analytics)
