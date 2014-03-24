;;; el-dispatcher.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defvar dispatcher-prefix-arg nil
  "Hack to pass or simulate `current-prefix-arg' when the
dispatcher isn't called directly.")

(defun el-dispatcher-make (function handlers)
  "Transform FUNCTION into a dispatcher using HANDLERS.

A dispatcher is a function that has different behavior depending
on the prefix.

For example given the following function:
\(defun my-browse-url (url)
   (interactive \"MURL: \")
   (list url))

You can transform it into a dispatcher with different handlers:
\(el-dispatcher-make 'my-browse-url '((w3m w3m-browse-url)
                                   (firefox browse-url-firefox)))

Then:
M-x my-browse-url RET <url> RET -> call w3m-browse-url
C-1 M-x my-browse-url RET <url> RET -> call browse-url-firefox
C-u M-x my-browse-url RET <url> RET -> prompt for a handler

Then handler is called with the result of the initial function.
\(apply 'handler (initial-function ARG))
"
  ;; Remove the old dispatcher (if any)
  (remove-function (symbol-function function) 'el-dispatcher)
  ;; Add the new dispatcher
  (add-function :filter-return (symbol-function function)
                (lambda (args)
                  ;; this makes debugging easier
                  (el-dispatcher--dispatch args (or current-prefix-arg dispatcher-prefix-arg) handlers))
                '((name . el-dispatcher))))

(defun el-dispatcher--dispatch (args prefix handlers)
  (let* ((handlers (etypecase handlers
                     (symbol (symbol-value handlers))
                     (list handlers)))
         (handler (cdr-safe
                   (cond ((null prefix)
                          (first handlers))
                         ((integerp prefix)
                          (nth prefix handlers))
                         (t
                          (assoc (completing-read "Choose: "
                                                  handlers
                                                  nil
                                                  t)
                                 handlers))))))
    (if handler
        (apply
         ;; `handler' can be a variable or a function
         (if (functionp handler)
             handler
           (symbol-value handler)) args)
      (user-error "No handler `%S' found for `%S'" handler args))))


(provide 'el-dispatcher)

;;; el-dispatcher.el ends here
