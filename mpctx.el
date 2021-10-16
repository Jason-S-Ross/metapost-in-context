;;; mpctx.el --- Metapost Processing for Org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jason Ross
;;
;; Author: Jason Ross <https://github.com/Jason-S-Ross>
;; Maintainer: Jason Ross <jasonross1024@gmail.com>
;; Created: October 07, 2021
;; Modified: October 07, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Jason-S-Ross/mpctx
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:
;;  See https://www.mail-archive.com/emacs-orgmode@gnu.org/msg139597.html
;;  Uses a hook to modify the source document if it's exported to ConTeXt.
;;
;;  FIXME: This is really a poor way to do this. However, I don't know
;;  what else to do!
;;
;;; Code:

(require 'org-element)
(require 'ox)

(defun mpctx-format-src-block-arguments (arguments)
  "Returns a formatted plist of header arguments"
  (mapconcat
   (lambda (argument)
     (let ((kw (car argument))
           (vals (cdr argument)))
       (concat (format "%s" kw)
               " "
               (format "%s" vals))))
   arguments
   " "))
(defun mpctx-metapost-process-hook (backend)
  "If BACKEND is `context', change metapost code blocks to output
raw code wrapped in #+BEGIN_METAPOST/#+END_METAPOST tags."
  ;; TODO This should be controlled by a flag.
  ;; TODO Check buffer info to see if we are allowed to do this.
  (when (string= backend "context")
    (goto-char (point-min))
    (let ((case-fold-search t)
          ;; Search for source code with a regex
          (regexp "^[ \t]*#\\+BEGIN_SRC"))
      (while (re-search-forward regexp nil t)
        (let* ((objectp (match-end 1))
               (tree (org-element-parse-buffer))
               ;; Get the buffer info plist (need this to export a caption)
               (info (org-combine-plists
                     (org-export--get-export-attributes)
                     (org-export-get-environment)))
               (info (progn
                      (org-export--prune-tree tree info)
                      (org-export--remove-uninterpreted-data tree info)
                      (org-combine-plists info
                                          (org-export--collect-tree-properties
                                           tree info))))
               ;; Get a code element
               (element
                (save-match-data
                  (if objectp (org-element-context) (org-element-at-point))))
               (caption (org-element-property :caption element))
               (type (org-element-type element))
               (begin (copy-marker (org-element-property :begin element)))
               (end (copy-marker
                     (save-excursion
                       (goto-char (org-element-property :end element))
                       (skip-chars-backward " \r\t\n")
                       (point))))
               (block-info (org-babel-get-src-block-info t))
               (language (nth 0 block-info))
               (body (nth 1 block-info))
               (arguments (nth 2 block-info))
               (arguments (delq (assoc :file arguments) arguments))
               (switches (nth 3 block-info))
               (name (nth 4 block-info))
               (start (nth 5 block-info))
               (coderef (nth 6 block-info)))

          (when (string= (downcase language) "metapost")
            ;; Remove "file" from `results' setting
            (setf (alist-get :results arguments)
                  (mapconcat
                   #'identity
                   (seq-filter
                    (lambda (a) (not (string= a "file")) )
                    (split-string (alist-get :results arguments)))
                   " "))
            ;; Add a wrap argument to wrap in a METAPOST special block
            (setf (alist-get :wrap arguments) "METAPOST")
            (pcase type
              (`src-block
               (progn
                 (delete-region begin end)
                 (goto-char begin)
                 (insert
                  (concat
                   ;; Captions and names got deleted; add them back
                   (when (org-string-nw-p name)
                     (format "#+NAME: %s \n" name))
                   (when caption
                     (format "#+CAPTION: %s\n"
                             (org-string-nw-p
                              (org-trim
                               (org-export-data
                                (or
                                 (org-export-get-caption element t)
                                 (org-export-get-caption element))
                                info)))))
                   ;; Add the (modified) header arguments back
                   (format "#+BEGIN_SRC metapost %s\n%s\n#+END_SRC"
                           (mpctx-format-src-block-arguments arguments)
                           body)
                   "\n"))))))))
      (goto-char (point-min)))))

(defun org-babel-execute:metapost (body params)
  "Execute a block of metapost code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (if (cdr (assq :file params))
      (let* ((out-file (cdr (assq :file params)))
             (cmdline (or (cdr (assq :cmdline params))
                          (format "-T%s" (file-name-extension out-file))))
             (cmd (or (cdr (assq :cmd params)) "mpost"))
             (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
             (coding-system-for-write 'utf-8)
             (in-file (org-babel-temp-file "metapost-" ".mp"))
             (in-dir (file-name-directory in-file))
             (thecmd
              (concat
               (format "(cd \"%s\";" (org-babel-process-file-name in-dir))
               cmd
               " -s 'outputformat=\"svg\"'"
               (format " -s 'outputtemplate=\"%s\"'" (org-babel-process-file-name out-file))
               " "
               (file-name-nondirectory in-file)
               ")")))
        (message in-file)
        (message (format "%s" thecmd))
        (with-temp-file in-file
          (insert (org-babel-expand-body:generic body params)))
        (org-babel-eval
         thecmd "")
        nil)
    body))

(add-hook 'org-export-before-processing-hook 'mpctx-metapost-process-hook)

(provide 'mpctx)
;;; mpctx.el ends here
