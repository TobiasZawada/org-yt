;;; org-yt.el --- Org youtube links.                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  U-ESI-INTERNAL\TOZ

;; Author: U-ESI-INTERNAL\TOZ <TOZ@smtp.1und1.de>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Idea from  https://emacs.stackexchange.com/questions/38098/org-mode-custom-youtube-link-syntax

;;; Code:

(require 'org)
(require 'org-element)

(defcustom org-yt-url-protocol "yt"
  "Protocol identifier for youtube links."
  :group 'org-yt
  :type 'string)

(defun org-yt-follow (video-id)
  "Open youtube with VIDEO-ID."
  (browse-url (concat "https://youtu.be/" video-id)))

(org-link-set-parameters org-yt-url-protocol :follow #'org-yt-follow)

(defun org-image-update-overlay (file link &optional data-p refresh)
  "Create image overlay for FILE associtated with org-element LINK.
If DATA-P is non-nil FILE is not a file name but a string with the image data.
If REFRESH is non-nil don't download the file but refresh the image.
See also `create-image'.
This function is almost a duplicate of a part of `org-display-inline-images'."
  (when (or data-p (file-exists-p file))
    (let ((width
           ;; Apply `org-image-actual-width' specifications.
           (cond
            ((not (image-type-available-p 'imagemagick)) nil)
            ((eq org-image-actual-width t) nil)
            ((listp org-image-actual-width)
             (or
              ;; First try to find a width among
              ;; attributes associated to the paragraph
              ;; containing link.
              (let ((paragraph
                     (let ((e link))
                       (while (and (setq e (org-element-property
                                            :parent e))
                                   (not (eq (org-element-type e)
                                            'paragraph))))
                       e)))
                (when paragraph
                  (save-excursion
                    (goto-char (org-element-property :begin paragraph))
                    (when
                        (re-search-forward
                         "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                         (org-element-property
                          :post-affiliated paragraph)
                         t)
                      (string-to-number (match-string 1))))))
              ;; Otherwise, fall-back to provided number.
              (car org-image-actual-width)))
            ((numberp org-image-actual-width)
             org-image-actual-width)))
          (old (get-char-property-and-overlay
                (org-element-property :begin link)
                'org-image-overlay)))
      (if (and (car-safe old) refresh)
          (image-refresh (overlay-get (cdr old) 'display))
        (let ((image (create-image file
                                   (and width 'imagemagick)
                                   data-p
                                   :width width)))
          (when image
            (let* ((link
                    ;; If inline image is the description
                    ;; of another link, be sure to
                    ;; consider the latter as the one to
                    ;; apply the overlay on.
                    (let ((parent
                           (org-element-property :parent link)))
                      (if (eq (org-element-type parent) 'link)
                          parent
                        link)))
                   (ov (make-overlay
                        (org-element-property :begin link)
                        (progn
                          (goto-char
                           (org-element-property :end link))
                          (skip-chars-backward " \t")
                          (point)))))
              (overlay-put ov 'display image)
              (overlay-put ov 'face 'default)
              (overlay-put ov 'org-image-overlay t)
              (overlay-put
               ov 'modification-hooks
               (list 'org-display-inline-remove-overlay))
              (push ov org-inline-image-overlays)
              ov)))))))

(defun org-yt-get-image (url)
  "Retrieve image from URL."
  (let ((image-buf (url-retrieve-synchronously url)))
    (when image-buf
      (with-current-buffer image-buf
        (goto-char (point-min))
        (when (looking-at "HTTP/")
          (delete-region (point-min)
                         (progn (re-search-forward "\n[\n]+")
                                (point))))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defconst org-yt-video-id-regexp "[-_[:alnum:]]\\{10\\}[AEIMQUYcgkosw048]"
  "Regexp matching youtube video id's taken from `https://webapps.stackexchange.com/questions/54443/format-for-id-of-youtube-video'.")

(defun org-yt-display-inline-images (&optional _include-linked _refresh beg end)
  "Like `org-display-inline-images' but for yt-links.
_INCLUDE-LINKED and _REFRESH are ignored.
Restrict to region between BEG and END if both are non-nil."
  (interactive)
  (when (and (called-interactively-p 'any)
             (use-region-p))
    (setq beg (region-beginning)
          end (region-end)))
  (when (display-graphic-p)
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((re (format "\\[\\[%s:\\(%s\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]" org-yt-url-protocol org-yt-video-id-regexp)))
       (while (re-search-forward re end t)
         (let ((video-id (match-string 1))
               (description (match-string-no-properties 2))
               (el (save-excursion (goto-char (match-beginning 1)) (org-element-context)))
               image-data)
           (when el
             (setq image-data
                   (or (let ((old (get-char-property-and-overlay
                                   (org-element-property :begin el)
                                   'org-image-overlay)))
                         (and old
                              (car-safe old)
                              (overlay-get (cdr old) 'display)))
                       (org-yt-get-image (format "http://img.youtube.com/vi/%s/0.jpg" video-id))))
             (when image-data
               (let ((ol (org-image-update-overlay image-data el t t)))
                 (when (and ol description)
                   (overlay-put ol 'after-string description)))))))))))

(advice-add #'org-display-inline-images :after #'org-yt-display-inline-images)

(provide 'org-yt)
;;; org-yt.el ends here
