;;; init-emms.el --- Init customization

;;; Commentary:

;; This contains init customization for emms.

;;; Code:

;; playback
(require 'emms-setup)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-simple)
(require 'emms-player-vlc)
(require 'emms-cache)
;; track information
(require 'emms-cue)
(require 'emms-info-exiftool)
(require 'emms-info-native)
(require 'emms-show-all)
(require 'emms-mode-line)
;; browsing
(require 'emms-browser)
(require 'emms-playlist-mode)
(require 'emms-playlist-limit)
(require 'emms-metaplaylist-mode)
(require 'dired)

(defun emms-info-my-track-description (track)
  "Return a description of TRACK."
  (let ((artist (or
                 (emms-track-get track 'info-albumartist)
                 (emms-track-get track 'info-artist)))
        (title  (emms-track-get track 'info-title))
        (duration (emms-track-get track 'info-playing-time))
        (discnumber (emms-track-get track 'info-discnumber))
        (tracknumber (emms-track-get track 'info-tracknumber)))
    (if (or artist title)
        (concat
         (when (stringp tracknumber) (format "%2s. " tracknumber)) (when (numberp tracknumber) (format "%2d. " tracknumber))
         (when (stringp discnumber) (format "(%s) " discnumber)) (when (numberp discnumber) (format "(%d) " discnumber))
         (when artist (format "%s - " artist))
         title
         (format "   (%02d:%02d)" (/ duration 60) (% duration 60)))
      (emms-track-simple-description track))))

(defun optional-flag-string (flag string)
  (if flag string ""))

(defun emms-my-mode-line-display ()
  (concat " ["
          (buffer-name emms-playlist-buffer)
          (if emms-player-paused-p " paused")
          (optional-flag-string (with-current-emms-playlist emms-random-playlist) " random")
          (optional-flag-string (with-current-emms-playlist emms-repeat-track) " repeat-track")
          " ]"))

(setq
 emms-player-list '(emms-player-vlc
                    emms-player-alsaplayer
                    emms-player-mpg321
                    emms-player-ogg123)
 ;; emms-info-functions '(emms-info-exiftool)
 emms-info-functions '(emms-info-native emms-info-cueinfo)
 emms-repeat-playlist t
 emms-info-asynchronously nil
 emms-playlist-mode-open-playlists t
 emms-source-playlist-default-format 'm3u
 emms-track-description-function #'emms-info-my-track-description
 emms-mode-line-mode-line-function #'emms-my-mode-line-display)

(add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
(add-hook 'emms-player-paused-hook #'emms-mode-line-alter)

(emms-cache 1)
(emms-mode-line-mode)

(defvar my-emms-playlist-directory "~/Music/playlists/")
(defvar my-emms-library-directory "~/Music/library/")

(defun emms-metaplaylist-mode-new-buffer-no-update (buffer-name)
  "Creates a new buffer playlist buffer BUFFER-NAME but skip `emms-metaplaylist-mode-update'."
  (interactive "sBuffer Name: ")
  (if (get-buffer buffer-name)
      (error "Buffer must not exist.")
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
	(emms-playlist-mode)
	(setq emms-playlist-buffer-p t)))))

(defun emms-browser-replace-playlist ()
  (interactive)
  (emms-browser-clear-playlist)
  (emms-browser-add-tracks))

(defun emms-my-play-playlist ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (emms-playlist-set-playlist-buffer emms-playlist-buffer-name)
    (call-interactively #'emms-play-playlist)))

(defun emms-metaplaylist-my-load-playlist (file)
  (interactive (list (read-file-name "Load playlist file: "
                                     my-emms-playlist-directory
                                     my-emms-playlist-directory
                                     t)))
  (let* ((filename (file-name-nondirectory file))
         (playlist-name (concat " *EMMS Playlist: " filename "*")))
    (unless (get-buffer playlist-name)
      (emms-metaplaylist-mode-new-buffer-no-update playlist-name))
    (emms-playlist-set-playlist-buffer playlist-name)
    (emms-play-playlist file)))

(defun emms-metaplaylist-my-find-playlist (file)
  (interactive (list (read-file-name "Find playlist file: "
                                     my-emms-playlist-directory
                                     my-emms-playlist-directory
                                     t)))
  (let* ((filename (file-name-nondirectory file))
         (current-playlist emms-playlist-buffer)
         (playlist-name (concat " *EMMS Playlist: " filename "*")))
    (unless (get-buffer playlist-name)
      (emms-metaplaylist-mode-new-buffer playlist-name)
      (with-current-buffer playlist-name
        (emms-insert-playlist file)))))

(defun emms-metaplaylist-my-load-all-playlists (directory)
  (interactive (list (read-directory-name "Load all playlists in: "
                                          my-emms-playlist-directory
                                          my-emms-playlist-directory
                                          t)))
  (dolist (file (directory-files directory t directory-files-no-dot-files-regexp))
    (emms-metaplaylist-my-find-playlist file))
  (emms-metaplaylist-mode-update))

(dolist (playlist (directory-files my-emms-playlist-directory t directory-files-no-dot-files-regexp))
  (message "%s" playlist))
(directory-files my-emms-playlist-directory t directory-files-no-dot-files-regexp)

(defun emms-my-playlist-save ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (call-interactively #'emms-playlist-save)))

(defun emms-playlist-write (format file)
  "Store the current playlist to FILE as the type FORMAT.
The default format is specified by `emms-source-playlist-default-format'."
  (interactive (list (emms-source-playlist-read-format)
                     (read-file-name "Store as: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     nil)))
  (if emms-playlist-buffer-p
      (let ((emms-playlist-buffer-to-write (current-buffer)))
        (with-temp-buffer
          (emms-source-playlist-unparse format
                                        emms-playlist-buffer-to-write
                                        (current-buffer))
          (let ((backup-inhibited t))
	    (write-file file emms-source-playlist-ask-before-overwrite))))
    (message "aborting save")))

(defun emms-my-playlist-write ()
  (interactive)
  (let ((emms-source-file-default-directory my-emms-playlist-directory))
    (call-interactively #'emms-playlist-write)))

(defun emms-my-toggle-random-playlist ()
  (interactive)
  (emms-toggle-random-playlist)
  (setq emms-repeat-track nil)
  (emms-mode-line-alter))

(defun emms-my-toggle-repeat-track ()
  (interactive)
  (emms-toggle-repeat-track)
  (when emms-repeat-track
    (customize-set-variable 'emms-random-playlist nil))
  (emms-mode-line-alter))

(let ((map global-map))
  (define-key map (kbd "C-c M L") #'emms-metaplaylist-my-load-playlist)
  (define-key map (kbd "C-c M b") #'emms-metaplaylist-mode-go)
  (define-key map (kbd "C-c M l") #'emms-playlist-mode-switch-buffer)
  (define-key map (kbd "C-c M B") #'emms-smart-browse)
  (define-key map (kbd "C-c M %") #'emms-my-toggle-random-playlist)
  (define-key map (kbd "C-c M 1") #'emms-my-toggle-repeat-track)
  (define-key map (kbd "C-c M m") #'emms-mode-line-mode)
  (define-key map [remap emms-playlist-save] #'emms-my-playlist-save)

  (define-key map (kbd "C-c M f") #'emms-show)
  (define-key map (kbd "C-c M F") #'emms-show-all)
  (define-key map (kbd "C-c M n") #'emms-next)
  (define-key map (kbd "C-c M p") #'emms-previous)
  (define-key map (kbd "C-c M P") #'emms-pause)
  (define-key map (kbd "C-c M s") #'emms-stop))

(let ((map emms-playlist-mode-map))
  (define-key map (kbd "F") #'emms-show-all)
  (define-key map (kbd "z") #'emms-metaplaylist-mode-go)
  (define-key map (kbd "%") #'emms-shuffle)
  (define-key map (kbd "M") #'emms-mark-mode)
  (define-key map (kbd "C-x C-w") #'emms-my-playlist-write))

(let ((map emms-mark-mode-map))
  (define-key map (kbd "M") #'emms-mark-mode-disable))

(let ((map emms-browser-mode-map))
  (define-key map (kbd "R") #'emms-browser-replace-playlist)
  (define-key map (kbd "a") #'emms-add-directory-tree))

(let ((map emms-metaplaylist-mode-map))
  (define-key map (kbd "f") #'emms-metaplaylist-my-find-playlist)
  (define-key map (kbd "z") #'emms-playlist-mode-go)
  (define-key map (kbd "G") #'emms-metaplaylist-my-load-all-playlists))

(let ((map dired-mode-map))
  (define-key map (kbd "C-c M a") #'emms-add-dired)
  (define-key map (kbd "C-c M A") #'emms-play-dired))

(provide 'init-emms)
;;; init-emms.el ends here
