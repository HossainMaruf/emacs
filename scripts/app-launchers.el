;;; app-launchers.el --- Possible alternatives to dmenu/rofi

;;; Code:

;; Counsel-Linux-App
;; Since we have counsel installed, we can use 'counsel-linux-app' to launch our Linux apps.  It list the apps by their executable command, so it's kind of tricky to use.

(defun dt/emacs-run-launcher ()
  "Launcher that works with the frame created by emacsclient."
  (interactive)
  (let ((frame (selected-frame)))
    (modify-frame-parameters frame
                             '((undecorated . t)
                               (internal-border-width . 10)
                               (menu-bar-lines . 0)
                               (tool-bar-lines . 0)))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame frame))))

;; App-Launcher
;; The 'app-launcher' is a better run launcher since it reads the desktop applications on your system and you can search them by their names as defined in their desktop file.  This means that sometimes you have to search for a generic term rather than the actual binary command of the program.

(use-package app-launcher
  :ensure '(app-launcher :host github :repo "SebastienWae/app-launcher"))
;; create a global keyboard shortcut with the following code
;; emacsclient -cF "((visibility . nil))" -e "(emacs-run-launcher)"

(defun dt/emacs-run-launcher-old ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame 
    (make-frame '((name . "emacs-run-launcher")
                  (minibuffer . only)
                  (fullscreen . 0) ; no fullscreen
                  (undecorated . t) ; remove title bar
                  ;;(auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  (internal-border-width . 10)
                  (width . 80)
                  (height . 11)))
                  (unwind-protect
                    (app-launcher-run-app)
                    (delete-frame))))

(defun dt/emacs-run-launcher ()
  "Launcher that works with the frame created by emacsclient."
  (interactive)
  (let ((frame (selected-frame)))
    (modify-frame-parameters frame
                             '((undecorated . t)
                               (width . 80)
                               (height . 11)
                               (internal-border-width . 10)
                               (menu-bar-lines . 0)
                               (tool-bar-lines . 0)))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame frame))))

;;  NEW LAUNCHERS 
(require 'json)    ;; for bookmarks and screenshots
(require 'cl-lib)  ;; for screenshots

(defun dt/emacs-generic-launcher (options prompt action-fn)
  "Create a minibuffer-only frame to select from OPTIONS with PROMPT.
ACTION-FN is a function that takes the selected string as an argument."
  (interactive)
  (with-selected-frame 
      (make-frame '((name . "emacs-launcher")
                    (minibuffer . only)
                    (undecorated . t)
                    (width . 80)
                    (height . 11)
                    (internal-border-width . 10)))
    (unwind-protect
        (let ((selection (completing-read prompt options)))
          (funcall action-fn selection))
      (delete-frame))))

(defun dt/emacs-command-launcher ()
  "A global M-x launcher that runs in a dedicated minibuffer frame."
  (interactive)
  (with-selected-frame 
      (make-frame '((name . "emacs-command-launcher")
                    (minibuffer . only)
                    (undecorated . t)
                    (width . 80)
                    (height . 15)
                    (internal-border-width . 10)))
    (unwind-protect
        ;; execute-extended-command is the built-in M-x
        (call-interactively #'execute-extended-command)
      (delete-frame))))

(defun dt/get-brave-bookmarks ()
  "Parse Brave bookmarks and return an alist of (name . url)."
  (let* ((path "~/.config/BraveSoftware/Brave-Browser/Default/Bookmarks") ;; Adjust "Default" if using profiles
         (json-object-type 'alist)
         (data (json-read-file (expand-file-name path)))
         (roots (cdr (assoc 'roots data)))
         (bookmarks '()))
    (cl-labels ((extract (node)
                  (let ((type (cdr (assoc 'type node)))
                        (children (cdr (assoc 'children node))))
                    (cond
                     ((string= type "url")
                      (push (cons (cdr (assoc 'name node))
                                  (cdr (assoc 'url node)))
                            bookmarks))
                     ((string= type "folder")
                      (mapc #'extract children))))))
      ;; Extract from both Bookmark Bar and Other Bookmarks
      (extract (cdr (assoc 'bookmark_bar roots)))
      (extract (cdr (assoc 'other roots))))
    bookmarks))

(defun dt/brave-bookmark-launcher ()
  "Open Brave bookmarks in a dmenu-like Emacs minibuffer."
  (interactive)
  (let* ((bookmarks (dt/get-brave-bookmarks))
         (choices (mapcar #'car bookmarks))) ; Get just the names for the menu
    (with-selected-frame 
        (make-frame '((name . "emacs-bookmark-launcher")
                      (minibuffer . only)
                      (undecorated . t)
                      (width . 80)
                      (height . 15)
                      (internal-border-width . 10)))
      (unwind-protect
          (let* ((selection (completing-read "Search Bookmarks: " choices))
                 (url (cdr (assoc selection bookmarks))))
            (when url
              ;; This uses your default browser (Brave) to open the link
              (browse-url url)))
        (delete-frame)))))

(defun dt/get-monitor-info ()
  "Robustly parse xrandr geometry, stripping physical size data for maim."
  (let ((xrandr-output (shell-command-to-string "xrandr --listactivemonitors")))
    (delq nil
          (mapcar (lambda (line)
                    ;; Regex matches: Name (column 3) and Geometry (column 2)
                    ;; Example line: 0: +*HDMI-A-0 1920/477x1080/268+1920+0
                    (when (string-match "[0-9]+: \\+\\*?\\([^ ]+\\) \\([0-9x/++-]+\\)" line)
                      (let ((name (match-string 1 line))
                            (geom (match-string 2 line)))
                        ;; Remove the physical size bits (e.g., /477) that xrandr adds
                        (cons name (replace-regexp-in-string "/[0-9]+" "" geom)))))
                  (split-string xrandr-output "\n" t)))))

(defun dt/screenshot-launcher ()
  "Emacs port of dm-maim with full monitor support and reliable clipboard handling."
  (interactive)
  (let* ((maim-dir (expand-file-name "~/Pictures/Screenshots/"))
         (maim-file-prefix "screenshot")
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (monitors (dt/get-monitor-info))
         (modes (append '("Fullscreen" "Active window" "Selected region") 
                        (mapcar #'car monitors)))
         (destinations '("File" "Clipboard" "Both")))

    (unless (file-exists-p maim-dir)
      (make-directory maim-dir t))

    (with-selected-frame 
        (make-frame '((name . "emacs-screenshot-launcher")
                      (minibuffer . only)
                      (undecorated . t)
                      (width . 80)
                      (height . 12)
                      (internal-border-width . 10)))
      (unwind-protect
          (let* ((target (completing-read "Take screenshot of: " modes))
                 (maim-target-args 
                  (cond ((string= target "Active window") 
                         (concat "-i " (string-trim (shell-command-to-string "xdotool getactivewindow"))))
                        ((string= target "Selected region") "-s")
                        ((assoc target monitors) 
                         (concat "-g " (cdr (assoc target monitors))))
                        (t "")))
                 
                 (file-type (if (assoc target monitors) target (downcase target)))
                 (delay (completing-read "Delay (seconds): " '("0" "1" "2" "3" "4" "5")))
                 (delay-arg (if (string= delay "0") "0.5" delay))
                 (dest (completing-read "Destination: " destinations))
                 
                 (filename (expand-file-name 
                            (format "%s-%s-%s.png" 
                                    maim-file-prefix 
                                    (replace-regexp-in-string " " "_" file-type) 
                                    timestamp) 
                            maim-dir))
                 (maim-args (format "%s --delay=%s -q" maim-target-args delay-arg))
                 
                 ;; USE XCLIP -LOOP 1 OR REDIRECTS TO ENSURE PERSISTENCE
                 (clipboard-cmd "xclip -selection clipboard -t image/png")
                 (full-command 
                  (cond
                   ((string= dest "File")
                    (format "maim %s %s && notify-send 'Saved Screenshot' '%s'" 
                            maim-args filename filename))
                   
                   ((string= dest "Clipboard")
                    (format "maim %s | %s && notify-send 'Saved' 'Clipboard'" 
                            maim-args clipboard-cmd))
                   
                   ((string= dest "Both")
                    (format "maim %s | tee %s | %s && notify-send 'Saved' 'File & Clipboard'" 
                            maim-args filename clipboard-cmd)))))

            ;; Use 'call-process-shell-command' with an ampersand to truly 
            ;; background it without Emacs waiting or killing the pipe too early.
            (call-process-shell-command (concat full-command " &") nil 0))
        
        (delete-frame)))))

(provide 'app-launchers)
;;; app-launchers.el ends here

