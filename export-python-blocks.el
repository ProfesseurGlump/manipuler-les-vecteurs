;;; export-python-simple.el -- Export simple des blocs Python

(defun export-python-blocks-simple ()
  "Exporter tous les blocs Python des fichiers Org."
  (interactive)
  (let ((output-dir "python_code")
        (counter 0))
    
    ;; Créer le dossier
    (unless (file-directory-p output-dir)
      (make-directory output-dir))
    
    ;; Parcourir tous les fichiers .org dans includes/
    (dolist (org-file (directory-files-recursively "includes" "\\.org$"))
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (goto-char (point-min))
          (let ((block-num 0))
            (while (search-forward "#+BEGIN_SRC python" nil t)
              (setq block-num (1+ block-num))
              (let* ((block-start (point-at-bol))
                     (block-end (progn
                                  (search-forward "#+END_SRC")
                                  (point-at-eol)))
                     (block-content (buffer-substring-no-properties 
                                     (+ block-start (length "#+BEGIN_SRC python") 1)
                                     (- block-end (length "#+END_SRC"))))
                     (base-name (file-name-base org-file))
                     (filename (format "%s/%s_%03d.py" output-dir base-name block-num)))
                
                ;; Nettoyer
                (setq block-content (string-trim block-content))
                
                ;; Écrire le fichier
                (with-temp-file filename
                  (insert (format "# Extracted from: %s\n" org-file))
                  (insert (format "# Block: %d\n\n" block-num))
                  (insert block-content))
                
                (message "Exported: %s" filename)
                (setq counter (1+ counter))))))))
    
    (message "Done! Exported %d Python blocks to %s/" counter output-dir)))

;; Pour tester rapidement dans un buffer
(defun test-find-python-blocks ()
  "Tester la recherche de blocs Python dans le buffer courant."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^[ \t]*#\\+BEGIN_SRC python" nil t)
        (setq count (1+ count))
        (let ((start (point))
              (end (progn (search-forward "#+END_SRC") (point))))
          (message "Block %d: lines %d-%d" 
                   count 
                   (line-number-at-pos start)
                   (line-number-at-pos end))))
      (message "Found %d Python blocks" count))))
