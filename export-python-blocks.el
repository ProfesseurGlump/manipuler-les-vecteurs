;;; export-python-blocks.el --- Exporter tous les blocs Python des fichiers Org

(defun export-all-python-blocks-from-org ()
  "Exporter tous les blocs Python de tous les fichiers Org dans includes/."
  (interactive)
  (let* ((org-files (directory-files-recursively "includes" "\\.org$"))
         (output-dir "python_code")
         (counter 1))
    
    ;; Créer le dossier de sortie s'il n'existe pas
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    
    (dolist (org-file org-files)
      (message "Traitement de: %s" org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^#\\+BEGIN_SRC python\\(.*\\)" nil t)
            (let* ((begin (match-beginning 0))
                   (end (save-excursion
                          (re-search-forward "^#\\+END_SRC" nil t)
                          (point)))
                   (content (buffer-substring-no-properties begin end))
                   (filename (format "%s/prog%03d.py" output-dir counter))
                   (header (match-string 1)))
              
              ;; Nettoyer le contenu (enlever les marqueurs)
              (setq content (replace-regexp-in-string "^#\\+BEGIN_SRC python.*\n" "" content))
              (setq content (replace-regexp-in-string "\n#\\+END_SRC$" "" content))
              
              ;; Écrire dans le fichier
              (with-temp-file filename
                ;; Ajouter un en-tête avec le fichier source
                (insert (format "# Code extrait de: %s\n" org-file))
                (when (and header (not (string-empty-p (string-trim header))))
                  (insert (format "# Options: %s\n" header)))
                (insert "# Generated automatically\n\n")
                (insert content))
              
              (message "  Exporté: %s" filename)
              (setq counter (1+ counter)))))))
    
    (message "Export terminé! %d fichiers créés dans %s/" (1- counter) output-dir)))

;; Version avancée avec noms de fichiers basés sur le contexte
(defun export-python-blocks-with-context ()
  "Exporter les blocs Python avec des noms basés sur le contexte."
  (interactive)
  (let* ((org-files (directory-files-recursively "includes" "\\.org$"))
         (output-dir "python_code")
         (counter 1)
         (file-counter (make-hash-table :test 'equal)))
    
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    
    (dolist (org-file org-files)
      (puthash org-file 1 file-counter)
      (message "Traitement de: %s" org-file)
      (with-current-buffer (find-file-noselect org-file)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^#\\+BEGIN_SRC python\\(.*\\)" nil t)
            (let* ((begin (match-beginning 0))
                   (end (save-excursion
                          (re-search-forward "^#\\+END_SRC" nil t)
                          (point)))
                   (content (buffer-substring-no-properties begin end))
                   (block-num (gethash org-file file-counter))
                   (base-name (file-name-base org-file))
                   (filename (format "%s/%s_%02d.py" output-dir base-name block-num)))
              
              ;; Chercher un titre ou en-tête avant le bloc
              (save-excursion
                (goto-char begin)
                (let ((title (or (and (re-search-backward "^\\*+ " nil t)
                                      (buffer-substring-no-properties
                                       (point)
                                       (line-end-position)))
                                 (and (re-search-backward "^#\\+TITLE:" nil t)
                                      (buffer-substring-no-properties
                                       (point)
                                       (line-end-position)))
                                 (format "bloc_%d" block-num))))
                  (setq title (string-trim (replace-regexp-in-string "^[#*]+ " "" title)))
                  (setq filename (format "%s/%s_%s.py" output-dir base-name 
                                         (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title)))))
              
              ;; Nettoyer le contenu
              (setq content (replace-regexp-in-string "^#\\+BEGIN_SRC python.*\n" "" content))
              (setq content (replace-regexp-in-string "\n#\\+END_SRC$" "" content))
              
              ;; Éviter les doublons de noms
              (when (file-exists-p filename)
                (setq filename (format "%s_%d.py" (file-name-sans-extension filename) block-num)))
              
              ;; Écrire le fichier
              (with-temp-file filename
                (insert (format "# Code extrait de: %s\n" org-file))
                (insert (format "# Bloc numéro: %d\n" block-num))
                (insert "# Généré automatiquement\n\n")
                (insert content))
              
              (message "  Exporté: %s" (file-name-nondirectory filename))
              (puthash org-file (1+ block-num) file-counter)
              (setq counter (1+ counter)))))))
    
    (message "Export terminé! %d fichiers créés dans %s/" (1- counter) output-dir)))

;; Fonction pour exporter depuis le fichier courant uniquement
(defun export-python-blocks-from-current-file ()
  "Exporter tous les blocs Python du fichier Org courant."
  (interactive)
  (let ((output-dir "python_code")
        (counter 1))
    
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+BEGIN_SRC python\\(.*\\)" nil t)
        (let* ((begin (match-beginning 0))
               (end (save-excursion
                      (re-search-forward "^#\\+END_SRC" nil t)
                      (point)))
               (content (buffer-substring-no-properties begin end))
               (filename (format "%s/%s_%02d.py" output-dir 
                                 (file-name-base (buffer-file-name)) 
                                 counter)))
          
          ;; Chercher un titre avant le bloc
          (save-excursion
            (goto-char begin)
            (when (re-search-backward "^\\*+ \\(.*\\)$" nil t)
              (let ((title (match-string 1)))
                (setq filename (format "%s/%s_%s.py" output-dir 
                                       (file-name-base (buffer-file-name))
                                       (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" title))))))
          
          ;; Nettoyer et écrire
          (setq content (replace-regexp-in-string "^#\\+BEGIN_SRC python.*\n" "" content))
          (setq content (replace-regexp-in-string "\n#\\+END_SRC$" "" content))
          
          (with-temp-file filename
            (insert (format "# Code extrait de: %s\n" (buffer-file-name)))
            (insert (format "# Bloc numéro: %d\n" counter))
            (insert "# Généré automatiquement\n\n")
            (insert content))
          
          (message "Exporté: %s" filename)
          (setq counter (1+ counter)))))
    
    (message "Export terminé! %d fichiers créés." (1- counter))))

;; Fonction pour exporter ET exécuter les blocs
(defun export-and-run-python-blocks ()
  "Exporter et exécuter tous les blocs Python."
  (interactive)
  (export-all-python-blocks-from-org)
  (let ((default-directory "python_code/"))
    (shell-command "for f in *.py; do echo \"=== Exécution de $f ===\"; python \"$f\"; done")))

(provide 'export-python-blocks)
