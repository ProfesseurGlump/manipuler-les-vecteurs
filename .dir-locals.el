;; Configuration locale pour le projet vecteurs

((nil . ((eval . (progn
                   ;; Ajouter le répertoire texlive au PATH
                   (add-to-list 'exec-path "/usr/local/texlive/2024/bin/universal-darwin")
                   
                   ;; Configuration LaTeX spécifique au projet
                   (setq org-latex-pdf-process
                         '("lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
                           "lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"
                           "lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
                   
                   ;; Désactiver la confirmation pour l'exécution des blocs de code
                   (setq org-confirm-babel-evaluate nil)
                   
                   ;; Activer l'évaluation automatique des blocs Python
                   (org-babel-do-load-languages
                    'org-babel-load-languages
                    '((python . t)))
                   
                   ;; Compiler automatiquement à la sauvegarde
                   (add-hook 'after-save-hook
                             (lambda ()
                               (when (and (buffer-file-name)
                                          (string= (file-name-nondirectory (buffer-file-name))
                                                   "solutions-mvp.org"))
                                 (my/compile-vecteurs-project)))
                             nil t)))))

 (org-mode . ((org-startup-folded . content)
              (org-startup-with-inline-images . t)
              (org-image-actual-width . 500)
              (eval . (visual-line-mode 1))))

 (python-mode . ((python-shell-interpreter . "python3")
                 (python-indent-offset . 4)
                 (eval . (progn
                          ;; Activer le mode linter pour Python
                          (add-hook 'python-mode-hook 'flycheck-mode)
                          ;; Auto-complétion pour Python
                          (add-hook 'python-mode-hook 'company-mode))))))
