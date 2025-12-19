;; Script pour corriger les erreurs d'énumération dans les fichiers Org
(defun fix-org-enumerate-labels ()
  "Corriger les labels d'énumération dans les fichiers Org."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\[label=\\\\alph\\*\\]" nil t)
    (replace-match "[label=\\alph*)]")))
