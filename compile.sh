#!/bin/bash
# compile.sh
cd /chemin/vers/projet-vecteurs
emacs -Q --batch \
  --eval "(require 'org)" \
  --eval "(setq org-latex-pdf-process '(\"lualatex -shell-escape -interaction=nonstopmode -output-directory=output %f\"))" \
  --eval "(org-latex-export-to-pdf)" \
  solutions-mvp.org
