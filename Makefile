# Makefile pour le projet vecteurs
# Usage: 
#   make pdf      - Génère le PDF
#   make clean    - Nettoie les fichiers temporaires
#   make all      - Génère tout
#   make help     - Affiche l'aide

.PHONY: all pdf clean help test check

# Variables
ORG_FILE = solutions-mvp.org
PDF_NAME = solutions-mvp.pdf
BUILD_DIR = build
CODE_DIR = code

# Couleurs pour les messages
GREEN = \033[0;32m
RED = \033[0;31m
YELLOW = \033[0;33m
NC = \033[0m

# Cible principale
all: pdf

# Générer le PDF
pdf: check-org
	@echo "$(GREEN)Compilation du document Org en PDF...$(NC)"
	@mkdir -p $(BUILD_DIR)
	emacs --batch $(ORG_FILE) \
	  --eval "(org-latex-export-to-pdf nil t t t t)" \
	  2>/dev/null
	@if [ -f $(PDF_NAME) ]; then \
	  mv $(PDF_NAME) $(BUILD_DIR)/; \
	  echo "$(GREEN)PDF généré: $(BUILD_DIR)/$(PDF_NAME)$(NC)"; \
	else \
	  echo "$(RED)Échec de la génération du PDF$(NC)"; \
	  exit 1; \
	fi

# Vérifier les fichiers Python
check:
	@echo "$(YELLOW)Vérification des programmes Python...$(NC)"
	@for pyfile in $(CODE_DIR)/*.py; do \
	  if python3 -m py_compile $$pyfile 2>/dev/null; then \
	    echo "$(GREEN)✓ $$pyfile$(NC)"; \
	  else \
	    echo "$(RED)✗ $$pyfile$(NC)"; \
	  fi; \
	done

# Exécuter tous les programmes
test:
	@echo "$(YELLOW)Exécution des programmes Python...$(NC)"
	@cd $(CODE_DIR) && for pyfile in prog_*.py; do \
	  echo "\n=== $$pyfile ==="; \
	  python3 $$pyfile 2>&1; \
	done

# Vérifier que le fichier Org existe
check-org:
	@if [ ! -f "$(ORG_FILE)" ]; then \
	  echo "$(RED)Erreur: $(ORG_FILE) n'existe pas$(NC)"; \
	  exit 1; \
	fi

# Nettoyer les fichiers temporaires
clean:
	@echo "$(YELLOW)Nettoyage...$(NC)"
	@rm -rf $(BUILD_DIR) *.aux *.log *.toc *.out *.tex \
	        *.pdf *.bbl *.blg *.synctex.gz *.fdb_latexmk \
	        *.fls *.pyc __pycache__

# Afficher l'aide
help:
	@echo "Commandes disponibles:"
	@echo "  make pdf     - Génère le PDF principal"
	@echo "  make check   - Vérifie la syntaxe Python"
	@echo "  make test    - Exécute tous les programmes"
	@echo "  make clean   - Nettoie les fichiers temporaires"
	@echo "  make all     - Génère tout (pdf + check)"
	@echo "  make help    - Affiche cette aide"

# Alias
compile: pdf
verify: check
run: test
