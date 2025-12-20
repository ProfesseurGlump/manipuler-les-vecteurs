#!/bin/bash
# export_python.sh - Script bash simple

OUTPUT_DIR="python_code"
mkdir -p "$OUTPUT_DIR"

# Compter les blocs
echo "🔍 Recherche de blocs Python dans includes/..."

# Utiliser awk pour extraire proprement
find includes -name "*.org" -exec grep -l "#+BEGIN_SRC python" {} \; | while read org_file; do
    filename=$(basename "$org_file" .org)
    echo "📄 Traitement: $filename.org"
    
    # Extraire les blocs avec awk
    awk -v file="$filename" -v outdir="$OUTPUT_DIR" '
    /^[[:space:]]*#\+BEGIN_SRC python/ {
        block = ""
        in_block = 1
        block_num++
        next
    }
    /^[[:space:]]*#\+END_SRC/ {
        if (in_block) {
            # Nettoyer le bloc
            gsub(/^\n+|\n+$/, "", block)
            if (block != "") {
                # Créer le nom de fichier
                output = outdir "/" file "_" sprintf("%03d", block_num) ".py"
                
                # Écrire le fichier
                print "# Extracted from: " FILENAME > output
                print "# Block: " block_num > output
                print "" > output
                print block > output
                
                print "  → " file "_" sprintf("%03d", block_num) ".py"
            }
            in_block = 0
            block = ""
        }
        next
    }
    in_block {
        block = block $0 "\n"
    }
    ' "$org_file"
done | head -20

echo "✅ Fait !"
