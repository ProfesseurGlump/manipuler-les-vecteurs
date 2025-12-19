#!/usr/bin/env python3
"""
Script pour extraire tous les blocs Python des fichiers Org.
"""

import os
import re
import sys

def extract_python_blocks_from_org(org_file):
    """Extrait tous les blocs Python d'un fichier Org."""
    with open(org_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Pattern pour trouver les blocs Python
    pattern = r'^#\+BEGIN_SRC python(.*?)^#\+END_SRC'
    blocks = re.findall(pattern, content, re.MULTILINE | re.DOTALL)
    
    return blocks

def export_all_python_blocks(org_dir="includes", output_dir="python_code"):
    """Exporte tous les blocs Python de tous les fichiers Org."""
    
    # Créer le dossier de sortie
    os.makedirs(output_dir, exist_ok=True)
    
    # Trouver tous les fichiers .org
    org_files = []
    for root, dirs, files in os.walk(org_dir):
        for file in files:
            if file.endswith('.org'):
                org_files.append(os.path.join(root, file))
    
    counter = 1
    for org_file in org_files:
        print(f"Traitement: {org_file}")
        blocks = extract_python_blocks_from_org(org_file)
        
        for i, block in enumerate(blocks, 1):
            # Nettoyer le bloc
            lines = block.strip().split('\n')
            # Enlever la première ligne (qui contient les options)
            if lines and lines[0].startswith('python'):
                lines = lines[1:]
            
            code = '\n'.join(lines)
            
            # Nom du fichier
            filename = f"{output_dir}/prog{counter:03d}.py"
            
            # Écrire le fichier
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(f"# Code extrait de: {org_file}\n")
                f.write(f"# Bloc: {i}\n")
                f.write("# Généré automatiquement\n\n")
                f.write(code)
            
            print(f"  Exporté: {filename}")
            counter += 1
    
    print(f"\nExport terminé! {counter-1} fichiers créés dans {output_dir}/")

if __name__ == "__main__":
    export_all_python_blocks()
