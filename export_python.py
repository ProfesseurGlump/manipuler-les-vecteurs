#!/usr/bin/env python3
"""
Script pour extraire tous les blocs Python des fichiers Org.
Version corrigée pour gérer les indentations.
"""

import os
import re
import sys

def extract_python_blocks_from_org(org_file):
    """Extrait tous les blocs Python d'un fichier Org."""
    with open(org_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Pattern amélioré pour gérer les indentations
    # Cherche #+BEGIN_SRC python avec possibilité d'espaces avant
    pattern = r'^\s*#\+BEGIN_SRC python\s*(?:.*?)\n(.*?)^\s*#\+END_SRC'
    
    blocks = re.findall(pattern, content, re.MULTILINE | re.DOTALL | re.IGNORECASE)
    
    # Alternative: méthode ligne par ligne plus robuste
    if not blocks:
        blocks = extract_python_blocks_line_by_line(content)
    
    return blocks

def extract_python_blocks_line_by_line(content):
    """Extrait les blocs Python en parcourant ligne par ligne."""
    lines = content.split('\n')
    blocks = []
    in_python_block = False
    current_block = []
    
    for line in lines:
        # Détecter le début d'un bloc Python
        if re.match(r'^\s*#\+BEGIN_SRC python', line, re.IGNORECASE):
            in_python_block = True
            current_block = []
            continue
        
        # Détecter la fin d'un bloc
        if in_python_block and re.match(r'^\s*#\+END_SRC', line, re.IGNORECASE):
            in_python_block = False
            if current_block:
                blocks.append('\n'.join(current_block))
            continue
        
        # Ajouter les lignes du bloc
        if in_python_block:
            current_block.append(line)
    
    return blocks

def export_all_python_blocks(org_dir="includes", output_dir="python_code"):
    """Exporte tous les blocs Python de tous les fichiers Org."""
    
    # Créer le dossier de sortie
    os.makedirs(output_dir, exist_ok=True)
    
    # Nettoyer l'ancien contenu
    for file in os.listdir(output_dir):
        if file.endswith('.py'):
            os.remove(os.path.join(output_dir, file))
    
    # Trouver tous les fichiers .org
    org_files = []
    for root, dirs, files in os.walk(org_dir):
        for file in files:
            if file.endswith('.org'):
                org_files.append(os.path.join(root, file))
    
    total_blocks = 0
    for org_file in org_files:
        print(f"Traitement: {os.path.basename(org_file)}")
        blocks = extract_python_blocks_from_org(org_file)
        
        if blocks:
            print(f"  ✓ {len(blocks)} bloc(s) Python trouvé(s)")
        
        for i, block_content in enumerate(blocks, 1):
            total_blocks += 1
            
            # Nettoyer le bloc
            lines = block_content.strip().split('\n')
            
            # Enlever les lignes vides au début et à la fin
            while lines and lines[0].strip() == '':
                lines.pop(0)
            while lines and lines[-1].strip() == '':
                lines.pop()
            
            code = '\n'.join(lines)
            
            # Nom du fichier basé sur le nom du fichier source
            base_name = os.path.splitext(os.path.basename(org_file))[0]
            
            # Créer un nom de fichier sûr
            safe_base = re.sub(r'[^a-zA-Z0-9_]', '_', base_name)
            filename = f"{output_dir}/{safe_base}_bloc{i:03d}.py"
            
            # Éviter les doublons
            counter = 1
            original_filename = filename
            while os.path.exists(filename):
                filename = f"{output_dir}/{safe_base}_bloc{i:03d}_{counter:02d}.py"
                counter += 1
            
            # Écrire le fichier
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(f"# Code extrait de: {os.path.basename(org_file)}\n")
                f.write(f"# Fichier complet: {org_file}\n")
                f.write(f"# Bloc numéro: {i}\n")
                f.write("# Généré automatiquement\n")
                f.write("#" * 60 + "\n\n")
                f.write(code)
            
            print(f"    → {os.path.basename(filename)} ({len(code.splitlines())} lignes)")
    
    print(f"\n" + "="*60)
    print(f"✅ EXPORT TERMINÉ !")
    print(f"   {total_blocks} fichier(s) Python créé(s) dans {output_dir}/")
    print("="*60)
    
    # Lister les fichiers créés
    if total_blocks > 0:
        print("\n📁 Fichiers créés:")
        py_files = sorted([f for f in os.listdir(output_dir) if f.endswith('.py')])
        for py_file in py_files[:10]:  # Afficher les 10 premiers
            print(f"  - {py_file}")
        if len(py_files) > 10:
            print(f"  ... et {len(py_files) - 10} autres")
        
        # Afficher la taille totale
        total_size = sum(os.path.getsize(os.path.join(output_dir, f)) for f in py_files)
        print(f"\n📊 Taille totale: {total_size / 1024:.1f} Ko")

def debug_single_file(filename):
    """Debug un seul fichier pour voir sa structure."""
    print(f"\n🔍 DEBUG du fichier: {filename}")
    with open(filename, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Compter les BEGIN_SRC python
    lines = content.split('\n')
    python_blocks = []
    current_block = []
    in_block = False
    
    print(f"Nombre total de lignes: {len(lines)}")
    
    for j, line in enumerate(lines, 1):
        if re.match(r'^\s*#\+BEGIN_SRC python', line, re.IGNORECASE):
            print(f"\nLigne {j}: DÉBUT DE BLOC PYTHON")
            print(f"  '{line.strip()}'")
            in_block = True
            current_block = [f"# Ligne {j}: {line}"]
        elif in_block and re.match(r'^\s*#\+END_SRC', line, re.IGNORECASE):
            print(f"Ligne {j}: FIN DE BLOC")
            print(f"  '{line.strip()}'")
            in_block = False
            python_blocks.append('\n'.join(current_block))
        elif in_block:
            current_block.append(f"# Ligne {j}: {line}")
    
    print(f"\nNombre de blocs Python trouvés: {len(python_blocks)}")
    
    if python_blocks:
        print("\n🎯 Premier bloc trouvé (20 premières lignes):")
        print("-" * 60)
        first_block_lines = python_blocks[0].split('\n')
        for line in first_block_lines[:20]:
            print(line)
        if len(first_block_lines) > 20:
            print(f"... ({len(first_block_lines) - 20} lignes supplémentaires)")
        print("-" * 60)

if __name__ == "__main__":
    # Pour debug: analyser un fichier spécifique
    if len(sys.argv) > 1 and sys.argv[1] == "--debug":
        if len(sys.argv) > 2:
            debug_single_file(sys.argv[2])
        else:
            # Analyser le premier fichier avec des blocs Python
            org_files = [f for f in os.listdir("includes") if f.endswith('.org')]
            for org_file in org_files:
                full_path = os.path.join("includes", org_file)
                with open(full_path, 'r', encoding='utf-8') as f:
                    if "#+BEGIN_SRC python" in f.read().upper():
                        debug_single_file(full_path)
                        break
    else:
        export_all_python_blocks()
