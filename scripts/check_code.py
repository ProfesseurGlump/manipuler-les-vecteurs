#!/usr/bin/env python3
"""
Vérificateur de code pour le projet vecteurs
"""

import os
import subprocess
import sys
from pathlib import Path

CODE_DIR = Path("code")
REQUIRED_PROGRAMS = [
    "prog_1.py",
    "prog_2.py", 
    "prog_3.py",
    "prog_4.py",
    "prog_5.py",
    "prog_6.py",
    "prog_7.py"
]

def check_syntax(file_path):
    """Vérifie la syntaxe d'un fichier Python"""
    try:
        subprocess.run(
            [sys.executable, "-m", "py_compile", str(file_path)],
            check=True,
            capture_output=True
        )
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, e.stderr.decode()

def check_all():
    """Vérifie tous les fichiers"""
    print("🔍 Vérification des programmes Python...\n")
    
    all_ok = True
    
    for prog in REQUIRED_PROGRAMS:
        file_path = CODE_DIR / prog
        if not file_path.exists():
            print(f"❌ Fichier manquant: {prog}")
            all_ok = False
            continue
            
        ok, error = check_syntax(file_path)
        if ok:
            print(f"✅ {prog:15} Syntaxe OK")
        else:
            print(f"❌ {prog:15} Erreur de syntaxe")
            print(f"   {error}")
            all_ok = False
    
    print(f"\n{'✅ Tout est OK' if all_ok else '❌ Problèmes détectés'}")
    return all_ok

if __name__ == "__main__":
    os.chdir(Path(__file__).parent.parent)
    sys.exit(0 if check_all() else 1)
