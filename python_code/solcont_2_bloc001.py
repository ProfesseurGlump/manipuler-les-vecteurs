# Code extrait de: solcont_2.org
# Fichier complet: includes/solcont_2.org
# Bloc numéro: 1
# Généré automatiquement
############################################################

rep = "\n(O/N) "
     msg += rep
     egal = input(msg)
     if egal.upper() == "O":
	 msg = "Les vecteurs sont-ils alignés ?"
	 align = input(msg)
	 if align.upper() == "N":
	     print("C'est un parallélogramme.")
	 else:
	     print("C'est le même vecteur.")
     else:
	 print("Les vecteurs ne sont pas colinéaires.")