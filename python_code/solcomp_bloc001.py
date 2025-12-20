# Code extrait de: solcomp.org
# Fichier complet: includes/solcomp.org
# Bloc numéro: 1
# Généré automatiquement
############################################################

y_1 = float(input("Ordonnée du point 1, y_1 = "))
	 x_2 = float(input("Abscisse du point 2, x_2 = "))
	 y_2 = float(input("Ordonnée du point 2, y_2 = "))
	 x_3 = float(input("Abscisse du point 3, x_3 = "))
	 y_3 = float(input("Ordonnée du point 3, y_3 = "))
	 x12 = x_2 - x_1
	 y12 = y_2 - y_1
	 x13 = x_3 - x_1
	 y13 = y_3 - y_1
	 det = x12 * y13 - y12 * x13
	 if det == 0:
	     print("Les points sont alignés")
	     if x13 / x12 < 0:
		 print("Le 3ème point est à gauche du 1er.")
	     elif x13 / x12 > 1:
		 print("Le 3ème point est à droite du 2nd.")
	     else:
		 print("Le 3ème point est entre les 2 premiers.")
	 else:
	     print("Les 3 points forment un triangle.")