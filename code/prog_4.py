base = """On dit que les vecteurs u et v forment une base s'ils ne sont
pas colinéaires.
Algérbiquement : il n'existe aucun réel k tel que u = kv.
Géométriquement : leurs directions sont des droites sécantes.
Rapidement : det(u, v) != 0.\n
"""

draw_base = """
   ^
  / v
 /
/       (u, v) forme une base
x---------------------------->
	       u
"""

orthonormal = """Ortho vient du grec pour dire droit donc ici qui forme un
angle droit (directions orthogonales). Normal pour norme, ici de même
norme.\n
"""

draw_orthornormal_base = """
^
|    i et j sont orthogonaux
|    ||i|| = ||j|| ont même norme
|    
^    (i, j) forme une base orthonormée
| j
------0->---------------------------------->
i
"""

relation = """Si (i, j) est une base orthonormale alors tout vecteur
u = a * i + b * j a pour coordonnées (a, b).\n
Par exemple le vecteur horizontal v_1 de coordonnées (2, 0) s'écrit :
v_1 = 2 * i + 0 * j.\n
Par exemple le vecteur vertical v_2 de coordonnées (0, 3) s'écrit :
v_2 = 0 * i + 3 * j.\n
Par exemple le vecteur diagonal v_3 de coordonnées (4, 4) s'écrit :
v_3 = 4 * i + 4 * j.\n
Si le vecteurs v_4 = 2 * v_1 + v_2 alors v_4 = 4 * i + 3 * j
a pour coordonnées : (4, 3).\n
"""

v1 = """
^
|
| --> v_1 = 2 * i
| -> i
|
0->-------->
i
"""

v2 = """
  ^       ^
  |       |
  |    ^  |
  |    |  |
  ^    j  v_2 = 3 * j
j |   
0----------------->

"""



v3 = """
^  v_3 = 4 * i + 4 * j    
|
8      ^ (7, 8)
| v_3 /|
6    / | 4 * j 
|   /  |
4--x---> (7, 4)  
|  |4i |        
2  |   |        
|  |   |        
0--3---7----->
"""

v4 = """
  ^      v_4 = 4 * i + 3 * j
  |     
  ^    /^    
  |   / |
  |  /  | v_2 = 3 * j
  ^ /   ^
j |/    |
  0->--->----------------->
   i
        2 * v_1 = 4 * i
"""

vectors = [v1, v2, v3, v4]

norme = """Un vecteur u = a * i + b * j de coordonnées (a, b) a pour norme
||u|| = sqrt{a^2 + b^2} = (a^2 + b^2) ** 0.5\n
ou dit autrement ||u||^2 = a^2 + b^2\n
Par exemple le vecteur horizontal v_1 de coordonnées (2, 0) a pour norme :
||v_1|| = sqrt{2^2 + 0^2} = sqrt{2^2} = 2.\n
Par exemple le vecteur vertical v_2 de coordonnées (0, 3) a pour norme :
||v_2|| = sqrt{0^2 + 3^2} = sqrt{3^2} = 3.\n 
Par exemple le vecteur diagonal v_3 de coordonnées (5, 5) a pour norme :
||v_3|| = sqrt{5^2 + 5^2} = sqrt{2 * 5^2} = 5 * sqrt{2}.\n
Si le vecteur v_4 = 2 * v_1 + v_2 alors v_4 = 4 * i + 3 * j a pour norme :
||v_4|| = sqrt{4^2 + 3^2} = sqrt{16 + 9} = sqrt{25} = sqrt{5^2} = 5.\n
"""


menu = """
MENU
1) Définition d'une base orthornomée.
2) Relation vectorielle entre coordonnées et vecteurs de la base.
3) Formule de calcul de la norme.
0) Quitter.
"""
titles = [
    "1) Définition d'une base orthonormée",
    "2) Relation vectorielle coordonnées base",
    "3) Norme d'un vecteur"
]
underlines = ["-" * len(t) for t in titles]
continuer = True
while continuer:
    choix = int(input(menu + "\nVotre choix : "))
    if choix == 1:
	print()
	title1 = titles[choix - 1]
	underline1 = underlines[choix - 1]
	print(title1)
	print(underline1)
	print()
	print("Une base orthonormée est une base de vecteurs orthonormaux.")
	print("base :", base)
	draw = int(input("Taper 1 afficher le dessin\n"))
	print(draw_base)
	print("orthonormal :", orthonormal)
	draw = int(input("Taper 1 afficher le dessin\n"))
	print(draw_orthornormal_base)
        

    elif choix == 2:
	print()
	title2 = titles[choix - 1]
	underline2 = underlines[choix - 1]
	print(title2)
	print(underline2)
	print()
	print(relation)
        
	for i in range(len(vectors)):
	    msg = f"Taper {i + 1} pour afficher le vecteur v_{i + 1} : "
	    dessin = int(input(msg))
	    if dessin == i + 1:
		print(vectors[i])

    elif choix == 3:
	print()
	title3 = titles[choix - 1]
	underline3 = underlines[choix - 1]
	print(title3)
	print(underline3)
	print()
	print(norme)

        
        def ask_coord(axe, c):
	    msg = " du vecteur dont vous voulez calculer la norme "
	    msg = f"{axe} {msg} {c} = "
	    return msg

        
	x = float(input(ask_coord("Abscisse", "x")))
	y = float(input(ask_coord("Ordonnée", "y")))
	n = (x ** 2 + y ** 2) ** 0.5
	print(f"Norme à 2 décimales près n = {n:.2f}")

    elif choix == 0:
	continuer = False
