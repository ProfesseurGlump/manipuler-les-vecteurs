x_u = float(input("Abscisse du 1er vecteur = "))
y_u = float(input("Ordonnée du 1er vecteur = "))
x_v = float(input("Abscisse du 2e vecteur = "))
y_v = float(input("Ordonnée du 2e vecteur = "))
d = x_u * y_v - x_v * y_u
if d == 0:
    print("Les vecteurs sont colinéaires.")
    k = x_v / x_u
    print(f"Vecteur 2 = {k} * Vecteur 1")
else:
    print("Les vecteurs ne sont pas colinéaires.")
    print("Ils forment donc une base.")
