#*--coding: utf-8 --*
def get_M(a, b, A, B, C):
    """
    Cette fonction prend en entrées :
    + a : 1 float correspondant au coefficient du vecteur AB
    + b : 1 float correspondant au coefficient du vecteur AC
    + A : 1 tuple de float correspondant aux coordonnées du point A
    + B : 1 tuple de float correspondant aux coordonnées du point B
    + C : 1 tuple de float correspondant aux coordonnées du point C
    et elle renvoie 1 tuple de float correspondant aux coordonnées du point M
    """
    x = (1 - a - b) * A[0] + a * B[0] + b * C[0]
    y = (1 - a - b) * A[1] + a * B[1] + b * C[1]
    return (x, y)


def vectAB(A, B):
    """
    Cette fonction prend en entrées :
    + A : 1 tuple de float correspondant aux coordonnées du point A
    + B : 1 tuple de float correspondant aux coordonnées du point B
    et renvoie 1 tuple de float correspondant aux coordonnées du vecteur AB
    """
    x = B[0] - A[0]
    y = B[1] - A[1]
    return (x, y)


# Tests
a, b, A, B, C = 1, 1, (3, 2), (-3, 2), (3, -2)
M = get_M(a, b, A, B, C)
relation = f"On a la relation Vect(A, M) = {a}Vect(A, B) + {b}Vect(A, C)"
print(relation)
input("Pour voir les coordonnées du point M tapez 1\t")
coordM = f"Voici les coordonnées du point M({M[0]}, {M[1]})"
print(coordM)
coordAB = vectAB(A, B)
coordAC = vectAB(A, B = C)
eqX = f"x - {A[0]} = {a} * {coordAB[0]} + {b} * {coordAC[0]}"
input("Pour voir l'équation en x tapez 1\t")
print(eqX)
eqY = f"y - {A[1]} = {a} * {coordAB[1]} + {b} * {coordAC[1]}"
input("Pour voir l'équation en y tapez 1\t")
print(eqY)
solve_x = f"x = {A[0] + a * coordAB[0] + b * coordAC[0]}"
input("Pour voir la solution de l'équation en x tapez 1\t")
print(solve_x)
solve_y = f"y = {A[1] + a * coordAB[1] + b * coordAC[1]}"
input("Pour voir la solution de l'équation en y tapez 1\t")
print(solve_y)
