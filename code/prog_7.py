def abc_aligned():
    points = []

    for i in range(3):
	x = float(input(f"Abscisse du {i + 1}e point = "))
	y = float(input(f"Ordonnée du {i + 1}e point = "))
	points.append((x, y))

    x_p1p2 = points[1][0] - points[0][0]
    y_p1p2 = points[1][1] - points[0][1]
    x_p1p3 = points[2][0] - points[0][0]
    y_p1p3 = points[2][1] - points[0][1]

    det = x_p1p2 * y_p1p3 - x_p1p3 * y_p1p2

    if det == 0:
	return True
    else:
	return False 


# Test
if abc_aligned():
    print("Les points sont alignés.")
else:
    print("Les points ne sont pas alignés.")
