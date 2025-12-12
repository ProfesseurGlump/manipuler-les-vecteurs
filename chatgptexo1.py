from manim import *

class Exercice1(Scene):
    def construct(self):

        # --- Définition des points de départ ---
        A = Dot(LEFT * 3 + DOWN * 1, color=BLUE)
        B = Dot(LEFT * 1 + DOWN * 1, color=BLUE)
        C = Dot(LEFT * 3 + UP * 1, color=BLUE)

        A_label = MathTex("A").next_to(A, DOWN)
        B_label = MathTex("B").next_to(B, DOWN)
        C_label = MathTex("C").next_to(C, UP)

        self.play(Create(A), Create(B), Create(C),
                  Write(A_label), Write(B_label), Write(C_label))
        self.wait(1)

        # --- Vecteurs de base ---
        u = Arrow(A.get_center(), B.get_center(), buff=0, color=YELLOW)
        v = Arrow(A.get_center(), C.get_center(), buff=0, color=GREEN)

        u_label = MathTex(r"\vec{u}").next_to(u, DOWN)
        v_label = MathTex(r"\vec{v}").next_to(v, LEFT)

        self.play(GrowArrow(u), FadeIn(u_label))
        self.play(GrowArrow(v), FadeIn(v_label))
        self.wait(1)

        # --- Construction du point D = image de B par translation de vecteur AC ---
        BD_vector = Arrow(B.get_center(),
                          B.get_center() + (C.get_center() - A.get_center()),
                          buff=0, color=GREEN)

        D_position = B.get_center() + (C.get_center() - A.get_center())
        D = Dot(D_position, color=RED)
        D_label = MathTex("D").next_to(D, RIGHT)

        self.play(GrowArrow(BD_vector))
        self.play(FadeIn(D), Write(D_label))
        self.wait(1)

        # --- Construction du point E = image de C par translation de vecteur AB ---
        CE_vector = Arrow(C.get_center(),
                          C.get_center() + (B.get_center() - A.get_center()),
                          buff=0, color=YELLOW)

        E_position = C.get_center() + (B.get_center() - A.get_center())
        E = Dot(E_position, color=RED)
        E_label = MathTex("E").next_to(E, UP)

        self.play(GrowArrow(CE_vector))
        self.play(FadeIn(E), Write(E_label))
        self.wait(1)

        # --- Mise en évidence que D = E ---
        self.play(
            Indicate(D, color=RED),
            Indicate(E, color=RED)
        )
        self.wait(1)

        # Ils sont censés être au même endroit, donc on fusionne
        self.play(E.animate.move_to(D.get_center()),
                  FadeOut(E_label))
        self.wait(1)

        # --- Parallélogramme ABDC ---
        parallelogram = Polygon(
            A.get_center(),
            B.get_center(),
            D.get_center(),
            C.get_center(),
            color=ORANGE
        )

        para_label = Text("ABDC est un parallélogramme").next_to(parallelogram, DOWN)

        self.play(Create(parallelogram))
        self.play(Write(para_label))
        self.wait(2)
