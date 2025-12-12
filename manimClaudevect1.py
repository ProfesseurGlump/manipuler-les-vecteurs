from manim import *

config.frame_height = 16.0
config.frame_width = 9.0
config.pixel_height = 1920
config.pixel_width = 1080

# SHORT 1 : Introduction aux vecteurs
class Short1_IntroVecteurs(Scene):
    def construct(self):
        self.add_sound("~/Desktop/mp3-files/demobass.mp3")
        # Titre accrocheur
        titre = Text("C'est quoi un", font_size=45, color=WHITE)
        vecteur = Text("VECTEUR ?", font_size=70, color=YELLOW, weight=BOLD)
        VGroup(titre, vecteur).arrange(DOWN, buff=0.3).move_to(UP * 6)
        
        self.play(FadeIn(titre), run_time=0.5)
        self.play(Write(vecteur), run_time=0.8)
        self.wait(0.5)
        
        # Plan avec repère
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            x_length=7,
            y_length=7,
            tips=False
        ).move_to(DOWN * 1)
        
        self.play(Create(axes), run_time=0.8)
        
        # Points M et M'
        M = Dot(axes.c2p(-1, -0.5), color=RED, radius=0.12)
        M_prime = Dot(axes.c2p(1.5, 1.5), color=RED, radius=0.12)
        
        label_M = MathTex("M", font_size=40, color=RED).next_to(M, DOWN, buff=0.2)
        label_M_prime = MathTex("M'", font_size=40, color=RED).next_to(M_prime, UP, buff=0.2)
        
        self.play(
            FadeIn(M, scale=2),
            Write(label_M),
            run_time=0.6
        )
        self.play(
            FadeIn(M_prime, scale=2),
            Write(label_M_prime),
            run_time=0.6
        )
        self.wait(0.3)
        
        # Vecteur MM'
        vecteur_arrow = Arrow(
            M.get_center(),
            M_prime.get_center(),
            buff=0.1,
            color=YELLOW,
            stroke_width=8,
            max_tip_length_to_length_ratio=0.15
        )
        
        vecteur_label = MathTex(r"\overrightarrow{MM'}", font_size=45, color=YELLOW).next_to(
            vecteur_arrow, RIGHT, buff=0.2
        )
        
        self.play(GrowArrow(vecteur_arrow), run_time=0.8)
        self.play(Write(vecteur_label), run_time=0.5)
        self.wait(0.5)
        
        # Caractéristiques
        carac = VGroup(
            Text("✓ Direction", font_size=32, color=GREEN),
            Text("✓ Sens", font_size=32, color=GREEN),
            Text("✓ Longueur (norme)", font_size=32, color=GREEN)
        ).arrange(DOWN, buff=0.3, aligned_edge=LEFT).to_edge(DOWN, buff=1)
        
        for elem in carac:
            self.play(FadeIn(elem, shift=RIGHT), run_time=0.4)
            self.wait(0.2)
        
        self.wait(1)


# SHORT 2 : Égalité de vecteurs
class Short2_EgaliteVecteurs(Scene):
    def construct(self):
        self.add_sound("~/Desktop/mp3-files/dyinBreed.mp3")
        # Titre
        titre = Text("Vecteurs égaux", font_size=55, color=BLUE, weight=BOLD).move_to(UP * 6.5)
        question = Text("Même flèche ?", font_size=40, color=YELLOW).next_to(titre, DOWN, buff=0.3)
        
        self.play(Write(titre), run_time=0.7)
        self.play(FadeIn(question), run_time=0.5)
        self.wait(0.5)
        
        # Plan
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-4, 4, 1],
            x_length=7,
            y_length=10,
            tips=False
        ).move_to(DOWN * 1.5)
        
        self.play(Create(axes), run_time=0.6)
        
        # Premier vecteur AB
        A = Dot(axes.c2p(-2, 1), color=RED, radius=0.1)
        B = Dot(axes.c2p(0, 2), color=RED, radius=0.1)
        
        vec_AB = Arrow(
            A.get_center(),
            B.get_center(),
            buff=0.1,
            color=YELLOW,
            stroke_width=7,
            max_tip_length_to_length_ratio=0.2
        )
        label_AB = MathTex(r"\overrightarrow{AB}", font_size=38, color=YELLOW).next_to(vec_AB, LEFT, buff=0.2)
        
        self.play(FadeIn(A), FadeIn(B), run_time=0.4)
        self.play(GrowArrow(vec_AB), Write(label_AB), run_time=0.6)
        self.wait(0.4)
        
        # Deuxième vecteur CD (même direction, sens, longueur)
        C = Dot(axes.c2p(0.5, -1.5), color=BLUE, radius=0.1)
        D = Dot(axes.c2p(2.5, -0.5), color=BLUE, radius=0.1)
        
        vec_CD = Arrow(
            C.get_center(),
            D.get_center(),
            buff=0.1,
            color=YELLOW,
            stroke_width=7,
            max_tip_length_to_length_ratio=0.2
        )
        label_CD = MathTex(r"\overrightarrow{CD}", font_size=38, color=YELLOW).next_to(vec_CD, RIGHT, buff=0.2)
        
        self.play(FadeIn(C), FadeIn(D), run_time=0.4)
        self.play(GrowArrow(vec_CD), Write(label_CD), run_time=0.6)
        self.wait(0.5)
        
        # Égalité
        egalite = MathTex(
            r"\overrightarrow{AB} = \overrightarrow{CD}",
            font_size=60,
            color=GREEN
        ).to_edge(DOWN, buff=1.5)
        
        self.play(Write(egalite), run_time=0.8)
        
        # Animation de parallélogramme
        parallelogram = Polygon(
            A.get_center(),
            B.get_center(),
            D.get_center(),
            C.get_center(),
            color=GREEN,
            stroke_width=3
        ).set_fill(GREEN, opacity=0.2)
        
        self.play(Create(parallelogram), run_time=0.8)
        self.wait(0.3)
        
        # Conclusion
        concl = Text("= Même translation !", font_size=35, color=GREEN).next_to(egalite, DOWN, buff=0.4)
        self.play(FadeIn(concl), run_time=0.5)
        
        self.wait(1)


# SHORT 3 : Somme de vecteurs
class Short3_SommeVecteurs(Scene):
    def construct(self):
        # Titre
        titre = Text("Addition", font_size=50, color=WHITE).move_to(UP * 6.8)
        vecteurs = Text("DE VECTEURS", font_size=60, color=YELLOW, weight=BOLD).next_to(titre, DOWN, buff=0.2)
        
        self.play(Write(titre), run_time=0.5)
        self.play(FadeIn(vecteurs, scale=1.3), run_time=0.7)
        self.wait(0.5)
        
        # Plan
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            x_length=7,
            y_length=7,
            tips=False
        ).move_to(DOWN * 1)
        
        self.play(Create(axes), run_time=0.6)
        
        # Points
        A = Dot(axes.c2p(-1.5, -1), color=RED, radius=0.12)
        B = Dot(axes.c2p(0.5, 0.5), color=RED, radius=0.12)
        C = Dot(axes.c2p(2, 1.5), color=RED, radius=0.12)
        
        label_A = MathTex("A", font_size=35).next_to(A, DOWN, buff=0.15)
        label_B = MathTex("B", font_size=35).next_to(B, DOWN, buff=0.15)
        label_C = MathTex("C", font_size=35).next_to(C, UP, buff=0.15)
        
        self.play(
            FadeIn(A, scale=2), Write(label_A),
            run_time=0.4
        )
        
        # Vecteur AB
        vec_AB = Arrow(
            A.get_center(),
            B.get_center(),
            buff=0.12,
            color=BLUE,
            stroke_width=7,
            max_tip_length_to_length_ratio=0.2
        )
        label_AB = MathTex(r"\vec{u}", font_size=40, color=BLUE).next_to(vec_AB, LEFT, buff=0.2)
        
        self.play(
            FadeIn(B, scale=2), Write(label_B),
            GrowArrow(vec_AB),
            run_time=0.6
        )
        self.play(Write(label_AB), run_time=0.4)
        self.wait(0.3)
        
        # Vecteur BC
        vec_BC = Arrow(
            B.get_center(),
            C.get_center(),
            buff=0.12,
            color=GREEN,
            stroke_width=7,
            max_tip_length_to_length_ratio=0.2
        )
        label_BC = MathTex(r"\vec{v}", font_size=40, color=GREEN).next_to(vec_BC, RIGHT, buff=0.2)
        
        self.play(
            FadeIn(C, scale=2), Write(label_C),
            GrowArrow(vec_BC),
            run_time=0.6
        )
        self.play(Write(label_BC), run_time=0.4)
        self.wait(0.5)
        
        # Vecteur AC (somme)
        vec_AC = Arrow(
            A.get_center(),
            C.get_center(),
            buff=0.12,
            color=YELLOW,
            stroke_width=9,
            max_tip_length_to_length_ratio=0.18
        )
        label_AC = MathTex(r"\vec{u} + \vec{v}", font_size=45, color=YELLOW, weight=BOLD).next_to(
            vec_AC, DOWN, buff=0.2
        )
        
        self.play(GrowArrow(vec_AC), run_time=0.8)
        self.play(Write(label_AC), run_time=0.6)
        self.wait(0.5)
        
        # Formule finale
        formule = MathTex(
            r"\overrightarrow{AB} + \overrightarrow{BC} = \overrightarrow{AC}",
            font_size=50,
            color=YELLOW
        ).to_edge(DOWN, buff=1.2)
        
        rect = SurroundingRectangle(formule, color=YELLOW, buff=0.3, corner_radius=0.2)
        
        self.play(Write(formule), run_time=0.8)
        self.play(Create(rect), run_time=0.5)
        
        # Nom
        nom = Text("Relation de Chasles", font_size=32, color=GREEN).next_to(rect, DOWN, buff=0.3)
        self.play(FadeIn(nom), run_time=0.5)
        
        self.wait(1)


# SHORT 4 : Coordonnées d'un vecteur
class Short4_Coordonnees(Scene):
    def construct(self):
        # Titre
        titre = Text("Coordonnées", font_size=52, color=WHITE).move_to(UP * 6.8)
        sous_titre = Text("d'un vecteur", font_size=45, color=YELLOW, weight=BOLD).next_to(titre, DOWN, buff=0.2)
        
        self.play(Write(titre), run_time=0.5)
        self.play(FadeIn(sous_titre), run_time=0.6)
        self.wait(0.5)
        
        # Repère orthonormé
        axes = Axes(
            x_range=[-3, 4, 1],
            y_range=[-2, 4, 1],
            x_length=7,
            y_length=8,
            tips=True,
            axis_config={"include_numbers": True, "font_size": 28}
        ).move_to(DOWN * 0.5)
        
        labels = axes.get_axis_labels(x_label="x", y_label="y")
        
        self.play(Create(axes), Write(labels), run_time=0.8)
        
        # Vecteurs de base
        i_vec = Arrow(
            axes.c2p(0, 0),
            axes.c2p(1, 0),
            buff=0,
            color=RED,
            stroke_width=6,
            max_tip_length_to_length_ratio=0.25
        )
        j_vec = Arrow(
            axes.c2p(0, 0),
            axes.c2p(0, 1),
            buff=0,
            color=BLUE,
            stroke_width=6,
            max_tip_length_to_length_ratio=0.25
        )
        
        label_i = MathTex(r"\vec{i}", font_size=38, color=RED).next_to(i_vec, DOWN, buff=0.15)
        label_j = MathTex(r"\vec{j}", font_size=38, color=BLUE).next_to(j_vec, LEFT, buff=0.15)
        
        self.play(
            GrowArrow(i_vec), Write(label_i),
            run_time=0.5
        )
        self.play(
            GrowArrow(j_vec), Write(label_j),
            run_time=0.5
        )
        self.wait(0.4)
        
        # Vecteur u
        A = Dot(axes.c2p(0.5, 0.5), color=YELLOW, radius=0.1)
        B = Dot(axes.c2p(3, 2.5), color=YELLOW, radius=0.1)
        
        vec_u = Arrow(
            A.get_center(),
            B.get_center(),
            buff=0.1,
            color=YELLOW,
            stroke_width=8,
            max_tip_length_to_length_ratio=0.15
        )
        label_u = MathTex(r"\vec{u}", font_size=42, color=YELLOW).next_to(vec_u, UP, buff=0.15)
        
        self.play(FadeIn(A), FadeIn(B), run_time=0.4)
        self.play(GrowArrow(vec_u), Write(label_u), run_time=0.6)
        self.wait(0.4)
        
        # Projection horizontale
        h_line = DashedLine(
            A.get_center(),
            axes.c2p(3, 0.5),
            color=RED,
            stroke_width=3
        )
        h_arrow = Arrow(
            A.get_center(),
            axes.c2p(3, 0.5),
            buff=0,
            color=RED,
            stroke_width=5,
            max_tip_length_to_length_ratio=0.2
        )
        h_label = MathTex(r"2.5\vec{i}", font_size=32, color=RED).next_to(h_arrow, DOWN, buff=0.1)
        
        self.play(Create(h_line), run_time=0.5)
        self.play(GrowArrow(h_arrow), Write(h_label), run_time=0.5)
        self.wait(0.3)
        
        # Projection verticale
        v_line = DashedLine(
            axes.c2p(3, 0.5),
            B.get_center(),
            color=BLUE,
            stroke_width=3
        )
        v_arrow = Arrow(
            axes.c2p(3, 0.5),
            B.get_center(),
            buff=0,
            color=BLUE,
            stroke_width=5,
            max_tip_length_to_length_ratio=0.2
        )
        v_label = MathTex(r"2\vec{j}", font_size=32, color=BLUE).next_to(v_arrow, RIGHT, buff=0.1)
        
        self.play(Create(v_line), run_time=0.5)
        self.play(GrowArrow(v_arrow), Write(v_label), run_time=0.5)
        self.wait(0.5)
        
        # Coordonnées
        coord = MathTex(
            r"\vec{u} = \begin{pmatrix} 2.5 \\ 2 \end{pmatrix}",
            font_size=55,
            color=YELLOW
        ).to_edge(DOWN, buff=1.5)
        
        box = SurroundingRectangle(coord, color=YELLOW, buff=0.3, corner_radius=0.2, stroke_width=4)
        
        self.play(Write(coord), run_time=0.8)
        self.play(Create(box), run_time=0.5)
        
        self.wait(1)


# SHORT 5 : Colinéarité
class Short5_Colinearite(Scene):
    def construct(self):
        # Titre choc
        titre = Text("Vecteurs", font_size=50, color=WHITE).move_to(UP * 6.8)
        colineaires = Text("COLINÉAIRES", font_size=65, color=RED, weight=BOLD).next_to(titre, DOWN, buff=0.2)
        
        self.play(Write(titre), run_time=0.5)
        self.play(FadeIn(colineaires, scale=1.3), run_time=0.7)
        self.wait(0.5)
        
        # Plan
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[-3, 3, 1],
            x_length=7,
            y_length=7,
            tips=False
        ).move_to(DOWN * 0.5)
        
        self.play(Create(axes), run_time=0.6)
        
        # Vecteur u
        start_u = axes.c2p(-2, -1)
        end_u = axes.c2p(0, 1)
        
        vec_u = Arrow(
            start_u,
            end_u,
            buff=0,
            color=BLUE,
            stroke_width=8,
            max_tip_length_to_length_ratio=0.15
        )
        label_u = MathTex(r"\vec{u}", font_size=45, color=BLUE).next_to(vec_u, LEFT, buff=0.2)
        
        self.play(GrowArrow(vec_u), Write(label_u), run_time=0.7)
        self.wait(0.4)
        
        # Vecteur v (colinéaire = 1.5 * u)
        start_v = axes.c2p(0.5, -0.5)
        end_v = axes.c2p(3.5, 2.5)
        
        vec_v = Arrow(
            start_v,
            end_v,
            buff=0,
            color=GREEN,
            stroke_width=8,
            max_tip_length_to_length_ratio=0.12
        )
        label_v = MathTex(r"\vec{v}", font_size=45, color=GREEN).next_to(vec_v, RIGHT, buff=0.2)
        
        self.play(GrowArrow(vec_v), Write(label_v), run_time=0.7)
        self.wait(0.5)
        
        # Ligne pointillée pour montrer l'alignement
        line = DashedLine(
            axes.c2p(-3, -2),
            axes.c2p(4, 3),
            color=YELLOW,
            stroke_width=3
        )
        self.play(Create(line), run_time=0.6)
        self.wait(0.4)
        
        # Définition
        definition = Text("= Même direction", font_size=40, color=YELLOW).move_to(UP * 4)
        self.play(FadeIn(definition, shift=DOWN), run_time=0.6)
        self.wait(0.5)
        
        # Formule mathématique
        formule = MathTex(
            r"\vec{v} = k \cdot \vec{u}",
            font_size=55,
            color=YELLOW
        ).to_edge(DOWN, buff=2)
        
        self.play(Write(formule), run_time=0.7)
        self.wait(0.4)
        
        # Exemple avec k
        exemple = MathTex(
            r"k = 1.5",
            font_size=45,
            color=GREEN
        ).next_to(formule, DOWN, buff=0.4)
        
        self.play(FadeIn(exemple), run_time=0.5)
        self.wait(0.3)
        
        # Critère avec déterminant
        critere = MathTex(
            r"\text{det}(\vec{u}, \vec{v}) = 0",
            font_size=40,
            color=RED
        ).to_edge(DOWN, buff=0.8)
        
        box = SurroundingRectangle(critere, color=RED, buff=0.25, corner_radius=0.15)
        
        self.play(Write(critere), Create(box), run_time=0.7)
        
        self.wait(1)


# SHORT 6 : Calculer la norme
class Short6_Norme(Scene):
    def construct(self):
        # Titre
        titre = Text("NORME", font_size=70, color=YELLOW, weight=BOLD).move_to(UP * 6.8)
        sous_titre = Text("= Longueur du vecteur", font_size=38, color=WHITE).next_to(titre, DOWN, buff=0.3)
        
        self.play(FadeIn(titre, scale=1.5), run_time=0.7)
        self.play(Write(sous_titre), run_time=0.6)
        self.wait(0.5)
        
        # Repère
        axes = Axes(
            x_range=[0, 5, 1],
            y_range=[0, 4, 1],
            x_length=7,
            y_length=6,
            tips=True,
            axis_config={"include_numbers": True, "font_size": 26}
        ).move_to(DOWN * 1.2)
        
        self.play(Create(axes), run_time=0.7)
        
        # Points A et B
        A = Dot(axes.c2p(1, 0.5), color=RED, radius=0.12)
        B = Dot(axes.c2p(4, 3.5), color=RED, radius=0.12)
        
        label_A = MathTex("A(1; 0.5)", font_size=32).next_to(A, DOWN, buff=0.2)
        label_B = MathTex("B(4; 3.5)", font_size=32).next_to(B, UP, buff=0.2)
        
        self.play(
            FadeIn(A, scale=2), Write(label_A),
            FadeIn(B, scale=2), Write(label_B),
            run_time=0.7
        )
        self.wait(0.4)
        
        # Vecteur AB
        vec_AB = Arrow(
            A.get_center(),
            B.get_center(),
            buff=0.12,
            color=YELLOW,
            stroke_width=9,
            max_tip_length_to_length_ratio=0.15
        )
        label_vec = MathTex(r"\overrightarrow{AB}", font_size=45, color=YELLOW).next_to(vec_AB, LEFT, buff=0.2)
        
        self.play(GrowArrow(vec_AB), Write(label_vec), run_time=0.7)
        self.wait(0.5)
        
        # Triangle rectangle pour Pythagore
        triangle = Polygon(
            A.get_center(),
            axes.c2p(4, 0.5),
            B.get_center(),
            color=BLUE,
            stroke_width=3
        ).set_fill(BLUE, opacity=0.2)
        
        self.play(Create(triangle), run_time=0.6)
        self.wait(0.3)
        
        # Dimensions
        dx = MathTex(r"\Delta x = 3", font_size=35, color=GREEN).next_to(
            axes.c2p(2.5, 0.5), DOWN, buff=0.2
        )
        dy = MathTex(r"\Delta y = 3", font_size=35, color=GREEN).next_to(
            axes.c2p(4, 2), RIGHT, buff=0.2
        )
        
        self.play(Write(dx), Write(dy), run_time=0.6)
        self.wait(0.5)
        
        # Formule de la norme
        formule = MathTex(
            r"\|\overrightarrow{AB}\| = \sqrt{\Delta x^2 + \Delta y^2}",
            font_size=48,
            color=YELLOW
        ).move_to(DOWN * 5.5)
        
        self.play(Write(formule), run_time=0.8)
        self.wait(0.5)
        
        # Calcul
        calcul = MathTex(
            r"= \sqrt{3^2 + 3^2} = \sqrt{18}",
            font_size=45,
            color=ORANGE
        ).next_to(formule, DOWN, buff=0.3)
        
        self.play(Write(calcul), run_time=0.7)
        self.wait(0.4)
        
        # Résultat simplifié
        resultat = MathTex(
            r"= 3\sqrt{2} \approx 4.24",
            font_size=50,
            color=GREEN,
            weight=BOLD
        ).next_to(calcul, DOWN, buff=0.3)
        
        box = SurroundingRectangle(resultat, color=GREEN, buff=0.25, corner_radius=0.15)
        
        self.play(Write(resultat), Create(box), run_time=0.7)
        
        self.wait(1)
