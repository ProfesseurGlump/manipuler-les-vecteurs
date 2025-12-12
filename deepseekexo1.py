from manim import *

class Exercice1Vecteurs(Scene):
    def construct(self):
        # Titre
        titre = Text("Exercice 1 : Translations et parallélogrammes", font_size=36, color=BLUE)
        self.play(Write(titre))
        self.wait(2)
        self.play(FadeOut(titre))
        
        # Énoncé
        enonce = VGroup(
            Text("Soient les points A, B, C tels que :", font_size=28),
            MathTex(r"\vec{u} = \overrightarrow{AB}"),
            MathTex(r"\vec{v} = \overrightarrow{AC}")
        ).arrange(DOWN, buff=0.5)
        enonce.move_to(UP * 2)
        
        self.play(Write(enonce))
        self.wait(2)
        
        # Création des points A, B, C
        A = Dot(color=RED).shift(LEFT * 3 + DOWN)
        B = Dot(color=BLUE).shift(RIGHT * 2 + DOWN)
        C = Dot(color=GREEN).shift(UP)
        
        # Labels des points
        label_A = Tex("A", font_size=24, color=RED).next_to(A, DOWN)
        label_B = Tex("B", font_size=24, color=BLUE).next_to(B, DOWN)
        label_C = Tex("C", font_size=24, color=GREEN).next_to(C, UP)
        
        # Affichage des points
        self.play(
            FadeIn(A), FadeIn(B), FadeIn(C),
            Write(label_A), Write(label_B), Write(label_C)
        )
        self.wait(1)
        
        # Vecteurs u et v
        vec_u = Arrow(
            start=A.get_center(),
            end=B.get_center(),
            color=YELLOW,
            stroke_width=6,
            buff=0.1
        )
        
        vec_v = Arrow(
            start=A.get_center(),
            end=C.get_center(),
            color=PURPLE,
            stroke_width=6,
            buff=0.1
        )
        
        label_u = MathTex(r"\vec{u}", color=YELLOW).next_to(vec_u, DOWN)
        label_v = MathTex(r"\vec{v}", color=PURPLE).next_to(vec_v, LEFT)
        
        self.play(
            GrowArrow(vec_u),
            GrowArrow(vec_v),
            Write(label_u),
            Write(label_v)
        )
        self.wait(2)
        
        # Question 1: Image de B par translation de vecteur v
        question1 = Text("1. Image de B par translation de vecteur v", font_size=24, color=WHITE)
        question1.to_edge(UP)
        self.play(Transform(enonce, question1))
        self.wait(1)
        
        # Translation de B par v pour obtenir D
        translation_v = Arrow(
            start=B.get_center(),
            end=B.get_center() + (C.get_center() - A.get_center()),
            color=PURPLE_A,
            stroke_width=4,
            buff=0.1
        )
        
        D = Dot(color=ORANGE).move_to(B.get_center() + (C.get_center() - A.get_center()))
        label_D = Tex("D", font_size=24, color=ORANGE).next_to(D, RIGHT)
        
        self.play(
            GrowArrow(translation_v),
            run_time=2
        )
        self.play(FadeIn(D), Write(label_D))
        self.wait(2)
        
        # Vecteur BD = AC
        vec_BD = Arrow(
            start=B.get_center(),
            end=D.get_center(),
            color=PURPLE,
            stroke_width=4,
            buff=0.1
        )
        label_BD = MathTex(r"\overrightarrow{BD} = \vec{v}", color=PURPLE).next_to(vec_BD, RIGHT)
        
        self.play(
            FadeOut(translation_v),
            GrowArrow(vec_BD),
            Write(label_BD)
        )
        self.wait(2)
        
        # Question 2: Image de C par translation de vecteur u
        question2 = Text("2. Image de C par translation de vecteur u", font_size=24, color=WHITE)
        question2.to_edge(UP)
        self.play(Transform(enonce, question2))
        self.wait(1)
        
        # Translation de C par u pour obtenir E
        translation_u = Arrow(
            start=C.get_center(),
            end=C.get_center() + (B.get_center() - A.get_center()),
            color=YELLOW_A,
            stroke_width=4,
            buff=0.1
        )
        
        E = Dot(color=PINK).move_to(C.get_center() + (B.get_center() - A.get_center()))
        label_E = Tex("E", font_size=24, color=PINK).next_to(E, LEFT)
        
        self.play(
            GrowArrow(translation_u),
            run_time=2
        )
        self.play(FadeIn(E), Write(label_E))
        self.wait(2)
        
        # Vecteur CE = AB
        vec_CE = Arrow(
            start=C.get_center(),
            end=E.get_center(),
            color=YELLOW,
            stroke_width=4,
            buff=0.1
        )
        label_CE = MathTex(r"\overrightarrow{CE} = \vec{u}", color=YELLOW).next_to(vec_CE, UP)
        
        self.play(
            FadeOut(translation_u),
            GrowArrow(vec_CE),
            Write(label_CE)
        )
        self.wait(2)
        
        # Question 3: Démontrer que D = E
        question3 = Text("3. Démontrer que D = E", font_size=24, color=WHITE)
        question3.to_edge(UP)
        self.play(Transform(enonce, question3))
        self.wait(1)
        
        # Animation montrant que D et E coïncident
        self.play(
            D.animate.set_color(RED),
            E.animate.set_color(RED),
            label_D.animate.set_color(RED),
            label_E.animate.set_color(RED)
        )
        
        # Formules démonstration
        demo_formulas = VGroup(
            MathTex(r"\overrightarrow{AD} = \overrightarrow{AB} + \overrightarrow{BD}"),
            MathTex(r"\overrightarrow{AD} = \vec{u} + \vec{v}"),
            MathTex(r"\overrightarrow{AE} = \overrightarrow{AC} + \overrightarrow{CE}"),
            MathTex(r"\overrightarrow{AE} = \vec{v} + \vec{u}"),
            MathTex(r"\overrightarrow{AD} = \overrightarrow{AE} \Rightarrow D = E")
        ).arrange(DOWN, buff=0.3).shift(RIGHT * 3)
        
        for formula in demo_formulas:
            self.play(Write(formula))
            self.wait(1)
        
        self.wait(2)
        
        # Question 4: ABDC est un parallélogramme
        question4 = Text("4. ABDC est un parallélogramme", font_size=24, color=WHITE)
        question4.to_edge(UP)
        self.play(
            Transform(enonce, question4),
            FadeOut(demo_formulas)
        )
        self.wait(1)
        
        # Retirer les points E/D en double et garder D
        self.play(
            FadeOut(E),
            FadeOut(label_E),
            FadeOut(vec_CE),
            FadeOut(label_CE)
        )
        
        # Tracer le parallélogramme
        parallelogram = Polygon(
            A.get_center(),
            B.get_center(),
            D.get_center(),
            C.get_center(),
            color=GREEN,
            stroke_width=3
        )
        
        # Égalités vectorielles
        vec_AB = Arrow(A.get_center(), B.get_center(), color=YELLOW, stroke_width=4)
        vec_CD = Arrow(C.get_center(), D.get_center(), color=YELLOW, stroke_width=4)
        vec_AC = Arrow(A.get_center(), C.get_center(), color=PURPLE, stroke_width=4)
        vec_BD = Arrow(B.get_center(), D.get_center(), color=PURPLE, stroke_width=4)
        
        # Mettre en évidence les côtés parallèles
        self.play(
            Create(parallelogram),
            run_time=2
        )
        self.wait(1)
        
        # Animer l'égalité AB = CD
        self.play(
            vec_AB.animate.set_opacity(0.3),
            vec_CD.animate.set_opacity(0.3)
        )
        
        egalite1 = MathTex(r"\overrightarrow{AB} = \overrightarrow{CD}", color=YELLOW)
        egalite1.next_to(parallelogram, UP)
        self.play(Write(egalite1))
        self.wait(1)
        
        # Animer l'égalité AC = BD
        self.play(
            vec_AC.animate.set_opacity(0.3),
            vec_BD.animate.set_opacity(0.3)
        )
        
        egalite2 = MathTex(r"\overrightarrow{AC} = \overrightarrow{BD}", color=PURPLE)
        egalite2.next_to(egalite1, DOWN)
        self.play(Write(egalite2))
        self.wait(2)
        
        # Conclusion
        conclusion = Text("Une seule égalité vectorielle suffit", font_size=24, color=YELLOW)
        conclusion.to_edge(DOWN)
        self.play(Write(conclusion))
        self.wait(2)
        
        # Récapitulatif final
        self.play(
            FadeOut(conclusion),
            FadeOut(egalite1),
            FadeOut(egalite2),
            FadeOut(parallelogram)
        )
        
        recapitulatif = VGroup(
            Text("Récapitulatif :", font_size=30, color=BLUE),
            Text("• Translation de B par v donne D", font_size=24),
            Text("• Translation de C par u donne D", font_size=24),
            Text("• ABDC est un parallélogramme", font_size=24),
            Text("• On a AB = CD et AC = BD", font_size=24)
        ).arrange(DOWN, buff=0.4, aligned_edge=LEFT).scale(0.8)
        
        self.play(Write(recapitulatif))
        self.wait(3)
        
        # Fin
        fin = Text("Fin de l'exercice 1", font_size=36, color=GREEN)
        self.play(
            FadeOut(recapitulatif),
            FadeOut(enonce),
            *[FadeOut(mob) for mob in self.mobjects if mob != fin]
        )
        self.play(Write(fin))
        self.wait(2)
