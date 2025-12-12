from manim import *

class Video1_NaissanceVecteur(Scene):
    def construct(self):

        self.add_sound("~/Desktop/mp3-files/demobass.mp3")

        # --------------------------
        # Titre
        # --------------------------
        title = Text("Naissance d’un vecteur : de la translation au vecteur", font_size=36)
        self.play(FadeIn(title))
        self.wait(2)
        self.play(FadeOut(title))

        # --------------------------
        # Points M et M'
        # --------------------------
        M = Dot(LEFT*3 + DOWN*1, color=BLUE)
        M_label = Text("M", font_size=32).next_to(M, DOWN)
        
        Mp = Dot(RIGHT*2 + UP*1, color=RED)
        Mp_label = Text("M'", font_size=32).next_to(Mp, UP)

        self.play(FadeIn(M), FadeIn(M_label))
        self.wait(1)
        self.play(FadeIn(Mp), FadeIn(Mp_label))
        self.wait(1)

        # --------------------------
        # Translation (flèche animée)
        # --------------------------
        arrow = Arrow(M.get_center(), Mp.get_center(), buff=0, color=YELLOW)
        text_trans = Text("Translation", font_size=32).to_edge(UP)

        self.play(GrowArrow(arrow), FadeIn(text_trans))
        self.wait(2)
        self.play(FadeOut(text_trans))

        # --------------------------
        # Vecteur MM'
        # --------------------------
        vect_label = MathTex(r"\overrightarrow{MM'}", font_size=40).next_to(arrow, UP)
        self.play(Write(vect_label))
        self.wait(2)

        # --------------------------
        # Caractéristiques direction / sens / norme
        # --------------------------
        charac = VGroup(
            Text("Direction", font_size=32),
            Text("Sens", font_size=32),
            Text("Norme", font_size=32),
        ).arrange(DOWN).to_edge(RIGHT)

        charac_title = Text("Caractéristiques :", font_size=32).to_edge(RIGHT).shift(UP*1.5)

        self.play(FadeIn(charac_title))
        self.play(LaggedStart(*[FadeIn(t) for t in charac], lag_ratio=0.3))
        self.wait(3)

        # --------------------------
        # "Glisser" le vecteur
        # --------------------------
        arrow_copy = arrow.copy()
        arrow_copy.shift(DOWN*2 + LEFT*1)
        vect_label_copy = vect_label.copy().next_to(arrow_copy, UP)

        glisser_text = Text("On peut déplacer un vecteur", font_size=32).to_edge(UP)

        self.play(FadeIn(glisser_text))
        self.wait(1)
        self.play(FadeIn(arrow_copy), FadeIn(vect_label_copy))
        self.wait(2)
        self.play(FadeOut(glisser_text))

        # --------------------------
        # Vecteur nul (animation progressive)
        # --------------------------

        self.play(FadeOut(arrow_copy), FadeOut(vect_label_copy))
        vect_nul_text = Text("Vecteur nul : M = M'", font_size=32).to_edge(UP)
        self.play(FadeIn(vect_nul_text))

        # Nouveau point M' à animer
        Mp_anim = Dot(Mp.get_center(), color=RED)
        Mp_label_anim = Text("M'", font_size=32).next_to(Mp_anim, UP)

        self.play(FadeIn(Mp_anim), FadeIn(Mp_label_anim))

        # Vecteur qui va se rétrécir
        arrow_anim = always_redraw(
        lambda: Arrow(
            start=M.get_center(),
            end=Mp_anim.get_center(),
            buff=0,
            color=YELLOW
            )
        )
        self.add(arrow_anim)

        # Animation progressive : M' glisse vers M
        self.play(
            Mp_anim.animate.move_to(M.get_center()),
            Mp_label_anim.animate.move_to(M.get_center() + UP*0.3),
            run_time=3
        )

        # À la fin, M' se confond avec M
        final_dot = Dot(M.get_center(), color=PURPLE)
        self.play(
            FadeOut(arrow_anim),
            Transform(Mp_anim, final_dot),
            FadeOut(Mp_label_anim),
            run_time=1
        )

        self.wait(1)
        self.play(FadeOut(vect_nul_text))


        
