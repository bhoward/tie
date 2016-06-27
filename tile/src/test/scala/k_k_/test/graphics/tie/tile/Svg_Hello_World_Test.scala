/*
   file: k_k_/test/graphics/tie/tile/Svg_Hello_World_Test.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.test.graphics.tie.tile

import org.junit._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.path._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Hello_World_Test extends Svg_Test_Base {

  val filename = "test_hello_world_example.svg"

  val title = "Hello World!"

  val (img_orig_dir_root, img_mapped_dir_root) = ("src/test/resources", "..")
  val img_path_mapper = (s: String) =>
                          s.replace(img_orig_dir_root, img_mapped_dir_root)

  val earth_img_path = img_orig_dir_root + "/images/earth_centered_w_africa.png"


  protected def create_canvas() = {
    val (orange, blue) = (C.Orange, C.RoyalBlue)
    val lin_grad = LinearGradient.uniform(Seq(C.orange, C.RoyalBlue))
    val rad_grad = RadialGradient(Seq(ColorStop(C.Orange,    15),
                                       ColorStop(C.RoyalBlue, 25)),
                                   ColorSpread.Repeat)

    val stripe = Rectangle(18, 80)
    val earth_img = Image(earth_img_path, img_path_mapper)

    val H_vert_bar  = stripe -~ Pen.fill(orange)
    val H_horiz_bar = stripe -% 90 -* (.5, 1) -~ Pen.fill(blue)
    val H_vert_horiz = (Left_Middle of H_horiz_bar).combo(H_vert_bar)


    val h_ello  = (Center of H_vert_bar).combo(R_Mid of H_vert_horiz)
    val h_e_llo = Path.from(-14, 0).
                    horiz(29).
                    arc(15, ArcChoice.LargeCCW, -5, 12) -* 1.4 -~
                      Pen.stroke(lin_grad -% 60, 10, StrokeEnds.Round)
    val he_l_lo = stripe -~ Pen.fill(lin_grad -% 90)
    val hel_l_o = stripe -~ Pen.fill(lin_grad)
    val hell_o  = (Circle(25).asPath & (DiamCircle(20) -+ (25, 0))) -~
                     Pen.fill(Pattern(Square(4) -% 45 -+ 2 -~
                                        Pen.fill(orange)),
                              FillRule.EvenOdd)

    val W_body  = Trapezoid(50, 70, 80) -% 180 -~ Pen.fill(orange)

    val stalagmites = {
          val stalagmite = IsoTriangle(14, 50) -% 180
          val stalagtite = Trapezoid(10, 20, 20)
          (stalagmite -+ (-12, 0)) -&
          (stalagmite -+ ( 12, 0)) -& (stalagtite -+ (0, -15))
    } -~ Pen.fill(blue)
    val lower_cleft = IsoTriangle(20, 40) -~ Pen.fill(blue)
    val w_orld  = W_body -& (lower_cleft.to(B_Mid of W_body, Inside)
                         -&  stalagmites.to(T_Mid of W_body, Inside))

    val w_o_rld = earth_img -* .25 -<> Rectangle(55, 55)
    // NOTE: use Text_Line.breadth = 1/100 to compensate for 'descender hang'
    val wo_r_ld = TextLine("r", Font("Times New Roman", 120), .01) -~
                    Pen.fill(blue)
    val wor_l_d = stripe -~ Pen.fill(rad_grad)
    val worl_d  = FreeForm(Path.
                              horiz(12).
                              vert(80).
                              horiz(-12).
                              arc(24, 22, ArcChoice.LargeCW, 0, -44).
                              close
                              &
                            Path.from(3, 48).
                              arc(12, ArcChoice.LargeCCW, 0, 22).
                              close) -~ Pen(blue, orange) pad(L_Mid, 25, 0)

    val kerning = 4
    val hello_letters = Seq(h_ello,
                            // compensate for imprecise Free_Form.bounding_box:
                            h_e_llo.pad(T_Mid, 0, 25),
                            he_l_lo, hel_l_o,
                            hell_o.pad(0, 25))
    val world_letters = Seq(w_orld, w_o_rld, wo_r_ld, wor_l_d, worl_d)
    //              map ( _.under_bounding_box(Pen.stroke(C.red).dashed(10)) )

    val hello = hello_letters.map { _.pad(kerning/2, 0) }.
                              chain(Right_Middle, CCW_Twist).recenter
    val world = world_letters.map {  _.pad(kerning/2, 0) }.
                              chain(Right_Middle, CCW_Twist).recenter

    val hello_world = (B_Mid of hello.pad(B_Mid, 0, 30)).combo(world, Outside)
    /*  equivalent phrasing:
    val hello_world = hello -&
                      world.to(hello.pad(0, 30), Bottom_Middle, Outside)
    */

    new Canvas(CanvasProps(325, 325, title = title),
               hello_world -@ (0, 0) -& (earth_img -* 1.5 -+ (0, -7) -# .20)
              )
  }
}
