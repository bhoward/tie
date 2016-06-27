/*
   file: k_k_/test/graphics/tie/tile/Svg_Named_Colors_Grid_Test.scala

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
import Assert._

import k_k_.graphics.tie._
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._

import k_k_.graphics.tie.tile._
import k_k_.graphics.tie.tile.conversions._


@Test
class Svg_Named_Colors_Grid_Test extends Svg_Test_Base {

  val filename = "test_all_named_colors.svg"

  val title = "All Named Colors (Clustered)"


  val color_names_fpath =
      "/data/k_k_.graphics.tie/color/clustered_color_names.list"
  // val named_colors = palette.All_Named_Colors.colors
  val named_colors = new palette.ColorPaletteFromColorNameSeq {

    import k_k_.io.data.StringSeqFromDataFile

    protected lazy val colorNameSeq = named_colors_data.getSeq

    private val named_colors_data =
        new StringSeqFromDataFile(color_names_fpath) {
          override protected val loadVia = classOf[Svg_Named_Colors_Grid_Test]
        }
  }.colors



  protected def create_canvas() = {
    val bg_pen = Pen.fill(C.Black)
    val color_tiles = render_colors(named_colors)//.over_bounding_box(bg_pen)

    new Canvas(CanvasProps(1300, 680, title = title),
               color_tiles -@ (0, 0)
              )
  }

  protected def render_colors(colors: Seq[NamedColor]): Shape = {
    val (horiz_pad, vert_pad) = (2.5, 2.5)
    val (n, n_groups) = (colors.length, 14)
    val group_size = n / n_groups + (if (n % n_groups == 0) 0 else 1)
    val color_rows_it = colors.map( render_color(_) ).
                               scale_up_to_uniform.toIterable.
    		               grouped(group_size)
    color_rows_it.map { color_row =>
      color_row.map( _.pad(Center, horiz_pad, vert_pad) ).chain(R_Mid)
    }.chain(Bottom_Middle)
  }

  protected def render_color(color: NamedColor): Shape = {
    val name_font = Font("Arial", 10)
    val text_colors = Seq(C.White, C.Black)
    val name_caption = text_colors.map( TextLine(color.name, name_font, 1.1) -~
    		       			  Pen.fill(_) ).
                                   chain(B_Mid).pad(10) -@ (0, 0)
    val Rectangular(w, h) = name_caption.boundingBox
    Octagon(.8*w, w, .8*h, h) -~ Pen(C.black, color) -& name_caption
  }
}
