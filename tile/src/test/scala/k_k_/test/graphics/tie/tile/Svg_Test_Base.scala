/*
   file: k_k_/test/graphics/tie/tile/Svg_Test_Base.scala

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

import java.io.File

import k_k_.graphics.tie._
import k_k_.graphics.tie.fmt.svg.SvgRenderer
import k_k_.graphics.tie.ink.{NamedColors => C, _}
import k_k_.graphics.tie.shapes._
import k_k_.graphics.tie.shapes.text._


abstract class Svg_Test_Base {

  val filename: String

  val out_dir = "target/test-classes/rendered_out"


  val title: String

  val desc_font = new Font("Arial", 12, weight = FontWeight.Bold)

  val renderer: Renderer = SvgRenderer

  val center_pen = Pen.stroke(C.gray, 0.4)
  val center_X = (Line(10) -%  45) -&
                 (Line(10) -% -45) -~ center_pen

  val shape_pen = Pen.stroke(C.black, 2.0)
  val bbox_pen = Pen.dashed(C.red, 0.8, 10, 0)


  @Test
  def test() {
    val filepath = out_dir + "/" + filename
    val out_file = new File(filepath)
    assertTrue(out_file.getParentFile.isDirectory ||
               out_file.getParentFile.mkdirs)

    val shapes_canvas = create_canvas()

    assertTrue("unable to render to file '" + out_file.getPath + "'",
               renderer.render(shapes_canvas, out_file, true))
  }

  protected def create_canvas(): Canvas

  protected def label_shape(shape: Shape, name: String, ink: Ink): Shape = {
    val name_offset_y = shape.boundingBox.height / 2 + 10
    (shape -~ Pen.stroke(ink)) -&
    (write(name, ink) -+ (0, name_offset_y))

  }

  protected def label_shape(shape: Shape, name: String): Shape =
    label_shape(shape, name, C.Black)

  protected def write(text: String, ink: Ink,
                      align: TextAlign = TextAlign.Middle) =
    TextLine(text, desc_font, align) -~ Pen.fill(ink)
}
