/*
   file: k_k_/graphics/tie/tile/pos/Shape_Pos.scala

   Tie - Tie Illustrates Everything.

     http://www.tie-illustrates-everything.com/

   Copyright (c)2010-2012 by Corbin "Kip" Kohn
   All Rights Reserved.

   Please reference the following for applicable terms, conditions,
   and licensing:

     http://tie-illustrates-everything.com/licensing
*/

package k_k_.graphics.tie.tile

package pos {

import k_k_.graphics.tie.shapes.{Bounding_Boxed, Line, Point, Shape}
import k_k_.graphics.tie.shapes.path.Path

import conversions._


final class Positionable_Shape(self: Shape) {

  def at(where: Bounding_Box_Pos): Shape_Pos =
    Shape_Pos(self, where)

  def @-(where: Bounding_Box_Pos): Shape_Pos =
    at(where)
}


object Shape_Pos {

  implicit def to_Point(shape_pos: Shape_Pos): Point =
    shape_pos.point

  implicit def to_Path(shape_pos: Shape_Pos): Path =
    shape_pos.path
}

final case class Shape_Pos(shape: Shape, pos: Bounding_Box_Pos) {

  def point: Point =
    pos(shape)

  def path: Path =
    Path.from_@(point)

  def on(other: Shape_Pos): Shape =
    align_with(other.shape, other.pos)

  def on(other_bboxed: Bounding_Boxed, where_on_other: Bounding_Box_Pos):
      Shape =
    align_with(other_bboxed, where_on_other)

  def align_with(other: Shape_Pos): Shape =
    align_with(other.shape, other.pos)

  def align_with(other_bboxed: Bounding_Boxed,
                 where_on_other: Bounding_Box_Pos):
      Shape = {
    (pos, where_on_other) match {
      case (Center, pos) =>
        shape.align_with(other_bboxed, pos, Centered)

      case (pos, Center) =>
        val (x_offset, y_offset) = pos(shape) - other_bboxed.center
        shape.move(-x_offset, -y_offset)

      case (pos, other_pos) if pos == other_pos =>
        shape.align_with(other_bboxed, other_pos, Inside)

      case (pos, other_pos) if pos.opposite == other_pos =>
        shape.align_with(other_bboxed, other_pos, Outside)

      case (pos, other_pos) =>
        val (x_offset, y_offset) = pos(shape) - other_pos(other_bboxed)
        shape.move(-x_offset, -y_offset)

/*
      case Top_Middle    =>
      case Bottom_Middle =>
      case Left_Middle   =>
      case Right_Middle  =>
      case Top_Left      =>
      case Bottom_Right  =>
      case Top_Right     =>
      case Bottom_Left   =>
*/
    }
  }


  //??????????`attach` / `attach_to`???????
  def align_under(other: Shape_Pos): Shape =
    align_under(other.shape, other.pos)

  def align_under(over: Shape, where_on_over: Bounding_Box_Pos): Shape =
    shape combo (where_on_over of over).align_with(shape, pos)

  def align_under(over: Shape, how: Alignment_Relation = Centered): Shape =
    shape combo over.align_with(shape, pos, how)




//!!!!!!!show example of fixing 'intersection problem' of:
// Top_Middle of shape line_to (Bottom_Left of shape_to_left)
// Top_Middle of shape line_to (shape_to_left at Bottom_Left)
// shape @- Top_Middle line_to (shape_to_left @-Bottom_Left)
// shape at Top_Middle line_to (shape_to_left at Bottom_Left)
// Top_Middle of shape bezier2_@ (Top_Left of shape_to_left -+ (-20),
//                                Bottom_Left of shape_to_left)


  def line_to(pt: Point): Shape =
    Line.between(this.point, pt)

  def line_to(other: Shape_Pos): Shape =
    line_to(other.point)
}

}
