package fragment

import frame.{Frame}
import ir.{Exp} 

class Fragment(val stack_frame : Frame, val body : ir.Exp) {
  def transform() : Fragment = {
    new Fragment(stack_frame, body.transform())
  }

  def get_label_name : String = {
    stack_frame.get_name
  }

  def get_body : ir.Exp = body
}

