package temp

object Temp {
  var current_num = 0

  def increase_num() : Int = {
    current_num += 1
    current_num
  }

  def get_new_name() : String = {
    "temp$%d".format(increase_num())
  }

  def apply() : Temp = {
    Temp(get_new_name())
  }
}

case class Temp(name : String) {
  def get_name = name
}

object Label {
  var current_num = 0

  def increase_num() : Int = {
    current_num += 1
    current_num
  }

  def get_new_name() : String = {
    "Label%d".format(increase_num())
  }

  def apply() : Label = Label(get_new_name())
}

case class Label(name : String) {
  def get_name = name
}
