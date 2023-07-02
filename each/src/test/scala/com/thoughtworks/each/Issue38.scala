package com.thoughtworks.each

import scalaz.std.option._
import Monadic._

final class Issue38 {
  def shouldNotWarning = {
    monadic[Option] {
      Option(
        {
          val mm = 1
          mm
        }).each;
      1
    }
  }
}