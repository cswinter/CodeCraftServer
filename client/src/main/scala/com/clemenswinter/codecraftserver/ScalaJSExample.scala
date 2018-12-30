package com.clemenswinter.codecraftserver

import com.clemenswinter.codecraftserver.shared.SharedMessages
import org.scalajs.dom

object ScalaJSExample {

  def main(args: Array[String]): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
  }
}
