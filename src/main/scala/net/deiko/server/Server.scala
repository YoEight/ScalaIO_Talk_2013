package net.deiko.server

import unfiltered.filter.async.Plan
import unfiltered.request._
import unfiltered.response._

object App extends Plan {
  def intent = {
    case req @ GET(Path("/arithmetic")) =>
      req.respond(ResponseString("6 + 3 * 8 - 1 + (2 - 12)"))
  }
}

object Server {
  def main(args: Array[String]) {
    unfiltered.jetty.Http.local(9000).filter(App).run()
  }
}
