package service

import Util._

object Main extends App {

  println(findOwnerNameByAccountId(12).run(liveEnv) + "\n")

  openAccount(1L, 123L).run(liveEnv)
}
