package service

import service.domain._
import service.infra._
import service.interface._

import cats.data.Reader

object Util {

  def findOwnerNameByAccountId(id: Long) =
    for {
      accRepo    <- Reader(identity[AccountRepository])
      personRepo <- Reader(identity[PersonRepository])
      acc   = accRepo.findAccountById(id)
      owner = personRepo.findPersonById(acc.ownerId)
    } yield owner.name

  def openAccount(
      accountId: Long,
      ownerId: Long
    ): Reader[AccountRepository with PersonRepository with EmailService, Unit] =
    for {
      accRepo    <- Reader(identity[AccountRepository])
      personRepo <- Reader(identity[PersonRepository])
      emailServ  <- Reader(identity[EmailService])
      acc    = Account(accountId, ownerId)
      person = personRepo.findPersonById(acc.ownerId)
      _      = accRepo.saveAccount(acc)
      _ = emailServ.sendEmail(
        person.emailAddress,
        s"Dear ${person.name},\n\nThanks for entrusting us with Your life savings!"
      )
    } yield ()

  type Env = PersonRepository with AccountRepository with EmailService
  val liveEnv: Env = new LivePersonRepository with LiveAccountRepository with LiveEmailService
}
