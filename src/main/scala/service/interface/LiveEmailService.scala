package service.interface

import service.infra.EmailService

trait LiveEmailService extends EmailService {

  val emailService = new Service {
    override def sendEmail(address: String, text: String): Unit =
      println(s"Email: \n```\n$text\n``` has been sent to `$address`")
  }
}
