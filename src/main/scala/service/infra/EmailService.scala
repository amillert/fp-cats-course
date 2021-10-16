package service
package infra

abstract private[service] trait EmailService {

  protected def emailService: Service

  protected trait Service {
    def sendEmail(address: String, text: String): Unit
  }

  final def sendEmail(address: String, text: String): Unit = emailService.sendEmail(address, text)
}
