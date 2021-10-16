package service
package infra

import service.domain._

abstract private[service] trait AccountRepository {

  protected def accountRepository: Service

  protected trait Service {
    def findAccountById(id: Long): Account
    def saveAccount(account: Account): Unit
  }

  final def findAccountById(id: Long): Account  = accountRepository.findAccountById(id)
  final def saveAccount(account: Account): Unit = accountRepository.saveAccount(account)
}
