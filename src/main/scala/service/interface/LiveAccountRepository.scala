package service
package interface

import service.domain.Account
import service.infra.AccountRepository

trait LiveAccountRepository extends AccountRepository {

  val accountRepository = new Service {
    override def findAccountById(id: Long): Account  = Account(id, 123L)
    override def saveAccount(account: Account): Unit = println(s"Account: `${account.id} saved`")
  }
}
