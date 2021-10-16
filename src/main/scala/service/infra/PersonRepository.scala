package service
package infra

import service.domain._

abstract private[service] trait PersonRepository {

  protected def personRepository: Service

  protected trait Service {
    def findPersonById(id: Long): Person
  }

  // front-facing API - accessor
  final def findPersonById(id: Long): Person = personRepository.findPersonById(id)
}
