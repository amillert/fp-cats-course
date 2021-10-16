package service
package interface

import service.domain.Person
import service.infra.PersonRepository

trait LivePersonRepository extends PersonRepository {

  val personRepository: Service = new Service {
    def findPersonById(id: Long): Person = Person(id, "Albert", "albert@gmail.com")
  }
}
