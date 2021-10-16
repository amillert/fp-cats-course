package service

package object domain {

  final case class Person(id: Long, name: String, emailAddress: String)
  final case class Account(id: Long, ownerId: Long)
}
