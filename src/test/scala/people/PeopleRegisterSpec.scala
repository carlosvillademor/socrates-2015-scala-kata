package people

import org.scalatest.{FunSpec, Matchers}

class RegisterSpec extends FunSpec  with Matchers {
  
  val people = """
Adam 23
Bob 15
Charlie 8
Dave 49
Elizabeth 12
Francis 42
George 85
Harriet 62
Janet 13
Karl 8
Laura 6
Mary 19
Nick 16
Oliver 54
Peter 27
Peter 34
Robert 36
Sally 26
Tom 58
Tom 13
Tom 58
Tom 45
Vicky 69
Wally 28
    """
  
  describe("the register") {

    val register = PeopleRegister.parse(people)
    
    it("should report twenty-four people") {(
      register.count should be(24))
    }
    
    it("should say that Dave is the oldest") {
      register.oldestPerson should be(Person("George", 85))
    }
    
    it("should say that Tom is the commonest name") {
      register.commonestName should be("Tom")
    }
    
    it("should say that the youngest Peter is 27 years old") {
      register.youngestCalled("Peter") should be(Some(27))
    }
    
    it("should say that the youngest Carlos is not fount") {
      register.youngestCalled("Carlos") should be(None)
    }

    it("should report eight children") {
      register.countOfChildren should be(8)
    }
    
    it("should report an adult-to-child ratio of 2") {
      register.adultToChildRatio should be(2.0)
    }
    
    it("should calculate the average age") {
      register.averageAge should be(33)
    }
    
    it("should calculate the median age") {
      register.medianAge should be(27.5)
    }
    
  }

}

class PeopleRegister(people: Seq[Person]) {
  
  def count(): Int = people.size
  
  def oldestPerson(): Person = people.maxBy(_.age)
  
  def commonestName(): String = people.groupBy(_.name).mapValues(_.size).maxBy(_._2)._1
  
  def youngestCalled(sought: String): Option[Int] = people.filter(_.name == sought) match {
    case Nil => None
    case peopleByName => Option(peopleByName.minBy(_.age).age)
  }
  
  def countOfChildren(): Int = people.filter(_.age<=18).size
  
  def adultToChildRatio(): Float = people.filter(_.age>=18).size/people.filter(_.age<=18).size
  
  def averageAge(): Int = people.map(person => person.age).sum/people.size

  def medianAge(): Double = ???
  
}

case class Person(name: String, age: Int)

object Person {
  def parse(data: String): Person = data.trim.split(" ").toSeq match {
    case Seq(name, age) => Person(name, age.toInt)
  }
}

object PeopleRegister {
  def parse(personData: String): PeopleRegister = new PeopleRegister(personData.trim.split("\n").toSeq.map(Person.parse))
}