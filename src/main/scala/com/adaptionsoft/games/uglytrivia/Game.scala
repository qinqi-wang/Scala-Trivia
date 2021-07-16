package com.adaptionsoft.games.uglytrivia

import java.util.{LinkedList, ArrayList}

class Game {
  var players: ArrayList[String] = new ArrayList[String]
  var places: Array[Int] = new Array[Int](6)
  var purses: Array[Int] = new Array[Int](6)
  var inPenaltyBox: Array[Boolean] = new Array[Boolean](6)
  var popQuestions: LinkedList[String] = new LinkedList[String]
  var scienceQuestions: LinkedList[String] = new LinkedList[String]
  var sportsQuestions: LinkedList[String] = new LinkedList[String]
  var rockQuestions: LinkedList[String] = new LinkedList[String]
  var currentPlayer: Int = 0
  var isGettingOutOfPenaltyBox: Boolean = false
  var outputString: String = ""

  def initialize(): Unit = {
    var i: Int = 0
    while (i < 50) {
      popQuestions.addLast(createQuestion("Pop", i))
      scienceQuestions.addLast(createQuestion("Science", i))
      sportsQuestions.addLast(createQuestion("Sports", i))
      rockQuestions.addLast(createQuestion("Rock", i))
      i += 1
    }
  }

  initialize()

  def createQuestion(name: String, index: Int): String = name + " Question " + index

  def isPlayable: Boolean = (howManyPlayers >= 2)

  def addPlayer(playerName: String): Boolean = {
    players.add(playerName)
    places(howManyPlayers) = 0
    purses(howManyPlayers) = 0
    inPenaltyBox(howManyPlayers) = false
    outputString = outputString.concat(playerName + " was added" + "\n")
    outputString = outputString.concat("They are player number " + players.size + "\n")
    true
  }

  def howManyPlayers: Int = players.size

  def roll(roll: Int): Unit = {
    outputString = outputString.concat(players.get(currentPlayer) + " is the current player" + "\n")
    outputString = outputString.concat("They have rolled a " + roll + "\n")
    if (inPenaltyBox(currentPlayer)) {
      if (roll % 2 != 0) {
        playerExitsPenaltyBox
        playerMovesLocation(roll)
        outputString = outputString.concat("The category is " + currentCategory + "\n")
        askQuestion
      }
      else {
        outputString = outputString.concat(players.get(currentPlayer) + " is not getting out of the penalty box" + "\n")
        isGettingOutOfPenaltyBox = false
      }
    }
    else {
      playerMovesLocation(roll)
      outputString = outputString.concat("The category is " + currentCategory + "\n")
      askQuestion
    }
  }

  private def askQuestion: Unit = {
    if (currentCategory == "Pop") {
      var removedPop = popQuestions.removeFirst()
      outputString = outputString.concat(removedPop + "\n")
    }
    if (currentCategory == "Science") {
      var removedScience = scienceQuestions.removeFirst()
      outputString = outputString.concat(removedScience + "\n")
    }
    if (currentCategory == "Sports") {
      var removedSports = sportsQuestions.removeFirst()
      outputString = outputString.concat(removedSports + "\n")
    }
    if (currentCategory == "Rock") {
      outputString = outputString.concat(rockQuestions.removeFirst + "\n")
    }
  }

  private def currentCategory: String = {
    val playerLocation: Int = places(currentPlayer);

    playerLocation match {
      case 0 => "Pop"
      case 4 => "Pop"
      case 8 => "Pop"
      case 1 => "Science"
      case 5 => "Science"
      case 9 => "Science"
      case 2 => "Sports"
      case 6 => "Sports"
      case 10 => "Sports"
      case _ => "Rock"
    }
  }

  def wasCorrectlyAnswered: Boolean = {
    if (inPenaltyBox(currentPlayer)) {
      if (isGettingOutOfPenaltyBox) {
        outputString = outputString.concat("Answer was correct!!!!" + "\n")
        purses(currentPlayer) += 1
        outputString = outputString.concat(players.get(currentPlayer) + " now has " + purses(currentPlayer) + " Gold Coins." + "\n")
        var winner: Boolean = didPlayerWin
        currentPlayer += 1
        if (currentPlayer == players.size) currentPlayer = 0
        winner
      }
      else {
        currentPlayer += 1
        if (currentPlayer == players.size) currentPlayer = 0
        true
      }
    }
    else {
      outputString = outputString.concat("Answer was corrent!!!!" + "\n")
      purses(currentPlayer) += 1
      outputString = outputString.concat(players.get(currentPlayer) + " now has " + purses(currentPlayer) + " Gold Coins." + "\n")
      var winner: Boolean = didPlayerWin
      currentPlayer += 1
      if (currentPlayer == players.size) currentPlayer = 0
      winner
    }
  }

  def wrongAnswer: Boolean = {
    outputString = outputString.concat("Question was incorrectly answered" + "\n")
    outputString = outputString.concat(players.get(currentPlayer) + " was sent to the penalty box" + "\n")
    inPenaltyBox(currentPlayer) = true
    currentPlayer += 1
    if (currentPlayer == players.size) currentPlayer = 0
    true
  }

  private def didPlayerWin: Boolean = !(purses(currentPlayer) == 6)

  private def playerExitsPenaltyBox: Unit = {
        isGettingOutOfPenaltyBox = true
        outputString = outputString.concat(players.get(currentPlayer) + " is getting out of the penalty box" + "\n")
  }

  private def playerMovesLocation(roll: Int): Unit = {
    places(currentPlayer) = places(currentPlayer) + roll
    if (places(currentPlayer) > 11) places(currentPlayer) = places(currentPlayer) - 12
    outputString = outputString.concat(players.get(currentPlayer) + "'s new location is " + places(currentPlayer) + "\n")
  }
}