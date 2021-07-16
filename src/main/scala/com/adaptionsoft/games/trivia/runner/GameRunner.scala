package com.adaptionsoft.games.trivia.runner

import com.adaptionsoft.games.uglytrivia.Game
import java.util.Random

object GameRunner {
  var notAWinner = false

  def main(args: Array[String]): Unit = {
    /*
    var aGame = new Game();
    aGame.add("Chet")
    aGame.add("Pat")
    aGame.add("Sue")

    var rand: Random = new Random(1);

    do {
      aGame.roll(rand.nextInt(5) + 1)
      if (rand.nextInt(9) == 7) {
        notAWinner = aGame.wrongAnswer
      }
      else {
        notAWinner = aGame.wasCorrectlyAnswered
      }
    } while (notAWinner)
    */
    var output = this.test(args)
    sys.exit(1)
  }

  def test(args: Array[String]): String = {
    var aGame = new Game();
    aGame.addPlayer("Chet")
    aGame.addPlayer("Pat")
    aGame.addPlayer("Sue")

    var rand: Random = new Random(1);

    do {
      aGame.roll(rand.nextInt(5) + 1)
      if (rand.nextInt(9) == 7) {
        notAWinner = aGame.wrongAnswer
      }
      else {
        notAWinner = aGame.wasCorrectlyAnswered
      }
    } while (notAWinner)
    
    println(aGame.outputString)
    return aGame.outputString
  }
}