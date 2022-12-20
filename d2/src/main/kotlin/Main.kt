import java.io.File
import kotlin.IllegalArgumentException


enum class Result(val score: Int){
    DRAW(3), LOST(0), WIN(6);

    companion object{
        fun of(s: String): Result {
            return when (s){
                "X" -> LOST
                "Y" -> DRAW
                "Z" -> WIN
                else -> throw IllegalArgumentException()
            }
        }

        fun scoreOf(opponent: Gesture, self: Gesture): Int {
            when (opponent){
                Gesture.ROCK -> return when (self){
                    Gesture.ROCK -> DRAW.score + self.score
                    Gesture.PAPER -> WIN.score + self.score
                    Gesture.SCISSORS -> LOST.score + self.score
                }
                Gesture.PAPER -> return when (self){
                    Gesture.ROCK -> LOST.score + self.score
                    Gesture.PAPER -> DRAW.score + self.score
                    Gesture.SCISSORS -> WIN.score + self.score
                }
                Gesture.SCISSORS -> return when (self){
                    Gesture.ROCK -> WIN.score + self.score
                    Gesture.PAPER -> LOST.score + self.score
                    Gesture.SCISSORS -> DRAW.score + self.score
                }
            }
        }
    }
}

enum class Gesture(val score: Int){
    ROCK(1), PAPER(2), SCISSORS(3);

    companion object {
        fun of(s: String): Gesture {
            return when (s){
                "A" -> ROCK
                "B" -> PAPER
                "C" -> SCISSORS
                else -> throw IllegalArgumentException()
            }
        }
        fun handToPlay(opponent: Gesture, intendedResult: Result) : Gesture {
            when (opponent){
                ROCK -> return when (intendedResult){
                    Result.DRAW -> ROCK
                    Result.LOST -> SCISSORS
                    Result.WIN -> PAPER
                }
                PAPER -> return when (intendedResult){
                    Result.DRAW -> PAPER
                    Result.LOST -> ROCK
                    Result.WIN -> SCISSORS
                }
                SCISSORS -> return when (intendedResult){
                    Result.DRAW -> SCISSORS
                    Result.LOST -> PAPER
                    Result.WIN -> ROCK
                }
            }
        }
    }
}



fun main(){
    val filePath = "/home/dm/my-repo/projects/aoc_2022/d2/d2_input"

    var totalScore = 0

    File(filePath).forEachLine {
        val round = it.split(' ').toTypedArray()
        totalScore += Result.scoreOf(
            Gesture.of(round[0]),
            Gesture.handToPlay(Gesture.of(round[0]), Result.of(round[1])))
    }

    println(totalScore)
}