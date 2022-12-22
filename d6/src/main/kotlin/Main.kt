//Q1 & 2
import java.io.File
import java.util.LinkedList


fun main(){
    val filePath = "/home/dm/projects/aoc_2022/d6/input"
    val line = File(filePath).bufferedReader().readLine()
    val window = LinkedList<Char>()
    var index = 0
    line.forEach {
        window.add(it)
        println("$index: $window")
        if (++index >= 14 ){
            if (window.toSet().size == 14 ){
                println("char count $index")
                return
            }
            window.pop()
        }

    }
}


