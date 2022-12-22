//Q1 & 2
import java.io.File
import java.util.LinkedList


fun main(){
    val filePath = "/home/dm/projects/aoc_2022/d6/input"
    val window = LinkedList<Char>()
    var index = 0
    File(filePath).inputStream().use { io ->
        io.readBytes().map {
            it.toInt().toChar()
        }.map {
            window.add(it)
            println("$index: $window")
            index++
            if (window.size == 14 ){
                if (window.toSet().size == 14 ){
                    println("char count $index")
                    return
                }
                window.pop()
            }
        }
    }

}


