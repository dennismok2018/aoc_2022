
import java.io.File

//Q1 & Q2 Solution inspired by https://www.youtube.com/watch?v=Q819VW8yxFo
fun main(){
    val filePath = "/home/dm/projects/aoc_2022/d7/input"

    val patternDir = "\\$ cd (\\w+|\\.\\.|/)".toRegex()
    val patternSize = "(\\d+) .*".toRegex()

    var pwd = ""
    val dirToVol = mutableMapOf<String, Int>()
    File(filePath).bufferedReader().lines().forEachOrdered {
        patternDir.find(it)?.apply {
            val (found) = this!!.destructured
            when(found){
                "/" -> {
                    pwd = ""
                }
                ".." -> {
                    pwd = pwd.substringBeforeLast("/", "")
                }
                else -> {
                    pwd = pwd + "/" + found
                }
            }
        }
        patternSize.find(it)?.apply {
            val (found) = this!!.destructured
            var layers = pwd

            while (true){
                dirToVol.put(layers, found.toInt() + dirToVol.getOrDefault(layers, 0))
                if (layers != "") {
                    layers = layers.substringBeforeLast("/", "")
                } else {
                    break
                }
            }
        }
    }

    //Q1
    println( dirToVol.map { it.value }.filter { it < 100000 }.sum())

    //Q2
    val sum = dirToVol[""]!!
    val needed = 30000000 - (70000000 - sum)
    val smallest = dirToVol.map { it.value }.filter { it >= needed }.sorted()[0]
    println(smallest)

}


