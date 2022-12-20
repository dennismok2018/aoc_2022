import java.io.File
import java.util.BitSet


enum class ItemType(index: Int){


}


// Q1
//fun main(){
//
//    val charToInt = mutableMapOf<Char, Int>()
//    var tmpIndex = 1;
//    for (ch in 'a'.. 'z'){
//        charToInt[ch] = tmpIndex++
//    }
//    for (ch in 'A'.. 'Z'){
//        charToInt[ch] = tmpIndex++
//    }
//    val filePath = "/home/dm/projects/aoc_2022/d3/d3_input"
//
//    val priorityScores = mutableListOf<Int>()
//    var sum = 0
//
//    File(filePath).forEachLine {
//        val firstHalf = BitSet(53)
//        val secondHalf = BitSet(53)
//        val firstHalfStr = it.substring(0, it.length/2)
//        val secondHalfStr = it.substring(it.length/2)
//        for (ch in firstHalfStr){
//            firstHalf[charToInt[ch]!!] = true
//        }
//        for (ch in secondHalfStr){
//            secondHalf[charToInt[ch]!!] = true
//        }
//        firstHalf.and(secondHalf)
//        sum += firstHalf.nextSetBit(0)
//        priorityScores.add(firstHalf.nextSetBit(0))
//    }
//    println(priorityScores)
//    println("sum $sum")
//}

// Q2
fun main(){

    val charToInt = mutableMapOf<Char, Int>()
    var tmpIndex = 1;
    for (ch in 'a'.. 'z'){
        charToInt[ch] = tmpIndex++
    }
    for (ch in 'A'.. 'Z'){
        charToInt[ch] = tmpIndex++
    }
    val filePath = "/home/dm/projects/aoc_2022/d3/d3_input"

    val priorityScores = mutableListOf<Int>()
    var sum = 0

    val firstLine = BitSet(53)
    val secondLine = BitSet(53)
    val thirdLine = BitSet(53)
    var counter=1
    File(filePath).forEachLine {
        println("c $counter")
        when (counter++) {
            3 -> {
                for (ch in it){
                    thirdLine[charToInt[ch]!!] = true
                }
                firstLine.and(secondLine)
                firstLine.and(thirdLine)
                sum += firstLine.nextSetBit(0)
                priorityScores.add(firstLine.nextSetBit(0))

                firstLine.clear()
                secondLine.clear()
                thirdLine.clear()
                counter = 1
            }
            2 -> {
                for (ch in it){
                    secondLine[charToInt[ch]!!] = true
                }
            }
            1 -> {
                for (ch in it){
                    firstLine[charToInt[ch]!!] = true
                }
            }
        }



    }
    println(priorityScores)
    println("sum $sum")
}