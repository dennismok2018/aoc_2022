
import java.io.File


//
//
//// Q1
//fun main(){
//
//
//    val filePath = "/home/dm/projects/aoc_2022/d4/d4_input"
//
//    var counter=0
//    File(filePath).forEachLine {
//
//        val pair = it.split(',').toTypedArray()
//        val a = pair[0].split('-')
//        val b = pair[1].split('-')
//        val aLow = a[0].toInt()
//        val aHigh = a[1].toInt()
//        val bLow = b[0].toInt()
//        val bHigh = b[1].toInt()
//
////        println("a $a b $b")
////        if (
////            (aLow <= bLow && aHigh >= bHigh)
////        ) {
////            println("first !!! $aLow <= $bLow && $aHigh >= $bHigh")
////            counter++
////            return@forEachLine
////        }
////
////        if (
////            (bLow <= aLow && bHigh >= aHigh)
////        ) {
////            println("second !!! $bLow <= $aLow && $bHigh >= $aHigh")
////            counter++
////            return@forEachLine
////        }
//        if (
//            (aLow <= bLow && aHigh >= bHigh) || (bLow <= aLow && bHigh >= aHigh)
//        ) {
//            counter++
//        }
//        
//    }
//    println("c $counter")
//}



// Q2
fun main(){


    val filePath = "/home/dm/projects/aoc_2022/d4/d4_input"

    var notOverlap=0
    var total=0
    File(filePath).forEachLine {

        val pair = it.split(',').toTypedArray()
        val a = pair[0].split('-')
        val b = pair[1].split('-')
        val aLow = a[0].toInt()
        val aHigh = a[1].toInt()
        val bLow = b[0].toInt()
        val bHigh = b[1].toInt()


        if (
            (aHigh < bLow) || (bHigh < aLow)
        ) {
            notOverlap++
        }
        total++
        
    }
    val overlap = total - notOverlap
    println("overlap $overlap")
}