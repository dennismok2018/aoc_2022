import java.io.File

//Q1
//fun main(){
//    val filePath = "/home/dm/projects/aoc_2022/d8/input"
//
//    // a 2d int array representing input
//    // reading input
//    var row = 0
//    val tabula = mutableMapOf<Int, MutableList<Int>>()
//    File(filePath).bufferedReader().lines().forEachOrdered { line ->
//        var list = tabula.getOrDefault(row, mutableListOf())
//        line.forEach {
//                list.add(it.toString().toInt())
//        }
//        tabula[row] = list
//        row++
//    }
//
////    println(tabula)
//    tabula.forEach { line ->
//        line.value.forEach {
//            print(it)
//        }
//        println()
//    }
//
//
//    //Q1
//    // a 2d boolean array as result
//    val result = mutableMapOf<Int, MutableList<Boolean>>()
//
//    // Populating boolean array: horizontally
//    for (i in 0 until row){
//
//        val line = tabula[i]
//        if (line.isNullOrEmpty()){
//            println("tabula[i].isNullOrEmpty()")
//            continue
//        }
//        val r = result.getOrDefault(i,  MutableList(line.size){false} )
//        // from left to right
//        var lastMax = -1
//        for (j in 0 until line.size){
//            val isHigher = line[j] > lastMax
//            r[j] = isHigher || r[j]
//            lastMax = if (line[j] > lastMax) line[j] else lastMax
//        }
//
//        // from right to left
//        lastMax = -1
//        for (j in  line.size - 1  downTo   0){
//            val isHigher = line[j] > lastMax
//            r[j] = isHigher  || r[j]
//            lastMax = if (line[j] > lastMax) line[j] else lastMax
//        }
//
//        result[i] = r
//    }
//
//    // Populating boolean array: traverse vertically
//    val width = tabula[0]!!.size
//    for (col in 0  until  width){
//
//        // from top to bottom
//        var lastMax = -1
//        for (row in 0 until tabula.size){
//            val r = result[row]
//            val line = tabula[row]
//            val higher = line!![col] > lastMax
//            r!![col] = higher || r[col]
//            lastMax = if (line!![col] > lastMax ) line[col] else lastMax
////            result[row] = r
//        }
//
//        // from bottom to top
//        lastMax = -1
//        for (row in tabula.size -1 downTo 0 ){
//            val r = result[row]
//            val line = tabula[row]
//            val higher = line!![col] > lastMax
//            r!![col] = higher || r[col]
//            lastMax = if (line!![col] > lastMax ) line[col] else lastMax
////            result[row] = r
//        }
//    }
//
//
////    println()
//    result.forEach { r ->
//        r.value.forEach {
//            if (it){
//                print("1")
//            } else{
//                print("0")
//            }
//        }
//        println()
//    }
//
//    val ans = result.map {
//        it.value
//    }.map { list ->
//        list.count {
//            it
//        }
//    }.sum()
//
//    println("ans $ans")
//
//}

//Q2
fun main() {
    val filePath = "/home/dm/projects/aoc_2022/d8/input"

    // a 2d int array representing input
    // reading input
    var row = 0
    val tabula = mutableMapOf<Int, MutableList<Int>>()
    File(filePath).bufferedReader().lines().forEachOrdered { line ->
        var list = tabula.getOrDefault(row, mutableListOf())
        line.forEach {
            list.add(it.toString().toInt())
        }
        tabula[row] = list
        row++
    }

    // a 2d Int array as result
    val result = mutableMapOf<Int, MutableList<Int>>()

    // Populating boolean array: horizontally
    for (i in 0 until tabula.size) {
        val r = result.getOrDefault(i, MutableList(tabula[0]!!.size) { 0 })
        for (j in 0 until tabula[0]!!.size) {
            val height = tabula[i]!![j]

            var level = 1
            var up = 0
            while (i - level != -1) {
                val t = tabula[i - level]!![j]
                up++
                if (height > t) {
                    level++
                } else {
                    break
                }
            }

            level = 1
            var down = 0
            while (i + level != tabula.size) {
                val t = tabula[i + level]!![j]
                down++
                if (height > t) {
                    level++
                } else {
                    break
                }
            }
            level = 1
            var left = 0
            while (j - level != -1) {
                val t = tabula[i]!![j - level]
                left++
                if (height > t) {
                    level++
                } else {
                    break
                }
            }
            level = 1
            var right = 0
            while (j + level != tabula[0]!!.size) {
                val t = tabula[i]!![j + level]
                right++
                if (height > t) {
                    level++
                } else {
                    break
                }
            }
            val scores = arrayOf(up, down, left, right)
            println("Sc $scores")
            val score = scores.filter {
                it != 0
            }.reduce { acc, v ->
                acc * v
            }
            r[j] = score
        }
        result[i] = r
    }


//    println()
    result.forEach { r ->
        r.value.map {
            print(it)
        }
        println()
    }

    val ans = result.map {
        it.value
    }.map {
        it.max()
    }.max()
    println("ans $ans")

}

