
import java.io.File
import java.util.LinkedList


enum class Line(val suf: CharSequence){
    CRATE("["),STACK(" 1"),INSTRUCTION("move");

    companion object {
        fun of(it: String) : Line {
            return if(it.contains(CRATE.suf)){
                CRATE
            } else if (it.startsWith(STACK.suf)){
                STACK
            } else if (it.startsWith(INSTRUCTION.suf)){
                INSTRUCTION
            } else {
                throw IllegalArgumentException()
            }
        }
    }
}

fun move(instruction: String): (tabula: MutableMap<Int, LinkedList<Char>>) -> Unit {
    return {
        val regex = "move (\\d+) from (\\d+) to (\\d)".toRegex()
        val result = regex.find(instruction)
        val (steps, source, target) = result!!.destructured

        val sourceStack = it[source.toInt()]
        val targetStack = it[target.toInt()]

        for (step in 1..steps.toInt()){
            if (!sourceStack.isNullOrEmpty()) {
                targetStack!!.addFirst(sourceStack.pop())
            }
        }

    }
}


// Q1
fun main(){


    val filePath = "/home/dm/projects/aoc_2022/d5/input"

    val stacks = mutableMapOf<Int, LinkedList<Char>>()

    val walk = fun (it : String) {
        var depth = 1;
        var stepOrder = 1;
        for (c: Char in it){
            println(stepOrder)
            when(stepOrder++){
                2 -> {
                    if (stacks[depth] == null){
                        stacks[depth] = LinkedList()
                    }
                    if (c.isLetter()){
                        stacks[depth]!!.add(c)
                    }
                    depth++
                }
                4 -> {
                    stepOrder = 1
                }
            }
        }
    }

    File(filePath).forEachLine {
        if (it.isBlank()){
            return@forEachLine
        }
        when(Line.of(it)){
            Line.CRATE ->{
                walk(it)
            }
            Line.STACK -> {
                return@forEachLine
            }
            Line.INSTRUCTION -> {
                val move = move(it)
                move(stacks)
            }
        }
    }
    println("s $stacks")
    for (stack in stacks){
        print(stack.value.first)
    }
}



// Q2
//fun main(){
//
//
//    val filePath = "/home/dm/projects/aoc_2022/d4/d4_input"
//
//    var notOverlap=0
//    var total=0
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
//
//        if (
//            (aHigh < bLow) || (bHigh < aLow)
//        ) {
//            notOverlap++
//        }
//        total++
//
//    }
//    val overlap = total - notOverlap
//    println("overlap $overlap")
//}