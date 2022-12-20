import java.io.File
import java.util.LinkedList

data class ElfCart(val rations: List<Int>, val total: Int)

fun main(){

    val highest : MutableList<Int> = LinkedList()
    var total:Int = 0
    val rations: MutableList<Int> = mutableListOf()
    val elfCarts: MutableList<ElfCart> = mutableListOf()
    val filePath: String = "/home/dm/my-repo/projects/aoc_2022/d1/q1_input"

    File(filePath).forEachLine { it ->
        if (it.isNotBlank()){
            val i = it.toInt()
            rations.add(i)
            total = total.plus(i)
        } else {
            elfCarts.add(
                ElfCart(rations.toList(), total)
            )
            rations.clear()

            if (highest.size == 0){
                highest.add(total)
            } else if (highest.last() >= total){
                highest.add(total)
            } else {
                var ind = 0
                highest.reversed().forEachIndexed { index, i ->
                    if (i < total){
                        if (index == highest.size -1){
                            ind = 0
                        } else if (highest[highest.size - index -2] >= total){
                            ind = highest.size - index -1
                        }
                    }
                }
                highest.add(ind, total)
            }
            total = total.minus(total)
        }
    }

    var highestTotal = 0;
    for ((index, element) in highest.withIndex()){
        highestTotal += element
        if (index == 2) {
            break
        }
    }
    println(highestTotal);

}

