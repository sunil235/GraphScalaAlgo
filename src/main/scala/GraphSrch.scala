import scala.collection.{mutable => m}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object GraphSrch {

  // Depth First Search
  def deptFirstPrint(graph: m.HashMap[String, ListBuffer[String]], node: String, visited: ListBuffer[String]): ListBuffer[String] = {
    if (!visited.contains(node)) {
      visited.append(node)
      for (k <- graph(node)) {
        deptFirstPrint(graph, k, visited)
      }
    }
    return visited
  }

  //Breadth First Search
  def breadthFirstPrint(graph: m.HashMap[String, ListBuffer[String]], node: String, visited: ListBuffer[String]): ListBuffer[String] = {
    val queue = ListBuffer.empty[String]
    visited.append(node)
    queue.append(node)
    while (queue.nonEmpty) {
      val s = queue.remove(0)
      for (k <- graph(s)) {
        if (!visited.contains(k)) {
          queue.append(k)
          visited.append(k)
        }
      }
    }
    return visited
  }
  // Build Adjacency list
  def buildAdjList(wordlist: List[String]): m.HashMap[String, ListBuffer[String]] = {
    val startswith: m.HashMap[Char, ListBuffer[String]] = m.HashMap.empty[Char, ListBuffer[String]]
    val graph: m.HashMap[String, ListBuffer[String]] = m.HashMap.empty[String, ListBuffer[String]]
    for (word <- wordlist) {
      if (!startswith.contains(word(0))) {
        startswith.put(word(0), ListBuffer(word))
      } else {
        startswith(word(0)).append(word)
      }
    }
    for (word <- wordlist) {
      val elem = graph.get(word)
      if (elem.isEmpty) {
        graph.put(word,startswith.getOrElse(word.last,ListBuffer.empty[String]))
      } else {
        graph += word -> startswith.getOrElse(word.last,ListBuffer.empty[String])
   }
     }
    return graph
  }


  def main(args: Array[String]): Unit = {
    //Args 0 : File name 1:label
    val file =  Source.fromFile(args(0))
    val lines: List[String] = file.getLines.toList
    file.close()
    val GetOption: String = args(1)
    val GetWord: String = args(2)


    if (GetOption == "DFS") {
      val t0 = System.nanoTime()
      print(deptFirstPrint(buildAdjList(lines),GetWord,ListBuffer.empty[String]))
      val t1 = System.nanoTime()
      print("\n Time taken for " + GetOption + " is :" + (t1 - t0) / 1e9d + " seconds")
      //print("\n Node id :"+ {dictl[GetWord]}")

   }
    else {
      val t0 = System.nanoTime()
      print(breadthFirstPrint(buildAdjList(lines),GetWord,ListBuffer.empty[String]))
      val t1 = System.nanoTime()
      print("\n Time taken for " + GetOption + " is :" + (t1 - t0) / 1e9d + " seconds")
    }
  }
}


