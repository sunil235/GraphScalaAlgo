import java.io.File
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
import scala.io.Source


object GraphSrch {

  // Depth First Search
  def deptFirstPrint(graph: Map[String, ListBuffer[String]], node: String, visited: ListBuffer[String]): ListBuffer[String] = {
    if (visited.contains(node) == false) {
      visited.append(node)
      for (k <- graph(node)) {
        deptFirstPrint(graph, k, visited)
      }
    }
    return visited
  }

  //Breadth First Search
  def breadthFirstPrint(graph: Map[String, ListBuffer[String]], node: String, visited: ListBuffer[String]): ListBuffer[String] = {
    val queue = ListBuffer.empty[String]
    visited.append(node)
    queue.append(node)
    while (queue.length > 0) {
      val s = queue.remove(0)
      for (k <- graph(s)) {
        if (visited.contains(k) == false) {
          visited.append(k)
          queue.append(k)
        }
      }
    }
    return visited
  }
  // Build Adjacency list
  def buildAdjList(wordlist: List[String]): Map[String, ListBuffer[String]] = {
    val startswith: Map[Char, ListBuffer[String]] = Map.empty[Char, ListBuffer[String]]
    val graph: Map[String, ListBuffer[String]] = Map.empty[String, ListBuffer[String]]
    for (word <- wordlist) {
      if (startswith.get(word(0)) == None) {
        startswith.put(word(0), ListBuffer(word))
      } else {
        startswith(word(0)).append(word)
      }
    }
    for (word <- wordlist) {
      val elem = graph.get(word)
      if (elem == None) {
        graph.put(word,startswith.get(word.last).getOrElse(ListBuffer.empty[String]))
      } else {
        graph += word -> startswith.get(word.last).getOrElse(ListBuffer.empty[String])
   }
     }
    return graph
  }


  def main(args: Array[String]): Unit = {
    //Args 0 : File name 1:label
    val lines: List[String] = Source.fromFile(args(0)).getLines.toList
    val GetOption: String = args(1)
    val GetWord: String = args(2)

    if (GetOption == "DFS") {
      val t0 = System.nanoTime()
      print(deptFirstPrint(buildAdjList(lines),GetWord,ListBuffer.empty[String]))
      val t1 = System.nanoTime()
      print("\n Time taken for " + GetOption + " is :" + (t1 - t0) / 1e9d + " seconds")
   }
    else {
      val t0 = System.nanoTime()
      print(breadthFirstPrint(buildAdjList(lines),GetWord,ListBuffer.empty[String]))
      val t1 = System.nanoTime()
      print("\n Time taken for " + GetOption + " is :" + (t1 - t0) / 1e9d + " seconds")
    }
  }
}


