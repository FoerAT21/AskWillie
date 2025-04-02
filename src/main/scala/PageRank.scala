import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((id, page) => (id,1.0)).toMap
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number
     *         of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((id,_) => (id, pages.map((_,page) => page.links.count(_ contains id)).sum.toDouble))
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val S = 10000
        val N = pages.size

        def combineMaps(map1 : Map[String, Double],
                        map2: Map[String, Double]): Map[String, Double] = {
            map1.map((id , count) => (id, count+map2(id)))
        }

        (0 until S).view.par.map { _ =>
            val startingPage = pages.keysIterator.drop(Random.nextInt(pages.size)).next()
            walker(pages, startingPage, pages.par.map((id, _) => (id, 0.0)).seq)
        }.foldLeft(pages.map((id, _) => (id, 0.0)))(combineMaps).map(
            (id, count) => (id, count+1/(S+N).toDouble))
    }

    @tailrec
    private def walker(pages : Map[String, WebPage], currPageId : String, currMap : Map[String,Double], clicks : Int = 0): Map[String,Double] = {
        if clicks == 100 then currMap
        else {
            val currentPage: WebPage = pages(currPageId)
            val newPage =
                if (Random.nextDouble() <= 0.85 && currentPage.links.nonEmpty)
                    currentPage.links(Random.nextInt(currentPage.links.length))
                else
                    pages.keysIterator.drop(Random.nextInt(pages.size)).next()

            walker(pages, newPage, followLink(newPage, currMap), clicks + 1)
        }
    }

    private def followLink(pageid : String, currMap: Map[String,Double]): Map[String, Double] = {
        currMap.map((id,count) => if pageid != id then (id,count) else (id,count+1.0))
    }
}