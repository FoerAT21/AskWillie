import scala.util.Random
//import scala.collection.parallel.CollectionConverters._

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        for(key <- pages.keys) do {
            println(key)
        }
        for(key <- pages.keys) do {
            println("Links " + pages(key).links)
        }
        pages.map((id, page) => (id,1.0))
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
        Map() // TODO: remove this stub and implement this method
    }
}