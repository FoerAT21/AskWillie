import scala.math.log
//import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared
     *               in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map(page => query.map(word => page.text.toLowerCase.split(" ").count(_ contains word.toLowerCase).toDouble).sum)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of
     *              those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map(page => query.map(word => page.text.toLowerCase.split(" ").count(
            _ contains word.toLowerCase).toDouble).sum/page.text.length)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val idfs = idf(pages, query)
        pages.map(page => query.map(word => page.text.toLowerCase.split(" ").count(
            _ contains word.toLowerCase).toDouble * idfs(word.toLowerCase)).sum / page.text.length)
    }

    def idf(pages : List [RankedWebPage], query: List[String]): Map[String, Double] = {
        val NUMDOCS = pages.length
        query.map(word => {
            val D = pages.count(page => page.text contains word.toLowerCase).toDouble + 1
            (word.toLowerCase, log(NUMDOCS.toDouble / D))
          }
        ).toMap
    }
}