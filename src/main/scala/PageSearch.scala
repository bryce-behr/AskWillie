import scala.math.log
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParMap

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.par.map(page => query.map(tgt => page.text.sliding(tgt.length).count(window => window == tgt).toDouble).sum).toList//countOccurrences(page.text, query))
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        count(pages, query).zip(pages.par.map(_.text.length)).par.map(_/_).toList
    }



//    @main def runner(): Unit =
//        println(tfidf)
//

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // list of IDF for each term
        val iDFS: ParMap[String, Double] = query.par.map(term => term -> log(pages.length / (pages.par.count(_.text.contains(term))+1))).toMap

        def singleTermSinglePageTf(p: RankedWebPage, term: String): Double =
            p.text.sliding(term.length).count(window => window == term).toDouble / p.text.length

        pages.par.map(p => query.foldLeft(0.0)((accumulator: Double, currentTerm: String) => accumulator + (singleTermSinglePageTf(p, currentTerm) * iDFS(currentTerm)))).toList

    }
}