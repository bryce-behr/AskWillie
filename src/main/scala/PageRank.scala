import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: remove this stub and implement this method
        pages.keys.map(_ -> 1.0).toMap
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: remove this stub and implement this method
        pages.keys.par.map(str => str -> pages.values.count(page => page.links.contains(str)).toDouble).seq.toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: remove this stub and implement this method
        val random = new scala.util.Random
        val dampingFactor = 0.85
        val walks = 10000
        val steps = 100
        val initialWeight = 1.0 / pages.size.toDouble

        def randomWalk(startId: String, stepsLeft: Int): String = {
            if (stepsLeft <= 0) startId
            else {
                val currentPage = pages(startId)
                val nextId = 
                    if (random.nextDouble() < dampingFactor && currentPage.links.nonEmpty) currentPage.links(random.nextInt(currentPage.links.size)) 
                    else pages.keys.toSeq(random.nextInt(pages.size))
                randomWalk(nextId, stepsLeft - 1)
            }
        }

        val finalPageCounts = (1 to walks).par.map({ _ =>
            val startPageId = pages.keys.toSeq(random.nextInt(pages.size))
            randomWalk(startPageId, steps)
        }).seq.groupBy(identity).view.mapValues(_.size).toMap.par

        val totalWalks = finalPageCounts.values.par.sum
        finalPageCounts.seq.view.mapValues(count => (count + 1).toDouble / (totalWalks + pages.size)).toMap
    }
}