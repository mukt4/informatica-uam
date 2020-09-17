package es.uam.eps.bmi.search.ranking.impl;

import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.util.Iterator;
import java.util.PriorityQueue;

/**
 *
 * @author pablo
 */
public class RankingIteratorImpl implements Iterator<SearchRankingDoc> {
    SearchRankingDoc ranking[];
    int pos;

    public RankingIteratorImpl (PriorityQueue<SearchRankingDoc> r) {
        PriorityQueue<SearchRankingDoc> results = new PriorityQueue<SearchRankingDoc>(r);
        ranking = new SearchRankingDoc[results.size()];
        pos = ranking.length;
        while (!results.isEmpty())
            ranking[--pos] = results.poll();
    }
    
    public boolean hasNext() {
        return pos < ranking.length;
    }

    public SearchRankingDoc next() {
         return ranking[pos++];
    }
}
