package es.uam.eps.bmi.search.ranking;

/**
 *
 * @author pablo
 * @author javier
 */
public interface SearchRanking extends Iterable<SearchRankingDoc> {
    public int size();
    public long nResults();
}
