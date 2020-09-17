package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.ranking.SearchRanking;

/**
 *
 * @author pablo
 */
public abstract class ResultsRenderer {
    SearchRanking ranking;

    public ResultsRenderer() {}

    public abstract String toString();

    public ResultsRenderer(SearchRanking r) {
        setResults(r);
    }

    public void setResults (SearchRanking r) {
        ranking = r;
    }
}
