package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

/**
 *
 * @author pablo
 */
public class TextResultsRenderer extends ResultsRenderer {
    
    public TextResultsRenderer (SearchRanking results) {
        super(results);
    }
    
    public String toString() {
        StringBuilder str = new StringBuilder();
            if (ranking.size() == 0) str.append("No results found.");
            else for (SearchRankingDoc result : ranking) 
                str.append(new TextResultDocRenderer(result));
        return str.toString();
    }
}
