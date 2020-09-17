package es.uam.eps.bmi.search;

import es.uam.eps.bmi.search.index.DocumentFeatureMap;
import es.uam.eps.bmi.search.index.DocumentMap;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;
import java.io.IOException;

/**
 *
 * @author Javier Sanz-Cruzado Puig (javier.sanz-cruzado@uam.es)
 * @author pablo
 */
public class StaticEngine implements SearchEngine {
    DocumentFeatureMap map;
    
    public StaticEngine(DocumentFeatureMap map) {
        this.map = map;
    }

    public SearchRanking search(String query, int cutoff) throws IOException {
        RankingImpl ranking = new RankingImpl(map, cutoff);
        for (int doc = 0; doc < map.numDocs(); doc++) 
            ranking.add(doc, map.getValue(doc));
        return ranking;
    }

    public DocumentMap getDocMap() {
        return map;
    }
}
