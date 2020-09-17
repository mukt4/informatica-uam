package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.io.File;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class TextResultDocRenderer {
    SearchRankingDoc result;
    
    public TextResultDocRenderer (SearchRankingDoc r) {
        result = r;
    }
    
    public String toString() {
        try {
            String uri = result.getPath();
            if (new File(uri).exists()) uri = new File(uri).toURI().toString();
            return result.getScore() + "\t" + uri;
        } catch ( IOException ex) {
            ex.printStackTrace();
            return "";
        }
    }
}
