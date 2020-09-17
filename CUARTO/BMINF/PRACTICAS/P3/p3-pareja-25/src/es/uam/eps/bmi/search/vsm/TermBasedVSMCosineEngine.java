package es.uam.eps.bmi.search.vsm;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.impl.SerializedRAMIndex;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author pablo
 */
public class TermBasedVSMCosineEngine extends AbstractVSMEngine {

    public TermBasedVSMCosineEngine(Index index) {
        super(index);
    }
    
    public SearchRanking search(String query, int cutoff) throws IOException {
        RankingImpl ranking = new RankingImpl(index, cutoff);
        Map<Integer,Double> scores = new HashMap<Integer,Double>();
        for (String q : parse(query))
            for (Posting p : index.getPostings(q))
                addScore(scores, p, q);
        for (int docID : scores.keySet())
            ranking.add(docID, scores.get(docID) / index.getDocNorm(docID));
        return ranking;
    }
    
    void addScore(Map<Integer,Double> scores, Posting p, String term) throws IOException {
        int docID = p.getDocID();
        if (!scores.containsKey(docID)) scores.put(docID, 0.0);
        scores.put(docID, scores.get(docID) + tfidf(p.getFreq(), index.getDocFreq(term), index.numDocs()));
    }
    
    public void loadIndex(String path) throws IOException {
        index = new SerializedRAMIndex(path);
    }
}
