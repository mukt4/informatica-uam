package es.uam.eps.bmi.search.ranking.impl;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class RankingImplIterator implements Iterator<SearchRankingDoc> {
    private List<SearchRankingDoc> results;
    private Index index;
    private int n;

    RankingImplIterator(Index idx, List<SearchRankingDoc> r) {
        index = idx;
        results = r;
        n = 0;
    }

    @Override
    public boolean hasNext() {
        return n < results.size();
    }

    @Override
    public SearchRankingDoc next() {

        int docID = results.get(n).getDocID();
        double score = results.get(n).getScore();
        String docPath = null;
        try {
            docPath = results.get(n).getPath();
        } catch (IOException ex) {
            Logger.getLogger(RankingImplIterator.class.getName()).log(Level.SEVERE, null, ex);
        }

        n++;

        return new RankingImplDoc(docID, score, docPath);
    }
}
