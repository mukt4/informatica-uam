package es.uam.eps.bmi.search.ranking.impl;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class RankingImpl implements SearchRanking {
    private Index index;
    private int cutoff;
    private int numDocs;
    private int numResults;
    private List<SearchRankingDoc> rankingDocs = new ArrayList<>();

    public RankingImpl(Index index, int cutoff) {

        this.cutoff = cutoff;
        this.index = index;
        this.numResults = 0;
        this.numDocs = 0;
    }

    @Override
    public Iterator<SearchRankingDoc> iterator() {

        Collections.sort(rankingDocs);
        return new RankingImplIterator(index, rankingDocs);
    }

    @Override
    public int size() {
        return rankingDocs.size();
    }

    @Override
    public long nResults() {
        return numResults;
    }

    public void add(int docID, double score) throws IOException {

        numResults++;

        // Rellenamos hasta llegar a cutoff
        if (numDocs < cutoff){

            numDocs++;
            rankingDocs.add(new RankingImplDoc(docID, score, index.getDocPath(docID)));
        }

        // Una vez relleno procedemos a ordenar
        else {

            Collections.sort(rankingDocs);
            if(rankingDocs.get(cutoff-1).getScore() < score){
                rankingDocs.remove(cutoff-1);
                rankingDocs.add(new RankingImplDoc(docID, score, index.getDocPath(docID)));
            }
        }
    }
}
