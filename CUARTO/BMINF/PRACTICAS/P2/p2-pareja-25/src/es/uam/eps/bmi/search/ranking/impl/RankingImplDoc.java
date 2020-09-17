package es.uam.eps.bmi.search.ranking.impl;

import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

import java.io.IOException;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class RankingImplDoc extends SearchRankingDoc {
    private Integer docID;
    private Double score;
    private String docPath;

    RankingImplDoc(int docID, double score, String docPath) {

        this.docID = docID;
        this.score = score;
        this.docPath = docPath;
    }

    @Override
    public double getScore() {

        return score;
    }

    @Override
    public int getDocID() {

        return docID;
    }

    @Override
    public String getPath() {

        return docPath;
    }
}
