package es.uam.eps.bmi.search.ranking.impl;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class RankingImplHeapObj implements Comparable<RankingImplHeapObj> {

    private int docID;
    private double score;

    public RankingImplHeapObj(int docID, double score){

        this.docID = docID;
        this.score = score;
    }

    public int getDocID(){

        return docID;
    }

    public double getScore(){

        return score;
    }

    @Override
    public int compareTo(RankingImplHeapObj d)  {

        int comp = (int) Math.signum(getScore() - d.getScore());

        if (comp == 0)
            return (int) Math.signum(getDocID() - d.getDocID());
        else
            return comp;
    }
}
