package es.uam.eps.bmi.search.vsm;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;
import java.io.IOException;
import java.util.Iterator;
import java.util.PriorityQueue;

/**
 *
 * @author pablo
 */
public class DocBasedVSMCosineEngine extends AbstractVSMEngine {

    public DocBasedVSMCosineEngine(Index index) {
        super(index);
    }
    
    public SearchRanking search(String q, int cutoff) throws IOException {
        String query[] = parse(q);
        RankingImpl ranking = new RankingImpl(index, cutoff);
        PriorityQueue<QueryPosting> cosineHeap = new PriorityQueue<QueryPosting>();
        
        for (String term : query) {
            Iterator<Posting> postingIter = index.getPostings(term).iterator();
            cosineHeap.add(new QueryPosting(postingIter.next(), index.getDocFreq(term), postingIter));
        }
        int numDocs = index.numDocs();
        
        int currentDocID = cosineHeap.peek().posting.getDocID();
        double score = 0;
        while (!cosineHeap.isEmpty()) {
            QueryPosting qp = cosineHeap.poll();
            if (qp.posting.getDocID() != currentDocID) {
                ranking.add(currentDocID, score / index.getDocNorm(currentDocID));
                score = 0;
                currentDocID = qp.posting.getDocID();
            }
            score += tfidf(qp.posting.getFreq(), qp.docFreq, numDocs);
            if (qp.postingIter.hasNext()) {
                qp.posting = qp.postingIter.next();
                cosineHeap.add(qp);
            }
        }
        ranking.add(currentDocID, score / index.getDocNorm(currentDocID));
        
        return ranking;
    }

    class QueryPosting implements Comparable<QueryPosting> {
        Posting posting;
        long docFreq;
        Iterator<Posting> postingIter;

        QueryPosting(Posting p, long df, Iterator<Posting> pl) {
            posting = p;
            docFreq = df;
            postingIter = pl;
        }

        public int compareTo(QueryPosting qp) {
            return posting.getDocID() - qp.posting.getDocID();
        }
    }
}
