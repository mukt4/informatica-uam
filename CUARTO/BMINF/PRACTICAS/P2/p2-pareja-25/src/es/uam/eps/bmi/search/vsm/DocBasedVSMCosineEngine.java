package es.uam.eps.bmi.search.vsm;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.impl.ImplPosting;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;
import es.uam.eps.bmi.search.ranking.impl.RankingImplHeapObj;

import java.io.IOException;
import java.util.Iterator;
import java.util.PriorityQueue;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class DocBasedVSMCosineEngine extends AbstractVSMEngine {
    public DocBasedVSMCosineEngine(Index idx) {
        super(idx);
    }

    @Override
    public SearchRanking search(String query, int cutoff) throws IOException {

        String[] terms = query.split(" ");
        int numDocs = index.numDocs();
        RankingImpl ranking = new RankingImpl(index, cutoff);
        PriorityQueue<RankingImplHeapObj> heapRanking = new PriorityQueue<>(terms.length);
        PriorityQueue<ImplPosting> heapPostings = new PriorityQueue<>(terms.length);
        Iterator[] PLIteratorList = new Iterator[terms.length];
        long[] docFreq = new long[terms.length];
        int termID = 0;

        // Inicializamos el heap con el primer posting de cada termino
        for (String term : terms){

            // Inicializamos el array de frecuencias de documentos por termino
            docFreq[termID] = index.getDocFreq(term);
           Iterator<Posting> ipl = index.getPostings(term).iterator();

            if (docFreq[termID] > 0){

                Posting p = ipl.next();
                heapPostings.add(new ImplPosting(p.getDocID(), p.getFreq(), termID));
                PLIteratorList[termID] = ipl;
            }

            termID++;
        }

        int lastDocID = heapPostings.peek().getDocID();
        Posting p;
        ImplPosting top;
        double score = 0;

        // Hasta que el heap este vacio
        while (!heapPostings.isEmpty()){

            // Miramos el último posting
            top = heapPostings.poll();

            // Si el docID actual es distinto al anterior, hay que añadir una
            // nueva entrada al ranking
            if (lastDocID != top.getDocID()){

                if (heapRanking.size() < terms.length)
                    heapRanking.add(new RankingImplHeapObj(lastDocID, score));
                else {
                    RankingImplHeapObj docMejor = heapRanking.poll();
                    ranking.add(docMejor.getDocID(), docMejor.getScore());
                    heapRanking.add(new RankingImplHeapObj(lastDocID, score));
                }
            }
            score = 0;
            lastDocID = top.getDocID();

            // Actualizamos el score del documento
            score += tfidf(top.getFreq(), docFreq[top.getTermID()], numDocs);

            // Añadimos el siguiente posting con termID igual al que acabamos de mirar
            if (PLIteratorList[top.getTermID()].hasNext()){

                p = (Posting) (PLIteratorList[top.getTermID()].next());
                heapPostings.add(new ImplPosting(p.getDocID(),p.getFreq(),top.getTermID()));
            }

        }

        if (heapRanking.size() < terms.length)
            heapRanking.add(new RankingImplHeapObj(lastDocID, score));
        else {
            RankingImplHeapObj docMejor = heapRanking.poll();
            ranking.add(docMejor.getDocID(), docMejor.getScore());
            heapRanking.add(new RankingImplHeapObj(lastDocID, score));
        }

        return ranking;
    }
}
