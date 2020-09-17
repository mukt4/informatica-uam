package es.uam.eps.bmi.search.vsm;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class TermBasedVSMCosineEngine extends AbstractVSMEngine {

    public TermBasedVSMCosineEngine(Index index) {
        super(index);
    }

    @Override
    public SearchRanking search(String query, int cutoff) throws IOException {
        String[] terms = query.split(" ");
        Map<Integer,Long> acumulador = new HashMap<>();
        List<Integer> listDocIDs = new ArrayList<>();

        for (String term : terms){

            PostingsList postingList = index.getPostings(term);

            // Creamos los acumuladores
            for (Posting posting : postingList){
                if (!listDocIDs.contains(posting.getDocID()))
                    listDocIDs.add(posting.getDocID());

                if (!acumulador.containsKey(posting.getDocID())){
                    acumulador.put(posting.getDocID(), posting.getFreq());
                } else {
                    acumulador.put(posting.getDocID(), acumulador.get(posting.getDocID()) + posting.getFreq());
                }
            }
        }

        // Si acumulador no tiene ninguna entrada
        if (acumulador.isEmpty())
            return new RankingImpl(index, cutoff);

        // Dividimos por el modulo
        Map<Integer, Double> parcialRanking = new HashMap<>();
        for (Integer listDocID : listDocIDs)
            parcialRanking.put(listDocID, acumulador.get(listDocID) / index.getDocNorm(listDocID));

        RankingImpl ranking = new RankingImpl(index, cutoff);
        for (Integer docID : listDocIDs)
            ranking.add(docID, parcialRanking.get(docID));

        return ranking;
    }
}
