package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.structure.Posting;

import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class ImplPostingsListIterator implements Iterator<Posting> {

    private List<Posting> postingsList;
    private int n;

    public ImplPostingsListIterator(List<Posting> pl){

        postingsList = pl;
        n = 0;
    }

    @Override
    public boolean hasNext() {

        return n < postingsList.size();
    }

    @Override
    public Posting next() {

        int docID = postingsList.get(n).getDocID();
        long freq = postingsList.get(n).getFreq();
        n++;
        return new Posting(docID, freq);
    }
}
