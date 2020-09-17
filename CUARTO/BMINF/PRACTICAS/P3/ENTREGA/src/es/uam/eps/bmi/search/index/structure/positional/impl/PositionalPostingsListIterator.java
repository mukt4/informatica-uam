package es.uam.eps.bmi.search.index.structure.positional.impl;

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PositionalPostingsListIterator implements Iterator<Posting> {
    private List<PositionalPosting> positionalPostingsList;
    private int n;

    PositionalPostingsListIterator(List<PositionalPosting> positionalPostingsList){
        this.positionalPostingsList = positionalPostingsList;
        n = 0;
    }

    public PositionalPostingsListIterator(){
        this.positionalPostingsList = new ArrayList<>();
        n = 0;
    }

    public boolean hasNext() {
        return n < positionalPostingsList.size();
    }

    public Posting next() {
        Posting nextPosting = new PositionalPosting(positionalPostingsList.get(n).getDocID(),
                positionalPostingsList.get(n).getFreq(),
                positionalPostingsList.get(n).getPositions());
        n++;
        return nextPosting;
    }
}
