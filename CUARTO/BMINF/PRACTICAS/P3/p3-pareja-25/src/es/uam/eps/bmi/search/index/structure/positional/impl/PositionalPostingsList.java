package es.uam.eps.bmi.search.index.structure.positional.impl;

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PositionalPostingsList implements PostingsList, Serializable {
    private List<PositionalPosting> positionalPostingsList;

    public PositionalPostingsList(int docID, int pos){
        this.positionalPostingsList = new ArrayList<>();
        positionalPostingsList.add(new PositionalPosting(docID, 1, new ArrayList(pos)));
    }

    public PositionalPostingsList(List<PositionalPosting> postingsList){
        this.positionalPostingsList = postingsList;
    }

    public PositionalPostingsList(){
        this.positionalPostingsList = new ArrayList<>();
    }

    public void add(int docID, int pos){
        positionalPostingsList.add(new PositionalPosting(docID, 1, new ArrayList(pos)));
    }

    public void add(PositionalPosting posPosting){
        positionalPostingsList.add(posPosting);
    }

    @Override
    public int size() {
        return positionalPostingsList.size();
    }

    @Override
    public Iterator<Posting> iterator() {
        return new PositionalPostingsListIterator(positionalPostingsList);
    }
}
