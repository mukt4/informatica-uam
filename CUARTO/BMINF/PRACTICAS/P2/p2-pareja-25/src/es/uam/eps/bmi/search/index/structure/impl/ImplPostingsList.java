package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class ImplPostingsList implements PostingsList, java.io.Serializable {

    private final List<Posting> postingsList;

    public ImplPostingsList(){

        postingsList = new ArrayList<>();
    }

    public void add(Posting posting){

        postingsList.add(posting);
    }

    @Override
    public int size() {

        return postingsList.size();
    }

    @Override
    public Iterator<Posting> iterator() {

        return new ImplPostingsListIterator(postingsList);
    }
}
