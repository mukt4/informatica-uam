package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.structure.EditableDictionary;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;
import es.uam.eps.bmi.search.index.structure.positional.impl.PositionalPostingsList;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PositionalDictionary implements EditableDictionary {
    private Map<String, PositionalPostingsList> positionalDictionary;

    public PositionalDictionary(){

        positionalDictionary = new HashMap<>();
    }

    public PositionalDictionary(String term, PositionalPostingsList posPostingsList){

        positionalDictionary = new HashMap<>();
        this.positionalDictionary.put(term, posPostingsList);
    }

    @Override
    public void add(String term, int docID) {

        if (positionalDictionary.containsKey(term))
            positionalDictionary.get(term).add(docID,-1);
        else
            positionalDictionary.put(term, new PositionalPostingsList (docID,-1));
    }

    public void add(String term, Posting posting) {

        if (positionalDictionary.containsKey(term)) {
            PositionalPostingsList p = positionalDictionary.get(term);
            p.add((PositionalPosting) posting);
            positionalDictionary.put(term, p);
        }
        else {
            PositionalPostingsList ppl = new PositionalPostingsList();
            ppl.add((PositionalPosting) posting);
            positionalDictionary.put(term, ppl);
        }
    }

    @Override
    public PostingsList getPostings(String term) {
        return positionalDictionary.get(term);
    }

    @Override
    public Collection<String> getAllTerms() {
        return positionalDictionary.keySet();
    }

    @Override
    public long getDocFreq(String term) {
        return positionalDictionary.get(term).size();
    }
}
