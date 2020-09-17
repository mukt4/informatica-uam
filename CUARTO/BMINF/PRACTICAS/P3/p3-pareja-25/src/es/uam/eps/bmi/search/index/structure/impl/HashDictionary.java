package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.structure.EditableDictionary;
import es.uam.eps.bmi.search.index.structure.EditablePostingsList;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author pablo
 */
public class HashDictionary implements EditableDictionary {
    protected Map<String,EditablePostingsList> termPostings;

    public HashDictionary() {
        termPostings = new HashMap<String,EditablePostingsList>();
    }    

    public PostingsList getPostings(String term) {
        return termPostings.get(term);
    }

    // We assume docIDs are inserter by order
    public void add(String term, int docID) {
        if (termPostings.containsKey(term)) termPostings.get(term).add(docID);
        else termPostings.put(term, new EditablePostingsList (docID));
    }

    public void add(String term, PostingsList postings) {
        termPostings.put(term, (EditablePostingsList) postings);
    }

    public Collection<String> getAllTerms() {
        return termPostings.keySet();
    }

    @Override
    public long getDocFreq(String term) throws IOException {
        return termPostings.get(term).size();
    }
}