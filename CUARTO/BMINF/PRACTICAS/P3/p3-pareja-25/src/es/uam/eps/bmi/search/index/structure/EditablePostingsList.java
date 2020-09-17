package es.uam.eps.bmi.search.index.structure;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author pablo
 */
public class EditablePostingsList implements PostingsList, Serializable {
    protected List<Posting> postings;

    public EditablePostingsList() {
        postings = new ArrayList<Posting>();
    }
    
    public EditablePostingsList(int docID) {
        this();
        add(docID);
    }
    
    public int size() {
        return postings.size();
    }

    public Iterator<Posting> iterator() {
        return postings.iterator();
    }
    
    // docIDs are supposed to be added by increasing docID
    public void add(int docID, long freq) {
        add(new Posting(docID, freq));
    }
    public void add(int docID) {
        if (!postings.isEmpty() && docID == postings.get(postings.size() - 1).getDocID())
        postings.get(postings.size() - 1).add1();
        else add(docID, 1);
    }

    protected void add(Posting posting) {
        postings.add(posting);
    }
}
