package es.uam.eps.bmi.search.index.structure.lucene;

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.PostingsEnum;

/**
 *
 * @author pablo
 */
public class LucenePostingsList implements PostingsList {
    protected PostingsEnum postings;
    int size;
    
    public LucenePostingsList(PostingsEnum p, int n) {
        postings = p;
        size = n;
    }

    public Iterator<Posting> iterator() {
        try {
            return new LucenePostingsIterator(postings);
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    public int size() {
        return size;
    }
}
