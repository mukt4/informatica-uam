package es.uam.eps.bmi.search.index.structure.positional.lucene;

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.lucene.LucenePostingsList;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.PostingsEnum;

/**
 *
 * @author pablo
 */
public class LucenePositionalPostingsList extends LucenePostingsList {
    PostingsEnum positionPostings;
    
    public LucenePositionalPostingsList(PostingsEnum p1, PostingsEnum p2, int n) {
        super(p1, n);
        positionPostings = p2;
    }

    public Iterator<Posting> iterator() {
        try {
            return new LucenePositionalPostingsIterator(postings, positionPostings);
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
    }
}
