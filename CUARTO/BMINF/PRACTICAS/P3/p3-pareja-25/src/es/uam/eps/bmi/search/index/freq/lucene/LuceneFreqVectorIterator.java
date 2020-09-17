package es.uam.eps.bmi.search.index.freq.lucene;

import es.uam.eps.bmi.search.index.freq.TermFreq;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.Terms;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.util.BytesRef;

/**
 *
 * @author pablo
 */
public class LuceneFreqVectorIterator implements Iterator<TermFreq> {
    TermsEnum terms;
    long size;
    long pointer;

    public LuceneFreqVectorIterator(Terms t) throws IOException {
        terms = t.iterator();
        size = t.size();
        pointer = 0;
    }
    
    public boolean hasNext() {
        return pointer < size;
    }
    
    public TermFreq next() {
        try {
            terms.next();
            pointer++;
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return new LuceneTermFreq(terms);
    }
    
    public long getFreq(String term) throws IOException {
        if (terms.seekExact(new BytesRef(term))) return terms.totalTermFreq();
        else return 0;
    }
}
