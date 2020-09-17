package es.uam.eps.bmi.search.index.freq.lucene;

import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.freq.TermFreq;
import java.io.IOException;
import java.util.Iterator;
import org.apache.lucene.index.Terms;

/**
 *
 * @author pablo
 */
public class LuceneFreqVector implements FreqVector {
    LuceneFreqVectorIterator iterator;
    
    public LuceneFreqVector(Terms terms) throws IOException {
        iterator = new LuceneFreqVectorIterator(terms);

    }
    public long size() throws IOException {
        return iterator.size;
    }

    public Iterator<TermFreq> iterator() {
        return iterator;
    }
    
    public long getFreq(String term) throws IOException {
        return iterator.getFreq(term);
    }
}
