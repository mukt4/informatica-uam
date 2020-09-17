package es.uam.eps.bmi.search.index.lucene;

import es.uam.eps.bmi.search.index.AbstractIndex;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.lucene.LucenePostingsList;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.lucene.index.*;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;

/**
 *
 * @author pablo
 * @author javier
 */
public class LuceneIndex extends AbstractIndex {
    IndexReader index;
    
    public LuceneIndex(String indexFolder) throws IOException {
        try {
            index = DirectoryReader.open(FSDirectory.open(Paths.get(indexFolder)));
            loadNorms(indexFolder);
        } catch (IndexNotFoundException ex) {
            throw new NoIndexException(indexFolder);
        }
    }

    public String getDocPath(int docID) throws IOException {
        return index.document(docID).get("path");
    }
    
    public Collection<String> getAllTerms() throws IOException {
        List<String> termList = new ArrayList<String>();
        TermsEnum terms = MultiTerms.getTerms(index, "content").iterator();
        while (terms.next() != null) 
            termList.add(terms.term().utf8ToString());
        return termList;
    }
    
    public int numDocs() {
        return index.numDocs();
    }

    public long getDocFreq(String term) throws IOException {
        return index.docFreq(new Term("content", term));
    }

    public long getTotalFreq(String term) throws IOException {
        return index.totalTermFreq(new Term("content", term));
    }
    
    public PostingsList getPostings(String term) throws IOException {
        TermsEnum terms = MultiTerms.getTerms(index, "content").iterator();
        if(terms.seekExact(new BytesRef(term)))
            return new LucenePostingsList(terms.postings(null), terms.docFreq());
        return new LucenePostingsList(null, 0);
    }
}