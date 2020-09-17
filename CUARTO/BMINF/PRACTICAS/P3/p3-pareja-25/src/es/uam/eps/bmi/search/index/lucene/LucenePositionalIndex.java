package es.uam.eps.bmi.search.index.lucene;

import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.positional.lucene.LucenePositionalPostingsList;
import java.io.IOException;
import org.apache.lucene.index.MultiFields;
import org.apache.lucene.index.MultiTerms;
import org.apache.lucene.index.PostingsEnum;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.util.BytesRef;

/**
 *
 * @author pablo
 * @author javier
 */
public class LucenePositionalIndex extends LuceneIndex {
    
    public LucenePositionalIndex(String path) throws IOException {
        super(path);
    }

    public PostingsList getPostings(String term) throws IOException {
        TermsEnum terms = MultiTerms.getTerms(index, "content").iterator();
        terms.seekExact(new BytesRef(term));
        return new LucenePositionalPostingsList(terms.postings(null), terms.postings(null, PostingsEnum.ALL), terms.docFreq());
    }
}
