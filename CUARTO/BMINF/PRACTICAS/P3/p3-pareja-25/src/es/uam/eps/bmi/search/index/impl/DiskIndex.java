package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.structure.Dictionary;
import es.uam.eps.bmi.search.index.structure.impl.DiskHashDictionary;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class DiskIndex extends BaseIndex {
    public DiskIndex(String indexFolder) throws IOException {
        super(indexFolder);
        dictionary = new DiskHashDictionary(indexFolder);
        ((DiskHashDictionary) dictionary).load();
    }

    public DiskIndex(Dictionary dic, int nDocs) {
        super(dic, nDocs);
    }
}
