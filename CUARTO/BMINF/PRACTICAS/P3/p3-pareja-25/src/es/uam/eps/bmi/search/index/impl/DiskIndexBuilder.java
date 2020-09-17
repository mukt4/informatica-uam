package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.impl.DiskHashDictionary;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 *
 * @author pablo
 */
public class DiskIndexBuilder extends BaseIndexBuilder {
    public void save(String indexFolder) throws IOException {
        DiskHashDictionary dict = new DiskHashDictionary(indexFolder);
        RandomAccessFile postingsFile = new RandomAccessFile(indexFolder + "/" + Config.POSTINGS_FILE, "rw");
        long address = 0;
        for (String term : dictionary.getAllTerms()) {
            PostingsList postings = dictionary.getPostings(term);
            postingsFile.writeInt(postings.size());
            for (Posting p : postings) {
                postingsFile.writeInt(p.getDocID());
                postingsFile.writeLong(p.getFreq());
            }
            dict.add(term, address);
            address = postingsFile.getFilePointer();
        }
        postingsFile.close();
        dict.save();
    }
}
