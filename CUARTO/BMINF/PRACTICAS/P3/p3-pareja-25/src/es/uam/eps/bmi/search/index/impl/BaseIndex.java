package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.AbstractIndex;
import es.uam.eps.bmi.search.index.structure.Dictionary;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.Scanner;

/**
 *
 * @author pablo
 */
public class BaseIndex extends AbstractIndex {
    Dictionary dictionary;
    int numDocs;
    String docPaths[];
    
    public BaseIndex(String indexFolder) throws IOException {
        loadPaths(indexFolder);
        loadNorms(indexFolder);
    }

    public BaseIndex(Dictionary dic, int nDocs) {
        dictionary = dic;
        numDocs = nDocs;
    }
    
    public int numDocs() {
        return numDocs;
    }

    public PostingsList getPostings(String term) throws IOException {
        return dictionary.getPostings(term);
    }

    public Collection<String> getAllTerms() throws IOException {
        return dictionary.getAllTerms();
    }

    public long getTotalFreq(String term) throws IOException {
        long freq = 0;
        for (Posting p : getPostings(term)) freq += p.getFreq();
        return freq;
    }

    public long getDocFreq(String term) throws IOException {
        return dictionary.getDocFreq(term);
    }

    public String getDocPath(int docID) {
        return docPaths[docID];
    }

    public void loadPaths(String path) throws FileNotFoundException {
        File f = new File(path + "/" + Config.PATHS_FILE);
        if (!f.exists()) return;
        Scanner scn = new Scanner(f);
        numDocs = new Integer(scn.nextLine());
        docPaths = new String[numDocs];
        for (int docID = 0; docID < numDocs; docID++)
            docPaths[docID] = scn.nextLine();
        scn.close();
    }
}
