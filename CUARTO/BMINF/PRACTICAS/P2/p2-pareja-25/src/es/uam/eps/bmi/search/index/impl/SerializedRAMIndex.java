package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.AbstractIndex;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static es.uam.eps.bmi.search.index.Config.DICTIONARY_FILE;
import static es.uam.eps.bmi.search.index.Config.NORMS_FILE;
import static es.uam.eps.bmi.search.index.Config.PATHS_FILE;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class SerializedRAMIndex extends AbstractIndex {

    private int numDocs;
    private Map<String, PostingsList> dictionary;
    private List<String> pathsList;
    private String indexFolder;

    public SerializedRAMIndex(String indexFolder) throws IOException, ClassNotFoundException {

        if (indexFolder.equals("") || !new File(indexFolder).exists()){
            throw new NoIndexException(indexFolder);
        }

        dictionary = (Map) loadObject(indexFolder + "/" + DICTIONARY_FILE);
        pathsList = (List) loadObject(indexFolder + "/" + PATHS_FILE);
        numDocs = pathsList.size();
        this.indexFolder = indexFolder;

        if (new File(indexFolder + "/" + NORMS_FILE).exists())
            loadNorms(indexFolder);
    }

    private Object loadObject(String fileString) throws ClassNotFoundException{

        Object o;
        try {
            FileInputStream fileIn = new FileInputStream(fileString);
            ObjectInputStream in = new ObjectInputStream(fileIn);
            o = in.readObject();
            in.close();
            fileIn.close();
        } catch (IOException e) {
            e.printStackTrace();
            return e;
        }
        return o;

    }

    public String getIndexFolder(){

        return indexFolder;
    }

    @Override
    public int numDocs() {

        return numDocs;
    }

    @Override
    public PostingsList getPostings(String term) {

        return dictionary.get(term);
    }

    @Override
    public Collection<String> getAllTerms() {

        return dictionary.keySet();
    }

    @Override
    public long getTotalFreq(String term) {

        long freq = 0;

        for (Posting p : dictionary.get(term)){
            freq += p.getFreq();
        }

        return freq;
    }

    @Override
    public long getDocFreq(String term) {

        return dictionary.get(term).size();
    }

    @Override
    public String getDocPath(int docID) {

        return pathsList.get(docID);
    }
}
