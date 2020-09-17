package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.AbstractIndexBuilder;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.structure.EditableDictionary;
import es.uam.eps.bmi.search.index.structure.impl.HashDictionary;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author pablo
 */
public abstract class BaseIndexBuilder extends AbstractIndexBuilder {
    int nDocs;
    EditableDictionary dictionary;
    List<String> docPaths;

    public abstract void save(String indexPath) throws IOException;

    public void build (String collectionPath, String indexPath) throws IOException {
        init(indexPath);
        indexDocuments(collectionPath);
        close(indexPath);
    }

    public void init(String indexPath) throws IOException {
        clear(indexPath);
        nDocs = 0;
        dictionary = new HashDictionary();
        docPaths = new ArrayList<String>();
    }

    public void close(String indexPath) throws IOException {
        save(indexPath);
        saveDocPaths(indexPath);
        saveDocNorms(indexPath);
    }
    
    public void saveDocPaths(String indexPath) throws IOException {
        PrintStream out = new PrintStream(indexPath + "/" + Config.PATHS_FILE);
        out.println(nDocs);
        for (String path : docPaths)
            out.println(path);
        out.close();
    }

    public void indexText(String text, String path) throws IOException {
        for (String term : text.toLowerCase().split("\\P{Alpha}+"))
            dictionary.add(term, nDocs);
        docPaths.add(path);
        nDocs++;
    }

    protected Index getCoreIndex() {
        return new BaseIndex(dictionary, nDocs);
    }
}
