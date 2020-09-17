package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.structure.Dictionary;
import es.uam.eps.bmi.search.index.structure.impl.PositionalDictionary;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PositionalIndex extends BaseIndex {
    public PositionalIndex(String indexFolder) throws IOException {
        super(indexFolder);
        File f = new File(indexFolder + "/" + Config.DICTIONARY_FILE);
        if (!f.exists()) throw new NoIndexException(indexFolder);
        ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
        try {
            dictionary = (PositionalDictionary) in.readObject();
        } catch (ClassNotFoundException ex) {
            ex.printStackTrace();
        }
    }

    public PositionalIndex(Dictionary dic, int nDocs) {
        super(dic, nDocs);
    }
}
