package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.structure.Dictionary;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 *
 * @author pablo
 */
public class SerializedRAMIndex extends BaseIndex implements Serializable {
    
    public SerializedRAMIndex(String indexFolder) throws IOException {
        super(indexFolder);
        File f = new File(indexFolder + "/" + Config.INDEX_FILE);
        if (!f.exists()) throw new NoIndexException(indexFolder);
        ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
        try {
            dictionary = (Dictionary) in.readObject();
        } catch (ClassNotFoundException ex) {
            ex.printStackTrace();
        }
    }

    public SerializedRAMIndex(Dictionary dic, int nDocs) {
        super(dic, nDocs);
    }
}
