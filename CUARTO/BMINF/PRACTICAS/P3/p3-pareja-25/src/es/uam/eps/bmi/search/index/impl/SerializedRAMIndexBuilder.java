package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.Index;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

/**
 *
 * @author pablo
 */
public class SerializedRAMIndexBuilder extends BaseIndexBuilder {
    public void save(String indexPath) throws IOException {
        ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(indexPath + "/" + Config.INDEX_FILE));
        out.writeObject(dictionary);
        out.close();
    }

    protected Index getCoreIndex() {
        return new SerializedRAMIndex(dictionary, nDocs);
    }
}
