package es.uam.eps.bmi.search.index;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;

/**
 *
 * @author pablo
 * @author javier
 */
public abstract class AbstractIndex implements Index {
    protected double docNorms[];
    
    public double getDocNorm(int docID) throws IOException {
        return docNorms[docID];
    }

    public void loadNorms(String indexFolder) throws FileNotFoundException {
        File f = new File(indexFolder + "/" + Config.NORMS_FILE);
        if (!f.exists()) return;
        Scanner scn = new Scanner(f);
        docNorms = new double[numDocs()];
        for (int docID = 0; docID < docNorms.length; docID++)
            docNorms[docID] = Double.parseDouble(scn.nextLine());
        scn.close();
    }
}
