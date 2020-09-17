package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.AbstractIndex;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.impl.ImplPostingsList;

import java.io.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import static es.uam.eps.bmi.search.index.Config.*;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class DiskIndex extends AbstractIndex {

    private Map<String, Long> dictionary;
    private String indexFolder;

    public DiskIndex(String indexFolder) throws IOException {

        dictionary = new HashMap<>();
        this.indexFolder = indexFolder;

        // Leemos el archivo
        FileInputStream diccFile = new FileInputStream(indexFolder + "/" + DICTIONARY_FILE);
        try (
                DataInputStream diccIn = new DataInputStream(diccFile)
        )
        {
            while(diccIn.available() > 0) {

                int longitud = diccIn.readInt();
                StringBuilder termino = new StringBuilder();
                for (int i = 0; i < longitud; i++)
                    termino.append(diccIn.readChar());
                Long offset = diccIn.readLong();
                dictionary.put(termino.toString(), offset);
            }
            diccIn.close();
        }
        if (new File(indexFolder + "/" + NORMS_FILE).exists())
            loadNorms(indexFolder);
    }

    @Override
    public int numDocs() {

        try {
            RandomAccessFile pathsFile = new RandomAccessFile(indexFolder + "/" + PATHS_FILE, "r");
            pathsFile.seek(0); // Numero de documentos está al principio del fichero
            return pathsFile.readInt();
        } catch (IOException ex) {
            Logger.getLogger(DiskIndex.class.getName()).log(Level.SEVERE, null, ex);
        }
        return -1;
    }

    @Override
    public PostingsList getPostings(String term) throws IOException {

        if (dictionary.get(term) == null)
            return null;

        // Objeto que nos permitirá hacer seek a las diferentes posiciones del fichero
        RandomAccessFile postingFile = new RandomAccessFile(indexFolder + "/" + POSTINGS_FILE, "r");

        postingFile.seek(dictionary.get(term)); // Seek al empiece de los postings del termino
        int numPostings = postingFile.readInt(); // Get de todos los postings
        ImplPostingsList postingList = new ImplPostingsList();
        // Get de los atributos y creamos un nuevo posting añadiendolo a la lista
        for (int i = 0; i < numPostings; i++){
            int docID = postingFile.readInt();
            long freq = postingFile.readLong();
            postingList.add(new Posting(docID, freq));
        }
        postingFile.close();

        if (new File(indexFolder + "/" + NORMS_FILE).exists())
            loadNorms(indexFolder);

        return postingList;
    }

    @Override
    public Collection<String> getAllTerms() throws IOException {

        return dictionary.keySet();
    }

    @Override
    public long getTotalFreq(String term) throws IOException {

        if (dictionary.get(term) == null)
            return -1;

        // Objeto que nos permitirá hacer seek a las diferentes posiciones del fichero
        RandomAccessFile postingFile = new RandomAccessFile(indexFolder + "/" + POSTINGS_FILE, "r");

        postingFile.seek(dictionary.get(term)); // Seek al empiece de los postings del termino
        int numPostings = postingFile.readInt(); // Get de todos los postings
        long freqTotalTerm = 0;

        // Get de los atributos y creamos un nuevo posting añadiendolo a la lista
        for (int i = 0; i < numPostings; i++){
            postingFile.skipBytes(Integer.BYTES); // Nos saltamos el docID
            freqTotalTerm += postingFile.readLong(); // Acumulamos la frecuencia total
        }
        postingFile.close();

        return freqTotalTerm;
    }

    @Override
    public long getDocFreq(String term) throws IOException {

        // Objeto que nos permitirá hacer seek a las diferentes posiciones del fichero
        RandomAccessFile postingFile = new RandomAccessFile(indexFolder + "/" + POSTINGS_FILE, "r");

        if (dictionary.get(term) == null)
            return -1;

        postingFile.seek(dictionary.get(term)); // Seek al empiece de los postings del termino

        return postingFile.readInt();
    }

    @Override
    public String getDocPath(int docID) throws IOException {

        StringBuilder path = new StringBuilder();
        try (RandomAccessFile pathsFile = new RandomAccessFile(indexFolder + "/" + PATHS_FILE, "r")) {
            pathsFile.seek(0); // Nos situamos al inicio del fichero
            pathsFile.skipBytes(Integer.BYTES); // Avanzamos el numDocs

            // Hasta que no encontremos el docID que necesitamos
            while(true){

                int docIDFich = pathsFile.readInt();  // Leemos el siguiente docID
                if (docIDFich == docID)  // Si son iguales nos salimos
                    break;
                int longitud = pathsFile.readInt();  // Si no, leemos lo que ocupa la siguiente ruta
                pathsFile.skipBytes(Character.BYTES*longitud);  // Nos la saltamos
            }

            int longitud = pathsFile.readInt();  // Leemos la longitud de la ruta que deseamos
            for (int i = 0; i < longitud; i++)  // Leemos la ruta caracter a caracter
                path.append(pathsFile.readChar());
        }
        return path.toString();
    }
}
