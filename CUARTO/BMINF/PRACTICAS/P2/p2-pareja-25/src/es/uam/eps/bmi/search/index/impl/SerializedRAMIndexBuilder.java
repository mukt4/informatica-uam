package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.AbstractIndexBuilder;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.IndexBuilder;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.impl.ImplPostingsList;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import static es.uam.eps.bmi.search.index.Config.DICTIONARY_FILE;
import static es.uam.eps.bmi.search.index.Config.INDEX_FILE;
import static es.uam.eps.bmi.search.index.Config.PATHS_FILE;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class SerializedRAMIndexBuilder extends AbstractIndexBuilder implements IndexBuilder, java.io.Serializable  {

    private Map<String,String> indiceDocs;
    private Index ramIndex = null;

    // Creas un index y le pasas el mapa para que lo guarde ahí.

    public SerializedRAMIndexBuilder(){

        indiceDocs = new HashMap<>(); // Guarda par PATH/CONTENT ; HashMap
    }

    @Override
    public void build (String collectionPath, String indexPath) throws IOException {

        File fileCollectionPath = new File(collectionPath);
        if (!fileCollectionPath.exists()){
            System.out.println("El archivo \"" + collectionPath + "\" no se ha encontrado en \"" + indexPath + "\".");
            return;
        }

        // Limpiamos el indice
        clear(indexPath);

        // Creamos el indice (Ruta - Contenido)

        if (collectionPath.endsWith(".txt")){

            indexURLs(fileCollectionPath);

        } else if (collectionPath.endsWith(".zip")){

            indexZip(fileCollectionPath);

        } else if (fileCollectionPath.isDirectory()){

            indexFolder(fileCollectionPath);
        }

        int numDocs = 0;
        List<String> pathsList = new ArrayList<>();
        Map<String, PostingsList> dictionary = new HashMap<>();

        // Para ejercicio Ley de Heap
        //int palabras = 0;
        //int tamañoDicc = 0;
        //FileWriter fichero = new FileWriter("/home/andres/Escritorio/leyHeap.txt");
        //PrintWriter pw = new PrintWriter(fichero);

        // Creamos el diccionario de terminos
        for (Map.Entry<String, String> indiceDocsEntry : indiceDocs.entrySet()){

            pathsList.add(indiceDocsEntry.getKey());

            String[] terminos = indiceDocsEntry.getValue().split("\\P{Alpha}+"); // Get de todos los terminos del contenido del doc
            Map<String, Long> freqTermMap = new HashMap<>(); // Mapa (termino - freqEnDoc)

            //palabras += terminos.length; // Para ejercicio Ley de Heap

            // Para todos los terminos
            for(String termino : terminos){

                // Si ya existe una entrada para el termino, sumamos 1 frecuencia
                if (freqTermMap.containsKey(termino)){
                    Long freq = freqTermMap.get(termino);
                    freq++;
                    freqTermMap.put(termino, freq);
                }
                // Si no, creamos una entrada de frecuencia 1, la primera vez que lo vemos
                else {
                    freqTermMap.put(termino, 1L);
                }
            }

            // Una vez hemos acabado de leer el fichero entero, actualizamos el diccionario

            // Para todas las entradas del mapa de frecuencias de terminos en el doc
            for (Map.Entry<String, Long> freqTermMapEntry : freqTermMap.entrySet()){

                String termino = freqTermMapEntry.getKey();
                Long freq = freqTermMapEntry.getValue();

                // Actualizamos la PL del termino añadiendo el nuevo posting a la lista
                if (dictionary.containsKey(termino)){
                    ImplPostingsList pl = (ImplPostingsList) dictionary.get(termino);
                    pl.add(new Posting(numDocs, freq));
                    dictionary.put(termino, pl);
                }
                // Si no, creamos una nueva entrada
                else {
                    ImplPostingsList pl = new ImplPostingsList();
                    pl.add(new Posting(numDocs, freq));
                    dictionary.put(termino, pl);
                }
            }

            //tamañoDicc = dictionary.size(); // Para ejercicio Ley de Heap

            //pw.println(palabras + "\t" + tamañoDicc);

            numDocs++;
        }

        // A partir de aquí lo hacemos diferente que el indice en Disco

        // Serializamos de una vez todos los objetos creados
        saveObject(indiceDocs, indexPath + "/" + INDEX_FILE);
        saveObject(pathsList, indexPath + "/" + PATHS_FILE);
        saveObject(dictionary, indexPath + "/" + DICTIONARY_FILE);

        try {
            ramIndex = new SerializedRAMIndex(indexPath);
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(SerializedRAMIndexBuilder.class.getName()).log(Level.SEVERE, null, ex);
        }
        saveDocNorms(indexPath);
    }

    @Override
    protected void indexText(String text, String path) {

        String content = text.toLowerCase();
        indiceDocs.put(path, content);
    }

    @Override
    protected Index getCoreIndex() {

        return ramIndex;
    }

    private void saveObject(Object o, String fileString){

        try {
            FileOutputStream fileOut = new FileOutputStream(fileString);
            ObjectOutputStream out = new ObjectOutputStream(fileOut);
            out.writeObject(o);
            out.close();
            fileOut.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
