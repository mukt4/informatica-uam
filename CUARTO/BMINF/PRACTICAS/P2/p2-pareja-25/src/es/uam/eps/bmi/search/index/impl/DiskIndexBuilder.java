package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.AbstractIndexBuilder;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.IndexBuilder;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import es.uam.eps.bmi.search.index.structure.impl.ImplPostingsList;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static es.uam.eps.bmi.search.index.Config.DICTIONARY_FILE;
import static es.uam.eps.bmi.search.index.Config.PATHS_FILE;
import static es.uam.eps.bmi.search.index.Config.POSTINGS_FILE;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class DiskIndexBuilder extends AbstractIndexBuilder implements IndexBuilder {

    private Map<String,String> indiceDocs;
    private Index diskIndex = null;

    public DiskIndexBuilder(){

        indiceDocs = new HashMap<>(); // Guarda par PATH/CONTENT ; HashMap
    }

    @Override
    public void build(String collectionPath, String indexPath) throws IOException {

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

        // Creamos el diccionario de terminos

        // Para todos los documentos
        for (Map.Entry<String, String> indiceDocsEntry : indiceDocs.entrySet()){

            pathsList.add(indiceDocsEntry.getKey());

            String[] terminos = indiceDocsEntry.getValue().split("\\P{Alpha}+"); // Get de todos los terminos del contenido del doc
            Map<String, Long> freqTermMap = new HashMap<>(); // Mapa (termino - freqEnDoc)

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

            numDocs++;
        }

        // A partir de aquí lo hacemos diferente que el indice en RAM

        // Creamos los ficheros necesarios para guardar el diccionario y los terminos
        // Estructura --> longTermino - nombreTermino - Offset de PostingList
        FileOutputStream diccFile = new FileOutputStream(indexPath + "/" + DICTIONARY_FILE);
        // Estructura --> nPostings - (Postings (docID, freq))*
        FileOutputStream postingFile = new FileOutputStream(indexPath + "/" + POSTINGS_FILE);

        try (
                DataOutputStream diccOut = new DataOutputStream(diccFile);
                DataOutputStream postingOut = new DataOutputStream(postingFile);
        )
        {
            for (Map.Entry<String, PostingsList> diccEntry : dictionary.entrySet()){

                long bytePos = 0;

                // (Fich Postings)

                // Escribimos la cantidad de postings del termino
                postingOut.writeInt(diccEntry.getValue().size());

                bytePos += Integer.BYTES; // Nos guardamos la cantidad de bytes escritos

                // Recorremos la lista de postings
                // Por cada uno, escribimos su docID y frecuencia en el doc
                // y seguimos acumulando lo escrito
                for (Posting p : diccEntry.getValue()){

                    postingOut.writeInt(p.getDocID());
                    bytePos += Integer.BYTES;
                    postingOut.writeLong(p.getFreq());
                    bytePos += Long.BYTES;
                }

                // (Fich Diccionario)

                //Escribimos los caracteres que ocupa el termino
                diccOut.writeInt(diccEntry.getKey().length());

                // Escribimos el termino
                diccOut.writeChars(diccEntry.getKey());

                // Offset de la PL = TamFich - BytesEscritos
                long offset = postingOut.size() - bytePos;
                diccOut.writeLong(offset);

            }
            // Cerramos los ficheros
            diccOut.close();
            postingOut.close();
        }

        // Escribimos las rutas de los ficheros en el archivo de rutas
        // Estructura: numDocs - (docID - ruta)*
        FileOutputStream pathsFile = new FileOutputStream(indexPath + "/" + PATHS_FILE);
        try (
                DataOutputStream pathsOut = new DataOutputStream(pathsFile);
        )
        {
            pathsOut.writeInt(numDocs);
            for(String path : pathsList){
                pathsOut.writeInt(pathsList.indexOf(path));
                pathsOut.writeInt(path.length());
                pathsOut.writeChars(path);
            }
            pathsOut.close();
        }

        diskIndex = new DiskIndex(indexPath);
        saveDocNorms(indexPath);
    }

    @Override
    protected void indexText(String text, String path) {

        String content = text.toLowerCase();
        indiceDocs.put(path, content);
    }

    @Override
    protected Index getCoreIndex() {

        return diskIndex;
    }
}
