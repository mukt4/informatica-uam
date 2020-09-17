/*
 *  Copyright (C) 2020 Pablo Castells y Javier Sanz-Cruzado
 *
 *  Este código se ha implementado para la realización de las prácticas de
 *  la asignatura "Búsqueda y minería de información" de 4º del Grado en
 *  Ingeniería Informática, impartido en la Escuela Politécnica Superior de
 *  la Universidad Autónoma de Madrid. El fin del mismo, así como su uso,
 *  se ciñe a las actividades docentes de dicha asignatura.
 *
 */
package es.uam.eps.bmi.search.test;

import es.uam.eps.bmi.search.SearchEngine;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.IndexBuilder;
import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.freq.TermFreq;
import es.uam.eps.bmi.search.index.lucene.LuceneIndex;
import es.uam.eps.bmi.search.index.lucene.LuceneBuilder;
import es.uam.eps.bmi.search.lucene.LuceneEngine;
//import es.uam.eps.bmi.search.vsm.VSMCosineEngine;
import es.uam.eps.bmi.search.vsm.VSMDotProductEngine;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import es.uam.eps.bmi.search.ui.TextResultDocRenderer;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 *
 * @author Pablo Castells
 */
public class TestEngine {
    public static void main (String a[]) throws IOException {
        testCollection ("src/es/uam/eps/bmi/search/ranking", "index/src", "size", "public abstract");
        testCollection ("collections/docs1k.zip", "index/docs", "seat", "obama family tree");
        testCollection ("collections/urls.txt", "index/urls", "wikipedia", "information probability");
    }
    
    static void testCollection(String collectionPath, String indexPath, String word, String query) throws IOException {
        System.out.println("=================================================================\n");
        System.out.println("Testing indices and search on " + collectionPath);

        // Creamos la carpeta si no existe y borramos índice si existía.
        
        clear(indexPath);
        
        // Prueba de creación de índice
        
        IndexBuilder builder = new LuceneBuilder();
        builder.build(collectionPath, indexPath);
        
        // Pruebas de inspección del índice
        
        Index index = new LuceneIndex(indexPath);
        List<String> terms = new ArrayList<String>(index.getAllTerms());
        Collections.sort(terms, new Comparator<String>() {
            public int compare(String t1, String t2) {
                try {
                    return (int) Math.signum(index.getTotalFreq(t2)- index.getTotalFreq(t1));
                } catch (IOException ex) {
                    ex.printStackTrace();
                    return 0;
                }
            }
        });
        
        System.out.println("\n  Most frequent terms:");
        for (String term : terms.subList(0, 5))
            System.out.println("\t" + term + "\t" + index.getTotalFreq(term));
        
        int docID = 0;
        FreqVector vector = index.getDocVector(docID);
        int initialTerm = (int) vector.size() / 2, nTerms = 5;
        System.out.print("\n  A few term frequencies for docID = " + docID + " - " + index.getDocPath(docID) + ": ");
        int i = 0;
        for (TermFreq f : vector) 
            if (++i >= initialTerm && i < initialTerm + nTerms) System.out.print(f.getTerm() + " (" + f.getFreq() + ") ");
        System.out.println();
        System.out.println("\n  Frequency of word \"" + word + "\" in document " + docID + " - " + index.getDocPath(docID) + ": " + index.getTermFreq(word, docID));
        System.out.println("\n  Total frequency of word \"" + word + "\" in the collection: " + index.getTotalFreq(word) + " occurrences over " + index.getDocFreq(word) + " documents\n");
        
        // Pruebas de búsqueda

        System.out.println("------------------------------\n");
        System.out.println("Checking search results");

        testSearch (new LuceneEngine(indexPath), query, 5);
        testSearch (new VSMDotProductEngine(new LuceneIndex(indexPath)), query, 5);
        //testSearch (new VSMCosineEngine(new LuceneIndex(indexPath)), query, 5);
    }
    
    static void testSearch (SearchEngine engine, String query, int cutoff) throws IOException {
        System.out.println("  " + engine.getClass().getSimpleName()
                + ": top " + cutoff + " for query '" + query + "'");
        SearchRanking ranking = engine.search(query, cutoff);
        for (SearchRankingDoc result : ranking)
            System.out.println("\t" + new TextResultDocRenderer(result));
        System.out.println();
    }

    static void clear (String indexPath) throws IOException {
        File dir = new File(indexPath);
        if (!dir.exists()) Files.createDirectories(Paths.get(indexPath));
        for (File f : dir.listFiles()) if (f.isFile()) f.delete();
    }
}
