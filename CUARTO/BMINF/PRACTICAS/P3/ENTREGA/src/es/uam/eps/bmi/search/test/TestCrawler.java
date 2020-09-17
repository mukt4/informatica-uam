package es.uam.eps.bmi.search.test;

import es.uam.eps.bmi.search.SearchEngine;
import es.uam.eps.bmi.search.index.WebCrawler;
import es.uam.eps.bmi.search.index.lucene.LuceneBuilder;
import es.uam.eps.bmi.search.lucene.LuceneEngine;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import es.uam.eps.bmi.search.ui.TextResultDocRenderer;
import es.uam.eps.bmi.search.util.Timer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class TestCrawler {
    public static void main (String a[]) throws IOException {
        WebCrawler wc = new WebCrawler(100, "test/semillas.txt", "test/");
        wc.runCrawler();

        // Contamos el numero de URLs encontradas y limpiamos las URLS mal formadas
        File file = new File("test/urlsWebCrawler.txt");
        FileInputStream fis = new FileInputStream(file);
        byte[] byteArray = new byte[(int)file.length()];
        fis.read(byteArray);
        String data = new String(byteArray);
        String[] stringArray = data.split("\n");
        System.out.println("URLs encontradas: " + stringArray.length);

        // Una vez crawleadas las paginas pasamos a crear el indice lucene
        // Creación del indice
        Timer.reset("Creacion de indice lucene: ");
        new LuceneBuilder().build( "test/urlsWebCrawler.txt", "test_out/regular/");
        Timer.time("--> ");

        // Probamos a realizar una consulta
        testSearch("urls", new LuceneEngine("test_out/regular"), "coronavirus", 5);
        testSearch("urls", new LuceneEngine("test_out/regular"), "barcelona", 5);
    }

    static void testSearch (String collName, SearchEngine engine, String query, int cutoff) throws IOException {
        System.out.println("-----------------------");
        System.out.println("Checking search results on " + collName + " collection");
        SearchRanking ranking = engine.search(query, cutoff);
        System.out.println("  " + engine.getClass().getSimpleName()
                + " + " + engine.getDocMap().getClass().getSimpleName()
                + ": top " + ranking.size() + " of " + ranking.nResults() + " for query '" + query + "'");
        for (SearchRankingDoc result : ranking)
            System.out.println("\t" + new TextResultDocRenderer(result));
    }
}
