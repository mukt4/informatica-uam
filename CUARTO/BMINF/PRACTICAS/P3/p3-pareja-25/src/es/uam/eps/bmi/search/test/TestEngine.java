package es.uam.eps.bmi.search.test;

import es.uam.eps.bmi.search.SearchEngine;
import es.uam.eps.bmi.search.StaticEngine;
import es.uam.eps.bmi.search.graph.PageRank;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.impl.PositionalIndex;
import es.uam.eps.bmi.search.index.impl.PositionalIndexBuilder;
import es.uam.eps.bmi.search.index.lucene.LuceneIndex;
import es.uam.eps.bmi.search.index.lucene.LuceneBuilder;
import es.uam.eps.bmi.search.index.lucene.LucenePositionalIndex;
import es.uam.eps.bmi.search.index.lucene.LucenePositionalIndexBuilder;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;
import es.uam.eps.bmi.search.lucene.LuceneEngine;
import es.uam.eps.bmi.search.proximity.ProximityEngine;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import es.uam.eps.bmi.search.ui.TextResultDocRenderer;
import es.uam.eps.bmi.search.util.Timer;
import es.uam.eps.bmi.search.vsm.DocBasedVSMCosineEngine;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class TestEngine {
    public static void main (String a[]) throws IOException {
        System.out.println("=======================");
        System.out.println("Building indices...");
        
        System.out.println("-----------------------");
        System.out.println("Toy collection");
        new LuceneBuilder().build("collections/toy", "index/toy/lucene/regular");
        new LucenePositionalIndexBuilder().build("collections/toy", "index/toy/lucene/positional");
        new PositionalIndexBuilder().build("collections/toy", "index/toy/positional");

        System.out.println("-----------------------");
        System.out.println("URLs collection");
        new LuceneBuilder().build("collections/urls.txt", "index/urls/lucene/regular");
        new LucenePositionalIndexBuilder().build("collections/urls.txt", "index/urls/lucene/positional");
        new PositionalIndexBuilder().build("collections/urls.txt", "index/urls/positional");

        System.out.println("-----------------------");
        Timer.reset("1k collection");
        new LuceneBuilder().build("collections/docs1k.zip", "index/1k/lucene/regular");
        new LucenePositionalIndexBuilder().build("collections/docs1k.zip", "index/1k/lucene/positional");
        new PositionalIndexBuilder().build("collections/docs1k.zip", "index/1k/positional");
        Timer.time("--> ");

        System.out.println("=======================");
        System.out.println("Checking indices...");
        
        System.out.println("-----------------------");
        System.out.println("Toy collection");
        testIndex(new LuceneIndex("index/toy/lucene/regular"), "a");
        testIndex(new LucenePositionalIndex("index/toy/lucene/positional"), "a");
        testIndex(new PositionalIndex("index/toy/positional"), "a");

        testIndex(new LuceneIndex("index/toy/lucene/regular"), "sleep");
        testIndex(new LucenePositionalIndex("index/toy/lucene/positional"), "sleep");
        testIndex(new PositionalIndex("index/toy/positional"), "sleep");

        System.out.println("-----------------------");
        System.out.println("URLs collection");
        testIndex(new LuceneIndex("index/urls/lucene/regular"), "channel");
        testIndex(new LucenePositionalIndex("index/urls/lucene/positional"), "channel");
        testIndex(new PositionalIndex("index/urls/positional"), "channel");

        System.out.println("-----------------------");
        System.out.println("1k collection");
        testIndex(new LuceneIndex("index/1k/lucene/regular"), "kansas");
        testIndex(new LucenePositionalIndex("index/1k/lucene/positional"), "kansas");
        testIndex(new PositionalIndex("index/1k/positional"), "kansas");
         
        System.out.println("=======================");
        System.out.println("Checking search...");
        
        testSearch("toy", new LuceneEngine("index/toy/lucene/regular"), "and to sleep", 5);
        testSearch("toy", new ProximityEngine(new LucenePositionalIndex("index/toy/lucene/positional")), "and to sleep", 5);
        testSearch("toy", new LuceneEngine("index/toy/lucene/regular"), "a b c", 5);
        testSearch("toy", new ProximityEngine(new LucenePositionalIndex("index/toy/lucene/positional")), "a b c", 5);
        testSearch("toy", new ProximityEngine(new LucenePositionalIndex("index/toy/lucene/positional")), "\"a b c\"", 5);
        testSearch("toy", new ProximityEngine(new LucenePositionalIndex("index/toy/lucene/positional")), "b c a", 5);
        testSearch("toy", new ProximityEngine(new LucenePositionalIndex("index/toy/lucene/positional")), "\"b c a\"", 5);
        testSearch("toy", new ProximityEngine(new PositionalIndex("index/toy/positional")), "b c a", 5);
         
        testSearch("urls", new LuceneEngine("index/urls/lucene/regular"), "information probability", 5);
        testSearch("urls", new ProximityEngine(new LucenePositionalIndex("index/urls/lucene/positional")), "information probability", 5);

        testSearch("1k", new LuceneEngine("index/1k/lucene/regular"), "obama family tree", 5);
        testSearch("1k", new DocBasedVSMCosineEngine(new LuceneIndex("index/1k/lucene/regular")), "obama family tree", 5);
        testSearch("1k", new ProximityEngine(new LucenePositionalIndex("index/1k/lucene/positional")), "obama family tree", 5);
        testSearch("1k", new ProximityEngine(new LucenePositionalIndex("index/1k/lucene/positional")), "\"obama family tree\"", 5);
        
        testSearch("toy 1", new StaticEngine(new PageRank("graph/toy-graph1.dat", 0.5, 50)), "", 5);
        testSearch("toy 2", new StaticEngine(new PageRank("graph/toy-graph2.dat", 0.6, 50)), "", 5);
        testSearch("1k simulated links", new StaticEngine(new PageRank("graph/1k-links.dat", 0.2, 50)), "", 5);
        Timer.reset();
        testSearch("google", new StaticEngine(new PageRank("graph/web-Google.txt", 0.2, 50)), "", 5);
        Timer.time("  --> ");
    }
    
    static void testIndex(Index index, String word) throws IOException {
        System.out.println("  " + index.getClass().getSimpleName());
 
        System.out.println("\tWord \"" + word + "\" occurs in " + index.getDocFreq(word) + " documents:");
        int n = 0;
        for (Posting posting : index.getPostings(word)) {
            System.out.print("\t\t" + posting.getDocID() + " - " + index.getDocPath(posting.getDocID()) + " (" + posting.getFreq());
            if (posting instanceof PositionalPosting) {
                System.out.print(" ( ");
                for (int pos : ((PositionalPosting) posting))
                    System.out.print(pos + " ");
                System.out.print(")");
            }
            System.out.println(") ");
            if (++n > 5) {
                System.out.println("\t\t...and so on up to " + (index.getDocFreq(word) - 10) + " more documents.");
                break;
            }
        }
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