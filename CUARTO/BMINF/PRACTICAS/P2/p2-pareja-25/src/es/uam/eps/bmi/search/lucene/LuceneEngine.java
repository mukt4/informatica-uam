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
package es.uam.eps.bmi.search.lucene;

import es.uam.eps.bmi.search.AbstractEngine;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.lucene.LuceneIndex;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.lucene.LuceneRanking;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.lucene.analysis.CharArraySet;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexNotFoundException;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.store.FSDirectory;

/**
 *
 * @author pablo
 */
public class LuceneEngine extends AbstractEngine {
    // The Lucene engine does not use the LuceneIndex, but an IndexSearcher instead.
    IndexSearcher searcher;
    QueryParser parser;
    
    public LuceneEngine(String path) throws IOException {
        super(new LuceneIndex(path));
        try {
            searcher = new IndexSearcher(DirectoryReader.open(FSDirectory.open(Paths.get(path))));
            parser = new QueryParser("content", new StandardAnalyzer(CharArraySet.EMPTY_SET));
        } catch (IndexNotFoundException ex) {
            throw new NoIndexException(path);
        }
    }

    public SearchRanking search(String query, int cutoff) throws IOException  {
        try {
            return new LuceneRanking(index, searcher.search(parser.parse(query), cutoff));
        } catch (ParseException ex) {
            ex.printStackTrace();
            return null;
        }
    }
}
