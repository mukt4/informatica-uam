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
package es.uam.eps.bmi.search.ranking.lucene;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.io.IOException;
import org.apache.lucene.search.ScoreDoc;

/**
 *
 * @author Pablo Castells
 */
public class LuceneRankingDoc extends SearchRankingDoc {
    Index index;
    ScoreDoc rankedDoc;
    
    LuceneRankingDoc (Index idx, ScoreDoc r) {
        index = idx;
        rankedDoc = r;
    }
    public double getScore() {
        return rankedDoc.score;
    }

    public int getDocID() {
        return rankedDoc.doc;
    }

    public String getPath() throws IOException {
        return index.getDocPath(rankedDoc.doc);
    }
}
