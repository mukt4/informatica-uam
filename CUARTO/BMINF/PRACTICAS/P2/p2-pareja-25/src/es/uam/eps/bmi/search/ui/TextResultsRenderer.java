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
package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.SearchRankingDoc;

/**
 *
 * @author pablo
 */
public class TextResultsRenderer extends ResultsRenderer {
    
    public TextResultsRenderer (SearchRanking results) {
        super(results);
    }
    
    public String toString() {
        StringBuilder str = new StringBuilder();
            if (ranking.size() == 0) str.append("No results found.");
            else for (SearchRankingDoc result : ranking) 
                str.append(new TextResultDocRenderer(result));
        return str.toString();
    }
}
