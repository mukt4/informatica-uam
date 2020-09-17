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

import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.io.File;
import java.io.IOException;

/**
 *
 * @author pablo
 */
public class TextResultDocRenderer {
    SearchRankingDoc result;
    
    public TextResultDocRenderer (SearchRankingDoc r) {
        result = r;
    }
    
    public String toString() {
        try {
            String uri = result.getPath();
            if (new File(uri).exists()) uri = new File(uri).toURI().toString();
            return result.getScore() + "\t" + uri;
        } catch ( IOException ex) {
            ex.printStackTrace();
            return "";
        }
    }
}
