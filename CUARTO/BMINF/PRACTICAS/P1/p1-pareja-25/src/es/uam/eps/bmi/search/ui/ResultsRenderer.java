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

/**
 *
 * @author Pablo Castells
 */
public abstract class ResultsRenderer {
    SearchRanking ranking;

    public ResultsRenderer() {}

    public abstract String toString();

    public ResultsRenderer(SearchRanking r) {
        setResults(r);
    }

    public void setResults (SearchRanking r) {
        ranking = r;
    }
}
