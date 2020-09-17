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
package es.uam.eps.bmi.search;

import es.uam.eps.bmi.search.index.Index;

/**
 *
 * @author pablo
 */
public abstract class AbstractEngine implements SearchEngine {
    protected Index index;
    
    public AbstractEngine(Index idx) {
        index = idx;
    }
    
    public Index getIndex() {
        return index;
    }

    public String[] parse(String query) {
        return query.toLowerCase().split("\\P{Alpha}+");
    }
}
