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
package es.uam.eps.bmi.recsys.ranking;

/**
 *
 * @author pablo
 * 
 * Puede ser un item recomendado con un score, o un usuario (vecino) con un valor de similitud, o un item con un valor de similitud.
 */
public class RankingElementImpl extends RankingElement {
    int id;
    double score;
    
    public RankingElementImpl (int elem, double s) {
        id = elem;
        score = s;
    }
    public double getScore() {
        return score;
    }

    public int getID() {
        return id;
    }
}
