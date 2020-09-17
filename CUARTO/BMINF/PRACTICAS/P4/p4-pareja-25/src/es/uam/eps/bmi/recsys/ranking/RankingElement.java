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
public abstract class RankingElement implements Comparable<RankingElement> {
    public abstract double getScore();
    public abstract int getID();
    
    public int compareTo(RankingElement elem) {
       int s = (int) Math.signum(elem.getScore() - getScore());
       // Si hay empate en score, ordenar por ID
       return s == 0? getID() - elem.getID() : s;
    }
}
