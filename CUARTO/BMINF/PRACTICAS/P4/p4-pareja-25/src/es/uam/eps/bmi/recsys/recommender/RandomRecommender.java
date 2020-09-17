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
package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Ratings;
import java.util.Random;

/**
 *
 * @author pablo
 */
public class RandomRecommender extends AbstractRecommender {
    Random rnd;
    
    public RandomRecommender(Ratings ratings) {
        super(ratings);
        rnd = new Random();
    }
    
    public double score (int user, int item) {
        return rnd.nextDouble();
    }

    public String toString() {
        return "random";
    }
}
