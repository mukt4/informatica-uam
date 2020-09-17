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
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author pablo
 */
public class MajorityRecommender extends AbstractRecommender {
    Map<Integer,Double> ratingSum;
    
    public MajorityRecommender(Ratings ratings) {
        super(ratings);
        ratingSum = new HashMap<Integer,Double>();
        for (int item : ratings.getItems()) {
            double sum = 0;
            for (int u : ratings.getUsers(item))
                sum += ratings.getRating(u, item);
            ratingSum.put(item, sum);
        }
    }
    
    public double score (int user, int item) {
        return ratingSum.get(item);
    }

    public String toString() {
        return "majority";
    }
}
