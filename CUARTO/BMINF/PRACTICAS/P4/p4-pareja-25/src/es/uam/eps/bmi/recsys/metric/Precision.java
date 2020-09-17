package es.uam.eps.bmi.recsys.metric;

import es.uam.eps.bmi.recsys.Recommendation;
import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.RankingElement;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class Precision implements Metric {
    private Ratings test;
    private double threshold;   // Umbral de relevante
    private int cutoff;         // Limite de cuantos items mirar por usuario ?¿

    public Precision (Ratings test, double threshold, int cutoff) {
        this.test = test;
        this.threshold = threshold;
        this.cutoff = cutoff;
    }

    @Override
    public double compute(Recommendation rec) {
        double precision = 0;

        for (Integer user : rec.getUsers()){     // Sacamos todos los usuarios del recomendador

            int countItems = 0;         // Tope de items por usuario
            double auxRelevantes = 0;   // Relevantes por usuario

            for (RankingElement item : rec.getRecommendation(user)){

                if (countItems == this.cutoff)
                    break;

                Double userScore = this.test.getRating(user, item.getID());

                if (userScore != null && !userScore.isNaN() && userScore >= this.threshold)
                    auxRelevantes++;

                countItems++;
            }
            precision += auxRelevantes/cutoff;  // Siempre es entre K
        }

        return precision/rec.getUsers().size();     // Dividimos entre los usuarios del recomendador no del test
    }

    @Override
    public String toString() {
        return "Precision@" + this.cutoff;
    }

}
