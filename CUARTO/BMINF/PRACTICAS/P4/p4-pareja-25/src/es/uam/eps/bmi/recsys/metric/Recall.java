package es.uam.eps.bmi.recsys.metric;

import es.uam.eps.bmi.recsys.Recommendation;
import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.RankingElement;

import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class Recall implements Metric {
    private Ratings test;
    private double threshold;
    private int cutoff;

    public Recall (Ratings test, double threshold, int cutoff) {
        this.test = test;
        this.threshold = threshold;
        this.cutoff = cutoff;
    }

    @Override
    public double compute(Recommendation rec) {
        double recall = 0;

        for (Integer user : rec.getUsers()){     // Sacamos todos los items del recomendador

            int countItems = 0;
            double auxRelevantes = 0;
            double relevantesUsuario = 0;
            Set<Integer> itemsRatedSet = this.test.getItems(user);

            if (itemsRatedSet != null)
                for (Integer item : this.test.getItems(user))
                    if (this.test.getRating(user, item) != null && this.test.getRating(user, item) >= this.threshold)
                        relevantesUsuario++;

            for (RankingElement item : rec.getRecommendation(user)){

                if (countItems == this.cutoff)
                    break;

                Double userScore = this.test.getRating(user, item.getID());

                if (userScore != null && !userScore.isNaN() && userScore >= this.threshold)
                    auxRelevantes++;

                countItems++;
            }

            if (relevantesUsuario != 0)
                recall += auxRelevantes/relevantesUsuario;
        }

        return recall/rec.getUsers().size();  // Dividimos entre los usuarios del recomendador no del test
    }

    @Override
    public String toString() {
        return "Recall@" + this.cutoff;
    }

}

