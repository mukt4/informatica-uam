package es.uam.eps.bmi.recsys.metric;

import es.uam.eps.bmi.recsys.Recommendation;
import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.RankingElement;

import static java.lang.Math.pow;
import static java.lang.Math.sqrt;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class Rmse implements Metric {

    private Ratings test;

    public Rmse(Ratings test) {
        this.test = test;
    }

    @Override
    public double compute(Recommendation rec) {
        double rmse = (double) 0;
        int denominador = 0;

        for (Integer user : rec.getUsers()){
            for (RankingElement item : rec.getRecommendation(user)){
                Double itemRating = this.test.getRating(user, item.getID());

                if (itemRating != null){
                    Double diferencia = pow(item.getScore() - itemRating, 2);
                    // Nos aseguramos de que el score del item sea un numero
                    if (!diferencia.isNaN())
                        rmse += diferencia;
                    denominador++;
                }
            }
        }

        if (denominador == 0)
            return rmse;
        else
            return sqrt(rmse/denominador);
    }

    @Override
    public String toString() {
        return "Rmse";
    }

}
