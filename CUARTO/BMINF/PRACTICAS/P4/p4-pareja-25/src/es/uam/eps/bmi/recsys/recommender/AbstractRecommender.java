package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.Recommendation;
import es.uam.eps.bmi.recsys.RecommendationImpl;
import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.Ranking;
import es.uam.eps.bmi.recsys.ranking.RankingImpl;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public abstract class AbstractRecommender implements Recommender{
    protected Ratings ratings;

    public AbstractRecommender(Ratings ratings) {

        this.ratings = ratings;
    }

    /**
     * Clase que da recomendaciones al usuario
     * @param cutoff Corte del ranking
     * @return Recomendador creado
     */
    @Override
    public Recommendation recommend(int cutoff) {

        Recommendation recom = new RecommendationImpl();
        Set<Integer> users = ratings.getUsers();

        for (Integer user : users) {

            Ranking userRanking = new RankingImpl(cutoff);  // Creamos un ranking para solo recomendar los mejores
            Set<Integer> recommendedItems = new HashSet(this.ratings.getItems());

            recommendedItems.removeAll(this.ratings.getItems(user)); // De todos los items, quitamos los rateados por el usuario

            for (Integer ri : recommendedItems){

                double scoreRecommended = score(user, ri);
                if (scoreRecommended != 0)
                    userRanking.add(ri, scoreRecommended);
            }
            recom.add(user, userRanking);
        }

        return recom;
    }
}
