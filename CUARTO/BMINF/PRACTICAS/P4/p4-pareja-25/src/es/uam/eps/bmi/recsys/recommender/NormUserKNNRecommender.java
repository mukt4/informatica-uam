package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.Ranking;
import es.uam.eps.bmi.recsys.ranking.RankingElement;
import es.uam.eps.bmi.recsys.ranking.RankingImpl;
import es.uam.eps.bmi.recsys.recommender.similarity.Similarity;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class NormUserKNNRecommender extends AbstractRecommender {
    private Map<Integer, Ranking> userHood; // Vecindario de un usuario
    private Similarity sim; // Tipo de similitud que utilizaremos
    private int minK;   // Minimo de vecinos para recomendar un objeto

    public NormUserKNNRecommender(Ratings ratings, Similarity sim, int k, int minK) {

        super(ratings);
        userHood = new HashMap<>();
        this.sim = sim;
        this.minK = minK;

        for (Integer u : ratings.getUsers()){
            Ranking r = new RankingImpl(k);
            for (Integer v : ratings.getUsers()){
                if (!v.equals(u))
                    r.add(v, this.sim.sim(u, v));
            }
            userHood.put(u, r);
        }
    }

    @Override
    public double score(int user, int item) {
        Ranking hood = this.userHood.get(user);
        int ratingsK = 0;
        Double score = (double) 0;
        Double c = (double) 0;

        for (RankingElement e : hood) {
            Double similitud = e.getScore();    // sim(user,v)
            Integer neighID = e.getID();              // ID del usuario v del hood
            Double rate = this.ratings.getRating(neighID, item); // r(v,item)

            if (rate != null){
                score += similitud * rate;
                c += similitud;
                ratingsK++;
            }
        }

        if(ratingsK < this.minK)
            return (double) 0;
        else
            return score/c;
    }

    @Override
    public String toString() {
        return "normalized user-based kNN " + this.sim;
    }

}
