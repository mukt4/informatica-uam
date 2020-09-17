package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.ranking.Ranking;
import es.uam.eps.bmi.recsys.ranking.RankingElement;
import es.uam.eps.bmi.recsys.ranking.RankingImpl;
import es.uam.eps.bmi.recsys.recommender.similarity.Similarity;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class UserKNNRecommender extends AbstractRecommender {
    private Map<Integer, Ranking> userHood = new HashMap<>();
    private Similarity sim;

    public UserKNNRecommender(Ratings ratings, Similarity sim, int k) {
        super(ratings);
        this.sim = sim;

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
        Iterator<RankingElement> iter = hood.iterator();
        Double recommRate = (double) 0;

        while (iter.hasNext()) {
            RankingElement e = iter.next();
            Double similitud = e.getScore();
            Double rate = ratings.getRating(e.getID(), item);

            if (rate != null)
                recommRate += similitud * rate;
        }

        return recommRate;
    }

    @Override
    public String toString() {
        return "user-based kNN " + this.sim;
    }

}

