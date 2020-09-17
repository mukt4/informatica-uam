package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Ratings;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class AverageRecommender extends AbstractRecommender {

    Map<Integer, Double> ratingAvgMap;

    // Remomendador que calcula los scores de todos los objetos
    public AverageRecommender(Ratings ratings, int umbralRatings) {
        super(ratings);
        ratingAvgMap = new HashMap<>();

        for (int item : ratings.getItems()) {
            Set<Integer> users = ratings.getUsers(item);

            if (users.size() >= umbralRatings){
                Double score = (double) 0;
                for (Integer u : users){
                    Double r = ratings.getRating(u,item);

                    if (r!=null)
                        score += r;
                }
                ratingAvgMap.put(item, score/users.size());
            }
        }
    }

    @Override
    public double score(int user, int item) {
        Double ret = ratingAvgMap.get(item);

        if (ret == null)
            return (double) 0;
        else
            return ret;
    }

    @Override
    public String toString() {
        return "average";
    }

}
