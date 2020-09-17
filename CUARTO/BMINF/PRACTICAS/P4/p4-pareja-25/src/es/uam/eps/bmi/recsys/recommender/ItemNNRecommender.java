package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.recommender.similarity.Similarity;

import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class ItemNNRecommender extends AbstractRecommender {

    private Similarity sim;

    public ItemNNRecommender (Ratings ratings, Similarity sim) {
        super(ratings);
        this.sim = sim;
    }

    @Override
    public double score(int user, int item) {
        Double sum = (double) 0;
        Set<Integer> itemSet = this.ratings.getItems(user);

        for (Integer i : itemSet){

            Double itemRating = this.ratings.getRating(user, i);
            if (itemRating != null){

                Double simIJ = this.sim.sim(item, i);
                //c += simIJ;
                sum += simIJ * itemRating;
            }
        }

        return sum;
    }

    @Override
    public String toString() {
        return "item-based NN " + this.sim;
    }

}
