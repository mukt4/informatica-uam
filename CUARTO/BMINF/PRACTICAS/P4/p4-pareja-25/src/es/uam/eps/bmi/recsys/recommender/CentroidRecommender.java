package es.uam.eps.bmi.recsys.recommender;

import es.uam.eps.bmi.recsys.data.Features;
import es.uam.eps.bmi.recsys.data.FeaturesImpl;
import es.uam.eps.bmi.recsys.data.Ratings;
import es.uam.eps.bmi.recsys.recommender.similarity.CosineFeatureSimilarity;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class CentroidRecommender<F> extends AbstractRecommender {
    private CosineFeatureSimilarity<F> features;
    private Map<Integer, Features> centroidsMap;

    public CentroidRecommender (Ratings ratings, CosineFeatureSimilarity<F> features){
        super(ratings);
        this.features = features;
        this.centroidsMap = new HashMap<>();

        for (Integer u : this.ratings.getUsers())
            calcularCentroide(u);
    }

    @Override
    public double score(int user, int item) {
        this.features.setXFeatures(this.centroidsMap.get(user));

        return features.sim(user, item);
    }

    @Override
    public String toString() {
        return "centroid-based (cosine on user centroid)";
    }

    public void calcularCentroide(int user){
        Features centroidFeatures = new FeaturesImpl();
        Map<F, Double> newFeaturesMap = new HashMap<>();
        Set<Integer> itemSet = this.ratings.getItems(user);
        int itemsRated = this.ratings.getItems(user).size();

        for (Integer j : itemSet){
            Set<F> itemFeatures = this.features.getFeatures().getFeatures(j);

            if (itemFeatures != null){
                for (F feat : itemFeatures){
                    Double featureScore = this.features.getFeatures().getFeature(j, feat);
                    Double itemRating = this.ratings.getRating(user, j);

                    if (itemRating != null){
                        if (newFeaturesMap.containsKey(feat))
                            newFeaturesMap.replace(feat, itemRating * featureScore + newFeaturesMap.get(feat));
                        else
                            newFeaturesMap.put(feat, itemRating * featureScore);
                    }
                }
            }
        }

        for (F feat : newFeaturesMap.keySet()){
            double recomValue = newFeaturesMap.get(feat) /itemsRated;

            newFeaturesMap.replace(feat, recomValue);
            centroidFeatures.setFeature(user, feat, recomValue);
        }

        this.centroidsMap.put(user, centroidFeatures);
    }
}
