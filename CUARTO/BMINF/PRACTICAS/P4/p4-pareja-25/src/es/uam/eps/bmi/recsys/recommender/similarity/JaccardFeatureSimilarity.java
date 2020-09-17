package es.uam.eps.bmi.recsys.recommender.similarity;

import es.uam.eps.bmi.recsys.data.Features;

import java.util.HashSet;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class JaccardFeatureSimilarity<F> extends FeatureSimilarity {

    public JaccardFeatureSimilarity(Features<F> features){
        super(features);
    }

    @Override
    public double sim(int x, int y) {
        Set<F> featureXSet = this.xFeatures.getFeatures(x);
        Set<F> featureYSet = this.yFeatures.getFeatures(y);

        if (featureXSet == null || featureYSet == null)
            return (double) 0;

        Set<F> featuresIntSet = new HashSet<>(featureXSet);
        featuresIntSet.retainAll(featureYSet);

        if (featureXSet == null || featureYSet == null)
            return (double) 0;

        return featuresIntSet.size() / (featureXSet.size() + featureYSet.size() - featuresIntSet.size());
    }

    @Override
    public String toString() {
        return "(Jaccard on item features)";
    }

}

