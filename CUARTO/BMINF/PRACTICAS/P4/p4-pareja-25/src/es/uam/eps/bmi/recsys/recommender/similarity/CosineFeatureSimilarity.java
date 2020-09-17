package es.uam.eps.bmi.recsys.recommender.similarity;

import es.uam.eps.bmi.recsys.data.Features;

import java.util.HashSet;
import java.util.Set;

import static java.lang.Math.sqrt;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class CosineFeatureSimilarity<F> extends FeatureSimilarity {

    public CosineFeatureSimilarity(Features<F> features){
        super(features);
    }

    @Override
    public double sim(int x, int y) {
        Set<F> xFeaturesSet = this.xFeatures.getFeatures(x);    // Set de features de X
        Set<F> yFeaturesSet = this.yFeatures.getFeatures(y);    // Set de features de Y

        if (xFeaturesSet == null || yFeaturesSet == null)
            return (double) 0;

        Set<F> intFeatureSet = new HashSet<>(xFeaturesSet);     // Set de features de la interseccion X Y

        intFeatureSet.retainAll(yFeaturesSet); // Interseccion

        // Calculamos la raiz del vector X
        Double rootX = sqrt(xFeaturesSet.stream().mapToDouble( fx -> {
            if (this.xFeatures.getFeature(x, fx) != null)
                return this.xFeatures.getFeature(x, fx) * this.xFeatures.getFeature(x, fx);
            else
                return (double) 0;
        }).sum());

        // Calculamos la raiz del vector Y
        Double rootY = sqrt(yFeaturesSet.stream().mapToDouble( fy -> {
            if (this.yFeatures.getFeature(y, fy) != null)
                return this.yFeatures.getFeature(y, fy) * this.yFeatures.getFeature(y, fy);
            else
                return (double) 0;
        }).sum());

        // Calculamos el sumatorio entre los productos de la interseccion (numerador)
        Double numerador = intFeatureSet.stream().mapToDouble( f -> {
            if (this.xFeatures.getFeature(x, f) != null && this.yFeatures.getFeature(y, f) != null)
                return this.xFeatures.getFeature(x, f) * this.yFeatures.getFeature(y, f);
            else
                return (double) 0;
        }).sum();

        // Calculamos el producto de las raices del denominador
        Double denominador = rootX * rootY;

        return numerador / denominador;
    }

    @Override
    public String toString() {
        return "(cosine on item feature)";
    }

}

