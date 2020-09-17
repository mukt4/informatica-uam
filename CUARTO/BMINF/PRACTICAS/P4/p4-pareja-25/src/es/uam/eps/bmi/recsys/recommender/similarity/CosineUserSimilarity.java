package es.uam.eps.bmi.recsys.recommender.similarity;

import es.uam.eps.bmi.recsys.data.Ratings;

import java.util.HashSet;
import java.util.Set;

import static java.lang.Math.sqrt;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class CosineUserSimilarity implements Similarity {
    private Ratings ratings;

    public CosineUserSimilarity(Ratings ratings){
        this.ratings = ratings;
    }

    @Override
    public double sim(int x, int y) {
        Set<Integer> xItemSet = this.ratings.getItems(x);   // Set de items de X
        Set<Integer> yItemSet = this.ratings.getItems(y);   // Set de items de Y
        Set<Integer> intItemSet = new HashSet<>(xItemSet);  // Set de items de la interseccion X e Y

        intItemSet.retainAll(yItemSet); // Interseccion

        Double rootX = sqrt(xItemSet.stream().mapToDouble(iu -> {
            if (this.ratings.getRating(x, iu) != null)
                return this.ratings.getRating(x, iu) * this.ratings.getRating(x, iu);
            else
                return (double) 0;
        }).sum());
        Double rootY = sqrt(yItemSet.stream().mapToDouble(iv -> {
            if (this.ratings.getRating(y, iv) != null)
                return this.ratings.getRating(y, iv) * this.ratings.getRating(y, iv);
            else
                return (double) 0;
        }).sum());

        Double numerador = intItemSet.stream().mapToDouble(i -> {
            if (this.ratings.getRating(x, i) != null && this.ratings.getRating(y, i) != null)
                return this.ratings.getRating(x, i) * this.ratings.getRating(y, i);
            else
                return (double) 0;
        }).sum();
        Double denominador = rootX * rootY;

        return numerador / denominador;
    }

    @Override
    public String toString() {
        return "(cosine on user similarity)";
    }
}

