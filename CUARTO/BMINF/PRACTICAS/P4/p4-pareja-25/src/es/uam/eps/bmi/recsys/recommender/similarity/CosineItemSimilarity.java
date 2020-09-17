package es.uam.eps.bmi.recsys.recommender.similarity;

import es.uam.eps.bmi.recsys.data.Ratings;

import java.util.HashSet;
import java.util.Set;

import static java.lang.Math.sqrt;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class CosineItemSimilarity implements Similarity {
    private Ratings ratings;

    public CosineItemSimilarity(Ratings ratings){
        this.ratings = ratings;
    }

    @Override
    public double sim(int x, int y) {
        Set<Integer> userXSet = this.ratings.getUsers(x);
        Set<Integer> userYSet = this.ratings.getUsers(y);
        Set<Integer> userIntSet = new HashSet<>(userXSet);  // Set de items de la interseccion X e Y
        userIntSet.retainAll(userYSet); // Interseccion

        Double rootX = sqrt(userXSet.stream().mapToDouble(ux -> {
            if (this.ratings.getRating(ux, x) != null)
                return this.ratings.getRating(ux, x) * this.ratings.getRating(ux, x);
            else
                return (double) 0;
        }).sum());

        Double rootY = sqrt(userYSet.stream().mapToDouble(uy -> {
            if (this.ratings.getRating(uy, y) != null)
                return this.ratings.getRating(uy, y) * this.ratings.getRating(uy, y);
            else
                return (double) 0;
        }).sum());

        Double denominador = rootX * rootY;

        Double numerador = userIntSet.stream().mapToDouble(uInt -> {
            if (this.ratings.getRating(uInt, x) != null && this.ratings.getRating(uInt, y) != null)
                return this.ratings.getRating(uInt, x) * this.ratings.getRating(uInt, y);
            else
                return (double) 0;
        }).sum();

        return numerador/denominador;
    }

    @Override
    public String toString() {
        return "(cosine on item similarity)";
    }

}
