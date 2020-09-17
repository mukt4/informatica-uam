package es.uam.eps.bmi.search.index;

/**
 *
 * @author Javier Sanz-Cruzado Puig (javier.sanz-cruzado@uam.es)
 * @author pablo
 */
public interface DocumentFeatureMap extends DocumentMap {
    public double getValue(int docId);
}
