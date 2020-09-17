package es.uam.eps.bmi.search.index.structure;

/**
 *
 * @author pablo
 */
public interface EditableDictionary extends Dictionary {
    public void add(String term, int docID);
}
