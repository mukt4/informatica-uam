package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.structure.Posting;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class ImplPosting extends Posting implements java.io.Serializable {

    int termID;

    public ImplPosting(int id, long f, int termID) {

        super(id, f);
        this.termID = termID;
    }

    public int getTermID(){

        return termID;
    }
}
