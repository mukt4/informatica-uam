package es.uam.eps.bmi.search.index.structure.positional;

import es.uam.eps.bmi.search.index.structure.Posting;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author pablo
 */
public class PositionalPosting extends Posting implements Iterable<Integer> {
    protected List<Integer> positions;
    
    public PositionalPosting(int id, long f, List<Integer> pos) {
        super(id, f);
        positions = pos;
    }
    
    public Iterator<Integer> iterator() {
        return new PositionsIterator(positions);
    }

    public List<Integer> getPositions()
    {
        return this.positions;
    }
}
