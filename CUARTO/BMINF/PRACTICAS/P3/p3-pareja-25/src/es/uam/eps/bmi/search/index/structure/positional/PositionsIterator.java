package es.uam.eps.bmi.search.index.structure.positional;

import java.util.Iterator;
import java.util.List;

/**
 *
 * @author pablo
 */
public class PositionsIterator implements Iterator<Integer> {
    List<Integer> positions;
    int up, down;

    public PositionsIterator(List<Integer> pos) {
        positions = pos;
    }
    
    public boolean hasNext() {
        return up < positions.size();
    }

    public Integer next() {
        return positions.get(up++);
    }
    
    public Integer nextAfter(int pos) {
        while (hasNext() && positions.get(up) <= pos) up++;
        if (hasNext()) return positions.get(up);
        else return Integer.MAX_VALUE;
    }

    public Integer nextBefore(int pos) {
        while (down < positions.size() - 1 && positions.get(down+1) <= pos) down++;
        /* if (down < positions.size()) */ return positions.get(down);
//        else return -1;
    }
}
