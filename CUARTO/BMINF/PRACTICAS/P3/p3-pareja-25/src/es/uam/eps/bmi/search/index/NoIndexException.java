package es.uam.eps.bmi.search.index;

import java.io.IOException;

/**
 *
 * @author pablo
 */
public class NoIndexException extends IOException {
    String folder;
    
    public NoIndexException (String f) {
        folder = f;
    }
    
    public String getFolder() {
        return folder;
    }
}
