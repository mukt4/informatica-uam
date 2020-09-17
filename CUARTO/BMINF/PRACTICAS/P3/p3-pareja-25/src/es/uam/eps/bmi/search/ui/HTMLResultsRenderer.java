package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.ranking.SearchRankingDoc;
import java.io.File;
import java.io.IOException;
import javax.swing.JComponent;

/**
 *
 * @author pablo
 */
public class HTMLResultsRenderer extends ResultsRenderer {
    JComponent context;
    
    public HTMLResultsRenderer(JComponent c) {
        context = c;
    }
    
    public String toString() {
        StringBuilder str = new StringBuilder("<html><body><table border = 0 cellpadding = 0 cellspacing = 0>");
        try {
            if (ranking.size() == 0) 
                str.append("<tr><td width='" + context.getBounds().width + "'>"
                        + "<center><br/><br/><b>No results found.</b></center></td></tr>");
            else for (SearchRankingDoc result : ranking) {
                String uri = result.getPath();
                if (new File(uri).exists()) uri = new File(uri).toURI().toString();
                str.append("<tr>"
                        + "<td>" + result.getScore() + "</td>"
                        + "<td><div style='white-space:nowrap'>&nbsp;&nbsp;"
                        + "<a href=" + uri + ">" + uri + "</a>"
                        + "</div></td></tr>");
            }
            str.append("</table>&nbsp;</body></html>");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return str.toString();
    }
}
