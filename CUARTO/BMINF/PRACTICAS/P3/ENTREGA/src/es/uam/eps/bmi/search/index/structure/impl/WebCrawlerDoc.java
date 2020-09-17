package es.uam.eps.bmi.search.index.structure.impl;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class WebCrawlerDoc implements Comparable<WebCrawlerDoc>{
    private int prioridad;
    private String link;

    public WebCrawlerDoc(int prioridad, String link){

        this.prioridad = prioridad;
        this.link = link;
    }

    @Override
    public int compareTo(WebCrawlerDoc o) {
        if (this.prioridad == o.prioridad)
            return this.prioridad;
        else
            return this.prioridad - o.prioridad;
    }

    public int getPrioridad() {
        return this.prioridad;
    }

    public String getLink() {
        return this.link;
    }
}
