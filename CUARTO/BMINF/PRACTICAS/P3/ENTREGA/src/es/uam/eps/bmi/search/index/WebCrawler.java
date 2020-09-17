package es.uam.eps.bmi.search.index;

import es.uam.eps.bmi.search.index.structure.impl.WebCrawlerDoc;
import org.jsoup.HttpStatusException;
import org.jsoup.Jsoup;
import org.jsoup.UnsupportedMimeTypeException;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import java.io.*;
import java.net.MalformedURLException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class WebCrawler {
    private PriorityQueue<WebCrawlerDoc> pqURLs;
    private int numMaxDocs;
    private int numDocs;
    private File fichGrafoSalida;
    private List<String> URLsExplored;
    private String indexPath;

    public WebCrawler(int numMaxDocs, String pathFichURLsSemilla, String indexPath) throws IOException {

        this.pqURLs = new PriorityQueue<>();
        this.numMaxDocs = numMaxDocs;
        this.numDocs = 0;
        this.fichGrafoSalida = new File("graph/", Config.GRAPH_FILE);
        this.URLsExplored = new ArrayList();
        this.indexPath = indexPath;


        if (fichGrafoSalida.exists())
            fichGrafoSalida.delete();

        BufferedReader reader = new BufferedReader(new FileReader(pathFichURLsSemilla));
        String line;

        while ((line = reader.readLine()) != null){
            pqURLs.add(new WebCrawlerDoc(numDocs, line));
            numDocs++;
        }

    }

    public void runCrawler() throws IOException{

        BufferedWriter bw = new BufferedWriter(new FileWriter(fichGrafoSalida));
        BufferedWriter bufferWIndex = new BufferedWriter(new FileWriter(this.indexPath + "urlsWebCrawler.txt"));

        while (numDocs < numMaxDocs){

            WebCrawlerDoc wbDoc = pqURLs.poll();
            while (URLsExplored.contains(wbDoc.getLink()))
                wbDoc = pqURLs.poll();

            // Antes de guardar la URL realizamos algunas comprobaciones
            if(wbDoc.getLink().startsWith("https://") && !wbDoc.getLink().endsWith(".pdf")) {
                bufferWIndex.write(wbDoc.getLink() + "\n");
            }

            try {

                if (wbDoc.getLink().endsWith(".pdf") || wbDoc.getLink().endsWith(".txt") || wbDoc.getLink().endsWith(".html"))
                    URLsExplored.add(wbDoc.getLink());

                else {
                    Document doc = Jsoup.parse(new URL(wbDoc.getLink()), 10000);
                    URLsExplored.add(wbDoc.getLink());
                    String hostActual = (new URL(wbDoc.getLink())).getHost();

                    for (Element e : doc.select("a[href]")){

                        String href = e.attr("abs:href");
                        if (!pqURLs.contains(href)){

                            bw.write(wbDoc.getLink() + "\t" + href + "\n");

                            if ((new URL(href)).getHost().equals(hostActual)){

                                int valAleat1 = (int)Math.floor(Math.random()*(0-numDocs+1)+numDocs);
                                int valAleat2 = (int)Math.floor(Math.random()*(0-numDocs+1)+numDocs);
                                pqURLs.add(new WebCrawlerDoc(valAleat1 + valAleat2 , href));
                            }
                            else {

                                int valAleat1 = (int)Math.floor(Math.random()*(0-numDocs+1)+numDocs);
                                pqURLs.add(new WebCrawlerDoc(valAleat1, href));
                            }
                            numDocs++;
                            // Antes de guardar la URL realizamos algunas comprobaciones
                            if(href.startsWith("https://") && !href.endsWith(".pdf")) {
                                bufferWIndex.write(href + "\n");
                            }
                        }
                    }
                }
            }
            catch (SocketTimeoutException e){
                System.out.println("!> SocketTimeoutException:\t" + wbDoc.getLink());
            }
            catch (MalformedURLException e){
                System.out.println("!> MalformedURLException:\t" + wbDoc.getLink() + e);
            }
            catch (HttpStatusException e){
                System.out.println("!> HttpStatusException:\t" + wbDoc.getLink());
            }
            catch (UnknownHostException e){
                System.out.println("!> UnknownHostException:\t" + wbDoc.getLink());
            }
            catch (UnsupportedMimeTypeException e){
                System.out.println("!> UnsupportedMimeTypeException:\t" + wbDoc.getLink());
            }
            finally {
                URLsExplored.add(wbDoc.getLink());
            }

        }
        bw.close();
        bufferWIndex.close();
    }
}
