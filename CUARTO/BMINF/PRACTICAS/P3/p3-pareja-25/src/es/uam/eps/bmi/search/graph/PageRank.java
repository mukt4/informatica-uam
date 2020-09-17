package es.uam.eps.bmi.search.graph;

import es.uam.eps.bmi.search.index.DocumentFeatureMap;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PageRank implements DocumentFeatureMap{
    private int numDocs;
    private Map<Integer, String> docMap; // Mapa docID - ruta
    private Map<Integer, Double> scoreMap; // Mapa docID - score

    public PageRank(String graphPath, double r, int numIters) throws IOException {

        numDocs = 0;
        docMap = new HashMap<>();
        scoreMap = new HashMap<>();
        Map<String, Integer> auxDocMap = new HashMap<>(); // Igual que docMap pero al reves
        Map<String, Integer> adjacencyMap = new HashMap<>(); // docPath - numDocs que apunta
        Map<String, List<String>> incidenceMap = new HashMap<>(); // docPath - lista de documentos que le apuntan
        Map<Integer, Double> auxProbabilityMap = new HashMap<>();

        BufferedReader reader = new BufferedReader(new FileReader(graphPath));
        String line;

        // Tratamiento del fichero de grafo
        while ((line = reader.readLine()) != null){

            String[] connection = line.split("\t");

            if (!docMap.containsValue(connection[0])){  // Actualizamos docMap
                docMap.put(numDocs++, connection[0]);
            }

            if (!docMap.containsValue(connection[1])){  // Actualizamos docMap
                docMap.put(numDocs++, connection[1]);
            }

            if (!adjacencyMap.containsKey(connection[0])){ // Si no esta el link en el mapa de documentos, tampoco en el de adyacencias
                adjacencyMap.put(connection[0], 1);
            }
            else {  // Actualizamos adyacencias
                int actualAdjacencies = adjacencyMap.get(connection[0]);
                adjacencyMap.put(connection[0], ++actualAdjacencies);
            }

            if (!incidenceMap.containsKey(connection[1])){   // Creamos o actualizamos entrada en el mapa de incidencias
                List<String> newIncidList = new ArrayList<>();
                newIncidList.add(connection[0]);
                incidenceMap.put(connection[1], newIncidList);
            }
            else {
                List<String> actualIncidList = incidenceMap.get(connection[1]);
                actualIncidList.add(connection[0]);
                incidenceMap.put(connection[1], actualIncidList);
            }
        }

        // Algoritmo de clase
        double prob = (double)1 / (double)numDocs;
        boolean flgSinks = false;
        for (int docID = 0; docID < numDocs; docID++){
            scoreMap.put(docID, prob);
            auxDocMap.put(docMap.get(docID), docID);
            if (!adjacencyMap.containsKey(docMap.get(docID))) // Si hay un nodo que no tiene adyacencias, hay sumideros
                flgSinks = true;
        }

        for (int iters = 1; iters < numIters; iters++) {

            double auxProb = r / numDocs;
            double sumatorioParcialProbs = 0;
            for (int docID = 0; docID < numDocs; docID++)
                auxProbabilityMap.put(docID, auxProb);

            for (Map.Entry<String, Integer> entryDocMap : adjacencyMap.entrySet()){

                List<String> listIncidenceLinks = incidenceMap.get(entryDocMap.getKey());
                double sumatorio = 0;

                if (listIncidenceLinks != null){
                    for (String indicDoc : listIncidenceLinks)
                        sumatorio += scoreMap.get(auxDocMap.get(indicDoc)) / adjacencyMap.get(indicDoc);
                }

                auxProb = r/numDocs + (1-r) * sumatorio;
                auxProbabilityMap.put(auxDocMap.get(entryDocMap.getKey()), auxProb);
                sumatorioParcialProbs += auxProb;
            }

            for (int docID = 0; docID < numDocs; docID++){
                if (!flgSinks)
                    scoreMap.put(docID, auxProbabilityMap.get(docID));
                else
                    scoreMap.put(docID, auxProbabilityMap.get(docID) + ((1 - sumatorioParcialProbs) / numDocs));
            }
        }

    }

    @Override
    public double getValue(int docId) {
        return scoreMap.get(docId);
    }

    @Override
    public String getDocPath(int docID) {
        return docMap.get(docID);
    }

    @Override
    public double getDocNorm(int docID) {
        throw new UnsupportedOperationException("Operacion no permitida desde pageRank.");
    }

    @Override
    public int numDocs() {
        return numDocs;
    }
}
