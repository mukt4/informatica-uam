package es.uam.eps.bmi.sna;

import edu.uci.ics.jung.algorithms.generators.random.BarabasiAlbertGenerator;
import edu.uci.ics.jung.algorithms.generators.random.ErdosRenyiGenerator;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.UndirectedGraph;
import edu.uci.ics.jung.graph.UndirectedSparseGraph;
import org.apache.commons.collections15.Factory;

import java.io.*;
import java.util.*;

class GraphFactory implements Factory<Graph<Integer,Integer>> {
    public Graph<Integer,Integer> create() {
        return new UndirectedSparseGraph<>();
    }
}

class UndirectedGraphFactory implements Factory<UndirectedGraph<Integer,Integer>> {
    public UndirectedGraph<Integer,Integer> create() {
        return new UndirectedSparseGraph<>();
    }
}

class VertexFactory implements Factory<Integer> {
    private Integer nVertices = 0;
    public Integer create() {
        return nVertices++;
    }
}

class EdgeFactory implements Factory<Integer> {
    private Integer nEdges = 0;
    public Integer create() {
        return nEdges++;
    }
}

public class GraphTest {
    private static void guardarGrafo(Graph<Integer, Integer> grafo, String filename){
        Collection<Integer> vertices = grafo.getVertices();
        HashMap<Integer, Integer> aristas = new HashMap<>();
        int indice = 1;

        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
            for(Integer vertice : vertices){
                if(!aristas.keySet().contains(vertice)){
                    aristas.put(vertice, indice);
                    indice++;
                }
                if (grafo.getNeighbors(vertice)!=null) {
                    for (Integer vecino : grafo.getNeighbors(vertice)) {
                        if (!aristas.keySet().contains(vecino)) {
                            aristas.put(vecino, indice);
                            indice++;
                        }
                        int id_vertice = aristas.get(vertice);
                        int id_vecino = aristas.get(vecino);
                        writer.write(id_vertice + "," + id_vecino + "\n");
                    }
                }
            }
            writer.close();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args){
        HashSet<Integer> seedVertices = new HashSet<>();

        Random rand = new Random();
        BarabasiAlbertGenerator<Integer, Integer> genBAG = new BarabasiAlbertGenerator( new GraphFactory(),
                                                                                        new VertexFactory(),
                                                                                        new EdgeFactory(),
                                                                                        700,70,rand.nextInt(500),
                                                                                        seedVertices);

        genBAG.evolveGraph(200);
        Graph<Integer, Integer> gBA = genBAG.create();

        // Guardamos el grafo en un fichero
        guardarGrafo(gBA, "graph/barabasi.csv");

        ErdosRenyiGenerator<Integer, Integer> genERG = new ErdosRenyiGenerator<>( new UndirectedGraphFactory(),
                                                                                new VertexFactory(),
                                                                                new EdgeFactory(),
                                                                                4000, 0.1);
        Graph<Integer, Integer> gER = genERG.create();

        // Guardamos el grafo en un fichero
        guardarGrafo(gER, "graph/erdos.csv");
    }
}
