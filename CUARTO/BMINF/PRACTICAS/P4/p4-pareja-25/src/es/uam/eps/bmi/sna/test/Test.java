/*
 *  Copyright (C) 2020 Pablo Castells y Javier Sanz-Cruzado
 *
 *  Este código se ha implementado para la realización de las prácticas de
 *  la asignatura "Búsqueda y minería de información" de 4º del Grado en
 *  Ingeniería Informática, impartido en la Escuela Politécnica Superior de
 *  la Universidad Autónoma de Madrid. El fin del mismo, así como su uso,
 *  se ciñe a las actividades docentes de dicha asignatura.
 *
 */
package es.uam.eps.bmi.sna.test;

import es.uam.eps.bmi.sna.metric.edge.Embeddedness;
import es.uam.eps.bmi.sna.metric.LocalMetric;
import es.uam.eps.bmi.sna.metric.GlobalMetric;
import es.uam.eps.bmi.sna.metric.network.Assortativity;
import es.uam.eps.bmi.sna.metric.network.AvgUserMetric;
import es.uam.eps.bmi.sna.metric.network.ClusteringCoefficient;
import es.uam.eps.bmi.sna.metric.user.UserClusteringCoefficient;
import es.uam.eps.bmi.sna.ranking.Ranking;
import es.uam.eps.bmi.sna.ranking.RankingElement;
import es.uam.eps.bmi.sna.structure.Edge;
import es.uam.eps.bmi.sna.structure.IntParser;
import es.uam.eps.bmi.sna.structure.Parser;
import es.uam.eps.bmi.sna.structure.StringParser;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetwork;
import es.uam.eps.bmi.sna.structure.UndirectedSocialNetworkImpl;
import es.uam.eps.bmi.sna.util.Timer;
import java.io.File;
import java.io.FileNotFoundException;


/**
 *
 * @author pablo
 */
public class Test {
    public static void main (String a[]) throws FileNotFoundException {
        testNetwork("graph/small1.csv", ",", new IntParser(), 5, 6, 4);
        testNetwork("graph/small2.csv", ",", new IntParser(), 5, 3, 5);
        testNetwork("graph/small3.csv", ",", new StringParser(), 5, "a", "b");
        testNetwork("graph/facebook_combined.txt", " ", new IntParser(), 5, 9, 3);
        testNetwork("graph/twitter.csv", ",", new StringParser(), 5, "el_pais", "ElviraLindo");
        testNetwork("graph/barabasi.csv", ",", new IntParser(), 5, 1, 2);
        testNetwork("graph/erdos.csv", ",", new IntParser(), 5, 1, 2);
    }
    
    static <U extends Comparable<U>>void testNetwork(String graphFile, String separator, Parser<U> parser, int topK, U u, U v) throws FileNotFoundException {
        System.out.println("==================================================");
        System.out.print("Testing " + new File(graphFile).getName() + " network - ");
        UndirectedSocialNetwork<U> network = new UndirectedSocialNetworkImpl<U>(graphFile, separator, parser);
        System.out.println(network.getUsers().size() + " users and " + network.nEdges() + " contact relationships");
        System.out.println("User " + u + " has " + network.getContacts(u).size() + " contacts");

        // Métricas de usuarios
        System.out.println("-------------------------");
        testMetric(new UserClusteringCoefficient<U>(topK), network, u);

        // Métricas de arcos
        System.out.println("-------------------------");
        testMetric(new Embeddedness<U>(topK), network, new Edge<U>(u, v));
        
        // Métricas globales de red
        System.out.println("-------------------------");
        testMetric(new ClusteringCoefficient<U>(), network);
        testMetric(new AvgUserMetric<U>(new UserClusteringCoefficient<U>()), network);
        testMetric(new Assortativity<U>(), network);
    }
    
    static <T extends Comparable<T>,U>void testMetric(LocalMetric<T,U> metric, UndirectedSocialNetwork<U> network, T example) {
        Timer.reset();
        System.out.println(metric);
        Ranking<T> ranking = metric.compute(network);
        for (RankingElement<T> element : ranking) 
            System.out.println("  " + element.geElement() + "\t" + element.getScore());
        System.out.println("  ...\n  " + example + "\t" + metric.compute(network, example));
        Timer.time("--> ");
        System.out.println();
    }

    static <U>void testMetric(GlobalMetric<U> metric, UndirectedSocialNetwork<U> network) {
        Timer.reset();
        System.out.print(metric + " = " + metric.compute(network) + " (");
        Timer.time();
        System.out.println(")");
    }
}
