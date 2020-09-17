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
package es.uam.eps.bmi.sna.structure;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

/**
 *
 * @author pablo
 */
public class UndirectedSocialNetworkImpl<U> implements UndirectedSocialNetwork<U> {
    Map<U,Set<U>> graph;
    int nEdges;

    public UndirectedSocialNetworkImpl(String file, String separator, Parser<U> parser) throws FileNotFoundException {
        this();
        Scanner scn = new Scanner(new File(file));
        while (scn.hasNext()) {
            String line[] = scn.nextLine().split(separator);
            addContact(parser.parse(line[0]), parser.parse(line[1]));
        }
    }
    
    public UndirectedSocialNetworkImpl() {
        graph = new HashMap<U,Set<U>>();
    }
    
    public Set<U> getUsers() {
        return graph.keySet();
    }

    public Set<U> getContacts(U user) {
        return graph.get(user);
    }

    public void addContact(U u, U v) {
        if (connected(u, v) || u.equals(v)) return;
        if (!graph.containsKey(u)) graph.put(u, new HashSet<U>());
        graph.get(u).add(v);
        if (!graph.containsKey(v)) graph.put(v, new HashSet<U>());
        graph.get(v).add(u);
        nEdges++;
    }

    public boolean connected(U u, U v) {
        return getContacts(u) != null && getContacts(u).contains(v);
    }

    public int nEdges() {
        return nEdges;
    }
}
