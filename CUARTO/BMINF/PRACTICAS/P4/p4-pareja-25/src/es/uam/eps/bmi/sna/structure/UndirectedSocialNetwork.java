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

import java.util.Set;

/**
 *
 * @author pablo
 */
public interface UndirectedSocialNetwork<U> {
    public Set<U> getUsers();
    public Set<U> getContacts(U user);
    public void addContact(U u, U v);
    public boolean connected(U u, U v);
    public int nEdges();
}
