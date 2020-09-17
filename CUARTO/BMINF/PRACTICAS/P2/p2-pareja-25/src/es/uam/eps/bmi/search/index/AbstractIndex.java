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
package es.uam.eps.bmi.search.index;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;

/**
 *
 * @author pablo
 */
public abstract class AbstractIndex implements Index {
    protected double docNorms[];
    
    public double getDocNorm(int docID) throws IOException {
        return docNorms[docID];
    }

    public void loadNorms(String indexFolder) throws FileNotFoundException {
        File f = new File(indexFolder + "/" + Config.NORMS_FILE);
        if (!f.exists()) return;
        Scanner scn = new Scanner(f);
        docNorms = new double[numDocs()];
        for (int docID = 0; docID < docNorms.length; docID++)
            docNorms[docID] = new Double(scn.nextLine());
        scn.close();
    }
}
