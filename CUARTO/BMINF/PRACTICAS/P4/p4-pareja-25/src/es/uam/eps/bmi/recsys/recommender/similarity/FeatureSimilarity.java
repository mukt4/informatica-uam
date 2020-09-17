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
package es.uam.eps.bmi.recsys.recommender.similarity;

import es.uam.eps.bmi.recsys.data.Features;

/**
 *
 * @author pablo
 * 
 * Features pueden ser géneros, tags, etc. F es el tipo de identificador de las características, 
 * p.e. enteros, strings.
 */
public abstract class FeatureSimilarity<F> implements Similarity {
    protected Features<F> xFeatures;
    protected Features<F> yFeatures;
    
    public FeatureSimilarity(Features<F> features) {
        xFeatures = yFeatures = features;
    }
    
    public Features<F> getFeatures() {
        return yFeatures;
    }

    public void setXFeatures(Features<F> xf) {
        xFeatures = xf;
    }
}
