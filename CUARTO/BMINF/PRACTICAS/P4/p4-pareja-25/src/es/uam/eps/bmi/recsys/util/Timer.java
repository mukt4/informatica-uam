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
package es.uam.eps.bmi.recsys.util;

/**
 *
 * @author pablo
 */
public class Timer {
    static long time;

    public static void reset() {
        time = System.currentTimeMillis();
    }
    
    public static void reset(String msg) {
        System.out.println(msg);
        reset();
    }
    
    public static void time() {
        long t = System.currentTimeMillis() - time;
        int min = (int) (t / 60000);
        int sec = (int) (t % 60000) / 1000;
        int msec = (int) (t % 1000);
        if (min > 0) System.out.print(min + "min ");
        if (sec > 0) System.out.print(sec + "s ");
        System.out.println(msec + "ms ");
        reset();
    }

    public static void time(String msg) {
        System.out.print(msg);
        time();
    }
}
