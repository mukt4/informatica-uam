package es.uam.eps.bmi.search.proximity;

import es.uam.eps.bmi.search.AbstractEngine;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;
import es.uam.eps.bmi.search.index.structure.positional.PositionsIterator;
import es.uam.eps.bmi.search.ranking.SearchRanking;
import es.uam.eps.bmi.search.ranking.impl.RankingImpl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class ProximityEngine extends AbstractEngine {
    public ProximityEngine(Index idx) {
        super(idx);
    }

    @Override
    public SearchRanking search(String query, int cutoff) throws IOException {

        boolean flagLiteral;
        String[] terminos;
        RankingImpl ranking = new RankingImpl(index, cutoff);

        // Vemos si vamos a trabajar sobre una busqueda literal o no
        if (query.charAt(0) == '"' && query.charAt(query.length()-1) == '"'){
            flagLiteral = true;
            terminos = query.split("\"")[1].split(" ");
        }
        else {
            flagLiteral = false;
            terminos = query.split(" "); // Array de terminos
        }

        Iterator<Posting>[] listPLIter = new Iterator[terminos.length]; // Array para almacenar los iteradores de posting
        PositionalPosting[] arrayPostings = new PositionalPosting[terminos.length]; // Array para almacenar los postings
        int[] listDocIDs = new int[terminos.length]; // Array de docIDs
        int termID = 0; // id de termino

        // Inicializamos la lista de iteradores
        for (String termino : terminos){
            listPLIter[termID] = index.getPostings(termino).iterator();
            termID++;
        }

        // Inicializamos la lista de docIDs y de postings
        for (termID = 0; termID < terminos.length; termID++){
            arrayPostings[termID] = (PositionalPosting) listPLIter[termID].next();
            listDocIDs[termID] = arrayPostings[termID].getDocID();
        }

        boolean flagEndSearch = false;

        while(!flagEndSearch){

            int docIDComun = listDocIDs[0];
            boolean flagIDsDistintos = false;

            for (int docID : listDocIDs){   // Vemos si los docIDs son iguales/distintos
                if (docIDComun != docID){
                    flagIDsDistintos = true;
                    break;
                }
            }

            if (flagIDsDistintos){  // docIDs distintos
                List<Integer> docIDsMenores = getDocIDsMenores(listDocIDs); // Obtenemos los menores
                for (int indice : docIDsMenores){
                    if (listPLIter[indice].hasNext()){
                        arrayPostings[indice] = (PositionalPosting) listPLIter[indice].next();  // Actualizamos solo los menores
                        listDocIDs[indice] = arrayPostings[indice].getDocID();
                    }
                    else
                        flagEndSearch = true;
                }
            }
            else {  //docIDs todos iguales

                if (!flagLiteral){  // Busqueda proximal

                    List<PositionsIterator> posListIterators = new ArrayList<>();
                    List<Integer> listPos = new ArrayList<>();
                    int j = 0;

                    // Inicializamos la lista de posiciones
                    for (PositionalPosting p : arrayPostings){
                        posListIterators.add(j, (PositionsIterator) p.iterator());
                        listPos.add(j, posListIterators.get(j).next());
                        j++;
                    }

                    int b = Collections.max(listPos);
                    boolean flgEnd = false;
                    double score = 0;

                    while (!flgEnd){

                        j = 0;
                        int menorPos = posListIterators.get(j).nextBefore(b);
                        int aj = 0;

                        for (j = 0; j < terminos.length; j++){

                            int finalPos = posListIterators.get(j).nextBefore(b);
                            if (finalPos < menorPos){
                                menorPos = finalPos;
                                aj = j;
                            }
                        }

                        int a = menorPos;

                        score += (double)1 / (double)(b - a - terminos.length + 2);

                        if (posListIterators.get(aj).nextAfter(b) != Integer.MAX_VALUE)
                            b = posListIterators.get(aj).nextAfter(b);
                        else
                            flgEnd = true;
                    }

                    ranking.add(docIDComun, score / index.getDocNorm(docIDComun));

                    for (int indice = 0; indice < terminos.length; indice++){
                        if (listPLIter[indice].hasNext()){
                            arrayPostings[indice] = (PositionalPosting) listPLIter[indice].next();  // Actualizamos las listas de docIDs
                            listDocIDs[indice] = arrayPostings[indice].getDocID();
                        }
                        else
                            flagEndSearch = true;
                    }
                }

                else {  // Busqueda Literal

                    List<PositionsIterator> posListIterators = new ArrayList<>();
                    int j = 0;
                    boolean flgEnd = false;

                    // Inicializamos la lista de posiciones
                    for (PositionalPosting p : arrayPostings){
                        posListIterators.add(j, (PositionsIterator) p.iterator());
                        j++;
                    }

                    int ocurrencias = 0;
                    int posIniQuery = posListIterators.get(0).next();

                    while (!flgEnd){

                        boolean flgOcurrencia = false;

                        for (j = 1; j < posListIterators.size(); j++){

                            int posSig = posListIterators.get(j).nextAfter(posIniQuery);
                            if (posSig != posIniQuery + j || posSig == Integer.MAX_VALUE)
                                break;
                            else if(posSig == posIniQuery + j && j+1 == posListIterators.size())
                                flgOcurrencia = true;
                        }
                        if (flgOcurrencia)
                            ocurrencias++;

                        if (posListIterators.get(0).hasNext())
                            posIniQuery = posListIterators.get(0).next();
                        else
                            flgEnd = true;
                    }
                    if (ocurrencias != 0)
                        ranking.add(docIDComun, (double)ocurrencias);

                    for (int indice = 0; indice < terminos.length; indice++){
                        if (listPLIter[indice].hasNext()){
                            arrayPostings[indice] = (PositionalPosting) listPLIter[indice].next();  // Actualizamos las listas de docIDs
                            listDocIDs[indice] = arrayPostings[indice].getDocID();
                        }
                        else
                            flagEndSearch = true;
                    }
                }
            }
        }
        return ranking;
    }

    /**
     * Función que encuentra los indices de los docIDs más bajos cuando no son
     * todos iguales
     * @param docIDArray Array de docIDs
     * @return Lista de enteros con los indices que contienen los docIDs más bajos
     */
    private List<Integer> getDocIDsMenores(int[] docIDArray){

        int docIDMenor = Integer.MAX_VALUE;
        List<Integer> idxDocIDMenor = new ArrayList<>();
        int docID;

        for (int idxActual = 0; idxActual < docIDArray.length; idxActual++){

            docID = docIDArray[idxActual];
            if (docID < docIDMenor){
                docIDMenor = docID;
                idxDocIDMenor.removeAll(idxDocIDMenor);
                idxDocIDMenor.add(idxActual);
            }
            else if(docID == docIDMenor)
                idxDocIDMenor.add(idxActual);
        }

        return idxDocIDMenor;
    }
}
