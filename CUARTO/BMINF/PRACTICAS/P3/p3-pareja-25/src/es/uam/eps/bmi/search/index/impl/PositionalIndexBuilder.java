package es.uam.eps.bmi.search.index.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.structure.impl.PositionalDictionary;
import es.uam.eps.bmi.search.index.structure.positional.PositionalPosting;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nunez Valle
 */
public class PositionalIndexBuilder extends BaseIndexBuilder{
    @Override
    public void init(String indexPath) throws IOException {
        clear(indexPath);
        nDocs = 0;
        dictionary = new PositionalDictionary();
        docPaths = new ArrayList<>();
    }

    @Override
    public void save(String indexPath) throws IOException {
        ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(indexPath + "/" + Config.DICTIONARY_FILE));
        out.writeObject(dictionary);
        out.close();
    }

    @Override
    public void indexText(String text, String path) {

        String[] terms = text.toLowerCase().split("\\P{Alpha}+");
        Map<String, List<Integer>> positionalDiccMap = new HashMap<>();
        List<Integer> positionalList = null;
        int position = 0;

        for (String term : terms){
            if (positionalDiccMap.containsKey(term))
                positionalList = positionalDiccMap.get(term);
            else
                positionalList = new ArrayList();
            positionalList.add(position);
            positionalDiccMap.put(term, positionalList);
            position++;
        }
        for (Map.Entry<String, List<Integer>> entry : positionalDiccMap.entrySet()){
            PositionalPosting newPositionalPosting = new PositionalPosting(nDocs, (long)entry.getValue().size(), entry.getValue());
            ((PositionalDictionary) dictionary).add(entry.getKey(), newPositionalPosting);
        }
        docPaths.add(path);
        nDocs++;
    }
}
