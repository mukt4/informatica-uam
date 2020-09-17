package es.uam.eps.bmi.recsys.data;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Tomas Higuera Viso
 * @author Miguel Antonio Nuñez Valle
 */
public class FeaturesImpl<F> implements Features<F> {
    private Map<Integer, Map<F, Double>> featuresMap;
    private Parser<F> featureParser;

    public FeaturesImpl(){

        this.featuresMap = new HashMap<>();
    }

    public FeaturesImpl(int id){

        this.featuresMap = new HashMap<>();
        featuresMap.put(id, new HashMap<>());
    }

    public FeaturesImpl(String featuresFile, String separator, Parser<F> featureParser){

        this.featureParser = featureParser;
        this.featuresMap = new HashMap<>();

        try {
            FileReader fr = new FileReader(new File(featuresFile));
            BufferedReader br = new BufferedReader(fr);

            String line;
            while((line = br.readLine()) != null){

                String[] items = line.split(separator); //[0]: ID, [1]: Object, [2]: Score

                setFeature( Integer.parseInt(items[0]),
                        this.featureParser.parse(items[1]),
                        Double.parseDouble(items[2]));

            }
        }
        catch(IOException e){
            System.out.print("!> ERROR: No se ha podido abrir el fichero de features.");
            e.printStackTrace();
        }
    }

    @Override
    public Set getFeatures(int id) {

        if (this.featuresMap.get(id) == null)
            return null;
        else
            return this.featuresMap.get(id).keySet();
    }

    @Override
    public Double getFeature(int id, Object feature) {

        if(this.featuresMap.get(id).get(feature) == null)
            return (double) 0;
        else
            return this.featuresMap.get(id).get(feature);
    }

    @Override
    public void setFeature(int id, F feature, double value) {

        if (this.featuresMap.containsKey(id)) {

            if (this.featuresMap.get(id).containsKey(feature))
                this.featuresMap.get(id).replace(feature, value);

            else
                this.featuresMap.get(id).put(feature, value);
        }

        else {

            Map<F, Double> aux = new HashMap<>();
            aux.put((F) feature, value);
            this.featuresMap.put(id, aux);
        }
    }

    @Override
    public Set getIDs() {

        return this.featuresMap.keySet();
    }

}

