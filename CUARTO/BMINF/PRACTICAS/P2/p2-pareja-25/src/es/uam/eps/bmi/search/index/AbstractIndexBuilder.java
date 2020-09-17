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

import es.uam.eps.bmi.search.index.structure.Posting;
import es.uam.eps.bmi.search.vsm.AbstractVSMEngine;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.jsoup.Jsoup;

/**
 *
 * @author pablo
 */
public abstract class AbstractIndexBuilder implements IndexBuilder {
    protected static final int BUFFER_SIZE = 4096;
    
    protected abstract void indexText(String text, String path) throws IOException;
    protected abstract Index getCoreIndex() throws IOException;

    protected void clear(String indexFolder) throws IOException {
        File dir = new File(indexFolder);
        if (!dir.exists()) Files.createDirectories(Paths.get(indexFolder));
        else for (File f : dir.listFiles()) if (f.isFile()) f.delete();
    }

    protected void indexFolder(File dir) throws IOException {
        for (File f : dir.listFiles()) 
            if (f.isFile()) indexHTML(new FileInputStream(f), f.getAbsolutePath());
    }

    protected void indexZip (File zipFile) throws IOException  {
        ZipInputStream in = new ZipInputStream(new FileInputStream(zipFile));
        ZipEntry entry;
        while ((entry = in.getNextEntry()) != null) {
            StringBuilder str = new StringBuilder();
            byte buffer[] = new byte[BUFFER_SIZE];
            int nread = 0;
            while ((nread = in.read(buffer)) > 0)
                str.append(new String((nread == buffer.length? buffer : Arrays.copyOf(buffer, nread))));
            indexHTML(str.toString(), zipFile.getAbsolutePath() + File.separator + entry.getName());
        }
        in.close();
     }
    
    protected void indexURLs(File f) throws IOException {
        Scanner in = new Scanner(f);
        while (in.hasNext()) indexHTML(in.nextLine());
    }

    protected void indexHTML(InputStream docStream, String path) throws IOException {
        indexText(Jsoup.parse(docStream, StandardCharsets.UTF_8.name(), path).text(), path);
    }
    
    protected void indexHTML(String url) throws IOException {
        indexText(Jsoup.parse(new URL(url), 10000).text(), url); // 10 seconds timeout
    }

     protected void indexHTML(String content, String path) throws IOException {
        try {
            indexText(Jsoup.parse(content, path).text(), path);
        } catch (IllegalArgumentException ex) {
            System.out.println("Failed to index document " + path);
//            ex.printStackTrace();
        }
    }
     
   protected void saveDocNorms(String indexPath) throws IOException {
        Index index = getCoreIndex();
        int numDocs = index.numDocs();
        Map<Integer,Double> norms = new HashMap<Integer,Double>();
        for (String term : index.getAllTerms()) {
            long docFreq = index.getDocFreq(term);
            for (Posting p : index.getPostings(term)) {
                int docID = p.getDocID();
                if (!norms.containsKey(docID)) norms.put(docID, 0.0);
                norms.put(docID, norms.get(docID) + Math.pow(AbstractVSMEngine.tfidf(p.getFreq(), docFreq, numDocs), 2));
            }
        }
        PrintStream out = new PrintStream(indexPath + "/" + Config.NORMS_FILE);
        for (int docID : norms.keySet())
            out.println(Math.sqrt(norms.get(docID)));
        out.close();
    }
}
