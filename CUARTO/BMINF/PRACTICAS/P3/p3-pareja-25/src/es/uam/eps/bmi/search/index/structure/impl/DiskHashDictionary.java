package es.uam.eps.bmi.search.index.structure.impl;

import es.uam.eps.bmi.search.index.Config;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.structure.Dictionary;
import es.uam.eps.bmi.search.index.structure.EditablePostingsList;
import es.uam.eps.bmi.search.index.structure.PostingsList;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.io.RandomAccessFile;
import java.util.Collection;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;

/**
 *
 * @author pablo
 */
public class DiskHashDictionary implements Dictionary {
    Map<String,Long> termPostings;
    String indexFolder;

    public DiskHashDictionary(String path) {
        indexFolder = path;
        termPostings = new TreeMap<String,Long>();
    }    

    public PostingsList getPostings(String term) throws IOException {
        EditablePostingsList postings = new EditablePostingsList();
        if (!termPostings.containsKey(term)) return postings;
        RandomAccessFile postingsFile = new RandomAccessFile(indexFolder + "/" + Config.POSTINGS_FILE, "r");
        postingsFile.seek(termPostings.get(term));
        int length = postingsFile.readInt();
        while (length-- > 0)
            postings.add(postingsFile.readInt(), postingsFile.readLong());
        postingsFile.close();
        return postings;
    }

    public void add(String term, long address) {
        termPostings.put(term, address);
    }
    
    public Collection<String> getAllTerms() {
        return termPostings.keySet();
    }
    
    public void load() throws IOException {
        File f = new File(indexFolder + "/" + Config.DICTIONARY_FILE);
        if (!f.exists()) throw new NoIndexException(indexFolder);
        Scanner scn = new Scanner(new FileInputStream(f));
        while(scn.hasNext()) {
            String s[] = scn.nextLine().split("\t");
            termPostings.put(s[0], new Long(s[1]));
        }
    }

    public void save() throws FileNotFoundException  {
        PrintStream dictFile = new PrintStream(indexFolder + "/" + Config.DICTIONARY_FILE);
        for (String term : termPostings.keySet())
            dictFile.println(term + "\t" + termPostings.get(term));
        dictFile.close();
    }

    @Override
    public long getDocFreq(String term) throws IOException {
        if (!termPostings.containsKey(term)) return 0;
        RandomAccessFile postingsFile = new RandomAccessFile(indexFolder + "/" + Config.POSTINGS_FILE, "r");
        postingsFile.seek(termPostings.get(term));
        return postingsFile.readInt();
    }
}