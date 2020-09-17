package es.uam.eps.bmi.search.index;

import java.io.IOException;
import java.util.List;

import es.uam.eps.bmi.search.index.freq.FreqVector;

public interface Index {

	List<String> getAllTerms();
	
	public int getNumeroDoc();

	long getTotalFreq(String t1) throws IOException;

	public FreqVector getDocVector(int docID) throws IOException;

	public String getDocPath(int docID) throws IOException;

	public long getTermFreq(String word, int docID) throws IOException;

	public int getDocFreq(String word) throws IOException;

}
