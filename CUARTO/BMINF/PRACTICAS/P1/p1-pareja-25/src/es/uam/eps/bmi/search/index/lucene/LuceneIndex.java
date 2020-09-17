package es.uam.eps.bmi.search.index.lucene;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.MultiFields;
import org.apache.lucene.index.Term;
import org.apache.lucene.index.TermsEnum;
import org.apache.lucene.store.FSDirectory;

import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.freq.FreqVector;
import es.uam.eps.bmi.search.index.freq.lucene.LuceneFreqVector;

public class LuceneIndex implements Index {
	
	private IndexReader indexReader_LuceneI;
	private List<String> listaTerms_LuceneI;
	
	
	public LuceneIndex(String path) throws IOException {
		indexReader_LuceneI = DirectoryReader.open(FSDirectory.open(Paths.get(path)));
		listaTerms_LuceneI = new ArrayList<>();
		
		TermsEnum terms = MultiFields.getFields(indexReader_LuceneI).terms("contenido").iterator(); //Multifields tiene el metodo estatico getfields
		while(null != terms.next()) {
			listaTerms_LuceneI.add(terms.term().utf8ToString());
		}
	}

	@Override
	public List<String> getAllTerms() {
		
		return listaTerms_LuceneI;
	}

	@Override
	public long getTotalFreq(String t1) throws IOException {
		
		return indexReader_LuceneI.totalTermFreq(new Term("contenido",t1));
	}

	@Override
	public FreqVector getDocVector(int docID) throws IOException {
		
		return new LuceneFreqVector(indexReader_LuceneI.getTermVector(docID,"contenido"));
	}

	@Override
	public String getDocPath(int docID) throws IOException {
		
		return indexReader_LuceneI.document(docID).get("ruta");
	}

	@Override
	public long getTermFreq(String word, int docID) throws IOException {
		
		return getDocVector(docID).getFreq(word);
	}

	@Override
	public int getDocFreq(String word) throws IOException {
		
		return indexReader_LuceneI.docFreq(new Term("contenido",word));
	}
	
	public IndexReader getIReader() {
		return indexReader_LuceneI;
	}

	@Override
	public int getNumeroDoc() {
		return indexReader_LuceneI.numDocs();
	}

}
