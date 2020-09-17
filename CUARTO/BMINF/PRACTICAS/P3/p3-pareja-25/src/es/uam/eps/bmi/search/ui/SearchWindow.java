package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.SearchEngine;
import es.uam.eps.bmi.search.index.Index;
import es.uam.eps.bmi.search.index.IndexBuilder;
import es.uam.eps.bmi.search.index.NoIndexException;
import es.uam.eps.bmi.search.index.lucene.LuceneIndex;
import es.uam.eps.bmi.search.index.lucene.LuceneBuilder;
import es.uam.eps.bmi.search.vsm.DocBasedVSMCosineEngine;
import java.awt.Container;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 *
 * @author pablo
 * @author javier
 */
public class SearchWindow extends JFrame {
    SearchEngine engine;
    int resultsPerPage;
    IndexDialog indexDialog;
    JEditorPane resultsPanel;
    ResultsRenderer renderer;
    
    // Pending: handle pagination beyond resultsPerPage = n.
    public SearchWindow(String indexFolder, int n) throws IOException {
        resultsPerPage = n;

        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setBounds(0, 0, 600, 800);
        
        Container content = getContentPane();
        
        // Search widgets on top
        JPanel searchPanel = new JPanel();
        
        JTextField searchBox = new JTextField(30);
        JButton searchButton = new JButton("Search");

        searchPanel.add(new JLabel("Enter query: "));
        searchPanel.add(searchBox);
        searchPanel.add(searchButton);
        content.add("North", searchPanel);
        
        // Search results in the center
        resultsPanel = new JEditorPane();
        resultsPanel.setContentType("text/html");
        resultsPanel.setEditable(false);
        JScrollPane scroll = new JScrollPane(resultsPanel);
        content.add(scroll);
        renderer = new HTMLResultsRenderer(resultsPanel);
        
        JPanel indexPanel = new JPanel();
        JButton indexButton = new JButton("Manage index");

        // Button at the bottom to open index dialog
        indexPanel.add(indexButton);
        content.add("South", indexPanel);
        
        indexDialog = new IndexDialog(this, indexFolder);
        createEngine(indexFolder); 

        // Interactions
        ActionListener searchListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (engine == null) missingIndexError();
                else {
                    try {
                        renderer.setResults(engine.search(searchBox.getText(), resultsPerPage));
                        resultsPanel.setText(renderer.toString());
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
                } 
            }
        };
        
        searchButton.addActionListener(searchListener);
        searchBox.addActionListener(searchListener);

        resultsPanel.addHyperlinkListener(new HyperlinkListener() {
            public void hyperlinkUpdate(HyperlinkEvent ev) {
                if(ev.getEventType().equals(HyperlinkEvent.EventType.ACTIVATED)) {
                    try {
                        Desktop.getDesktop().browse(ev.getURL().toURI());
//                        resultsPanel.setPage(ev.getURL().toString());
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    } catch (URISyntaxException ex) {
                        ex.printStackTrace();
                    }
                }
            }
        });

        indexButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                indexDialog.setVisible(true);
            }
        });
    }
    
    void missingIndexError() {
        resultsPanel.setText("<html><br/><br/><center><b>No index found, please configure index connection.<br/></center></html>");
        indexDialog.setVisible(true);
    }
    
    void createEngine(String indexFolder) throws IOException {
        try {
//        engine = new LuceneEngine(indexFolder);
        engine = new DocBasedVSMCosineEngine(createIndex(indexFolder));
        } catch (NoIndexException ex) {
            missingIndexError();
        }
    }

    static Index createIndex(String folder) throws IOException {
        return new LuceneIndex(folder);
    }

    static IndexBuilder createIndexBuilder() {
        return new LuceneBuilder();
    }

    public static void main (String a[]) throws IOException {
        new SearchWindow("." + File.separator + "index", 10).setVisible(true);
    }
}
