package es.uam.eps.bmi.search.ui;

import es.uam.eps.bmi.search.index.IndexBuilder;
import es.uam.eps.bmi.search.index.NoIndexException;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;

/**
 *
 * @author pablo
 * @author javier
 */
public class IndexDialog extends JDialog {
    IndexBuilder builder;
    JFileChooser indexChooser;
    JFileChooser collectionChooser;
    JLabel msgArea;
    
    public IndexDialog(final SearchWindow mainWindow, String indexFolder) throws IOException {
        super(mainWindow, false);
        builder = mainWindow.createIndexBuilder();
        
        setBounds(mainWindow.getX() + mainWindow.getWidth(), mainWindow.getY(), 700, 200);
        
        final Container content = getContentPane();
        
        // Index folder selection and connection on top
        JPanel connectionPanel = new JPanel();
        ((FlowLayout) connectionPanel.getLayout()).setAlignment(FlowLayout.LEFT);
        
        final JTextField indexFolderPath = new JTextField(indexFolder, 30);
        JButton browseFolderButton = new JButton("Browse");
        JButton connectionButton = new JButton("Connect");
        
        connectionPanel.add(new JLabel("Index folder path: "));
        connectionPanel.add(indexFolderPath);
        connectionPanel.add(browseFolderButton);
        connectionPanel.add(connectionButton);
        content.add("North", connectionPanel);
        
        // Msg area in the center
        msgArea = new JLabel();
        msgArea.setHorizontalAlignment(SwingConstants.CENTER);
        content.add(msgArea);
        
        // Index creation at the bottom
        JPanel buildPanel = new JPanel();
        ((FlowLayout) buildPanel.getLayout()).setAlignment(FlowLayout.LEFT);
        
        final JTextField collectionPath = new JTextField(30);
        JButton browseCollectionButton = new JButton("Browse");
        JButton buildButton = new JButton("Build index");
        
        buildPanel.add(new JLabel("Collection path: "));
        buildPanel.add(collectionPath);
        buildPanel.add(browseCollectionButton);
        buildPanel.add(buildButton);
        content.add("South", buildPanel);
        
         indexChooser = new JFileChooser(indexFolderPath.getText());
         indexChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
         collectionChooser = new JFileChooser("./");
         collectionChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
         
        // Interactions
        browseFolderButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int ret =  indexChooser.showDialog(content, "Select index directory");
                if (ret == JFileChooser.APPROVE_OPTION) indexFolderPath.setText(indexChooser.getSelectedFile().getPath());
            }
        });

        ActionListener connectionListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    mainWindow.createEngine(indexFolderPath.getText());
                    msgArea.setText("");
                } catch (NoIndexException ex) {
                    missingIndexError(ex.getFolder());
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
        };
        connectionButton.addActionListener(connectionListener);
        indexFolderPath.addActionListener(connectionListener);

        browseCollectionButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int ret =  collectionChooser.showDialog(content, "Select collection path or file");
                if (ret == JFileChooser.APPROVE_OPTION) collectionPath.setText(collectionChooser.getSelectedFile().getPath());
            }
        });

        buildButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    builder.build(collectionPath.getText(), indexFolderPath.getText());
                    mainWindow.createEngine(indexFolderPath.getText());
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }
        });
    }
    
    void missingIndexError(String folder) {
        msgArea.setText("<html><center>No index found in " + folder + ", please configure index connection:<br/><br/>"
                + "build some index into " + folder + ", or connect to an index in a different folder.</center></html>");
        setVisible(true);
    }
}
