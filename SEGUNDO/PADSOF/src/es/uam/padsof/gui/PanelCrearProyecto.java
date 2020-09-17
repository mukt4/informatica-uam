package es.uam.padsof.gui;

import es.uam.padsof.controlador.ControladorCrearProyecto;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.proyecto.Ambito;
import es.uam.padsof.modelo.proyecto.Distrito;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.io.File;
import java.util.ArrayList;

/**
 * Clase PanelCrearProyecto que contiene el form para crear un proyecto de tipo social o infraestructura
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelCrearProyecto extends JPanel {
    private JTextField nombre;
    private JTextArea descripcion;
    private JTextField importe;
    private JPanel panel_secundario;
    private JTextArea grupoSocial;
    private JComboBox ambitos;
    private ArrayList<JRadioButton> botones_distritos;
    private ButtonGroup botones_tipo_proyecto;
    private JFileChooser esquemaGrafico;
    private File fichero;
    private JLabel nombre_fichero_label;
    private CardLayout layout_secundario;
    private JButton boton_crear_social;
    private JButton boton_crear_infraestructura;

    /**
     * Constructor de la clase PanelCrearProyecto
     * @param panelMainUsuario PanelMainUsuario que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     * @param colectivo Colectivo si se ejecuta el panel desde un colectivo
     */
    public PanelCrearProyecto(PanelMainUsuario panelMainUsuario, Aplicacion aplicacion, Colectivo colectivo){
        this(panelMainUsuario, aplicacion);
        ((ControladorCrearProyecto)boton_crear_social.getActionListeners()[0]).setColectivo(colectivo);
        ((ControladorCrearProyecto)boton_crear_infraestructura.getActionListeners()[0]).setColectivo(colectivo);
    }

    /**
     * Constructor de la clase PanelCrearProyecto
     * @param panelMainUsuario PanelMainUsuario que hace de contenedor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelCrearProyecto(PanelMainUsuario panelMainUsuario, Aplicacion aplicacion){
        setLayout(new MigLayout("insets 0 0 0 0"));
        fichero = null;

        // Creamos un nuevo panel para poder crear un scroll
        JPanel contenido = new JPanel();
        contenido.setLayout(new MigLayout("insets 20 20 0 20, wrap 1, fill"));
        JScrollPane scrollable = new JScrollPane(contenido);
        add(scrollable, "pushx, growx");
        // Ponemos los bordes del scroll vacios
        scrollable.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 0, Color.black));

        // Creamos los componentes necesarios para la vista
        JLabel nombre_etiqueta = new JLabel("Nombre: ");
        nombre_etiqueta.setFont(Styles.getFont_botones());
        nombre = new JTextField(15);
        nombre.setFont(Styles.getFont_label());
        JLabel descripcion_etiqueta = new JLabel("Descripcion: ");
        descripcion_etiqueta.setFont(Styles.getFont_botones());
        // Creamos un scroll para la descripcion
        descripcion = new JTextArea();
        // Creamos un scroll para la descripcion
        JScrollPane scroll_descripcion = new JScrollPane(descripcion);
        scroll_descripcion.setBorder(nombre.getBorder());
        descripcion.setFont(Styles.getFont_label());
        JLabel importe_etiqueta = new JLabel("Importe: ");
        importe_etiqueta.setFont(Styles.getFont_botones());
        importe = new JTextField(15);
        importe.setFont(Styles.getFont_label());

        // Creamos el radio button para elegir dependiendo del tipo de proyecto
        botones_tipo_proyecto = new ButtonGroup();
        JRadioButton infraestructura_boton = new JRadioButton("Infraestructura");
        infraestructura_boton.setFont(Styles.getFont_label());
        JRadioButton social_boton = new JRadioButton("Social");
        social_boton.setFont(Styles.getFont_label());
        botones_tipo_proyecto.add(infraestructura_boton);
        botones_tipo_proyecto.add(social_boton);

        // Creamos el panel que depende de la eleccion del tipo de proyecto
        panel_secundario = new JPanel();
        layout_secundario = new CardLayout();
        panel_secundario.setLayout(layout_secundario);
        // Creamos un panel vacio que sera el que se mostrara si no hay nada seleccionado
        JPanel panel_vacio = new JPanel();
        panel_secundario.add(panel_vacio, "vacio");

        // Creamos los dos posibles paneles de creacion dependiendo del tipo de proyecto
        /* **************************** Panel infraestructura ***************************/
        JPanel panel_infraestructura = new JPanel();
        panel_infraestructura.setLayout(new MigLayout("insets 0 0 0 0, wrap 3"));
        // Creamos un file chooser para el esquema grafico
        JLabel fichero_label = new JLabel("Esquema grafico: ");
        fichero_label.setFont(Styles.getFont_botones());
        JButton boton_esquema = new JButton("Choose file");
        boton_esquema.setFont(Styles.getFont_label());
        // Creamos la etiqueta que contendra el nombre del fichero
        nombre_fichero_label = new JLabel();
        nombre_fichero_label.setFont(Styles.getFont_label());
        // Anadimos el controlador del boton de elegir fichero
        boton_esquema.addActionListener(actionEvent -> {
            FileNameExtensionFilter filter = new FileNameExtensionFilter("Filtro", "jpeg", "jpg");
            esquemaGrafico = new JFileChooser("Choose file");
            esquemaGrafico.setFileFilter(filter);
            if (esquemaGrafico.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
                fichero = esquemaGrafico.getSelectedFile();
                // Una vez elegido el fichero, colocamos su nombre en el panel
                nombre_fichero_label.setText(fichero.getName());
            }
        });
        JLabel distritos_label = new JLabel("Distritos: ");
        distritos_label.setFont(Styles.getFont_botones());
        // Creamos un panel donde almacenaremos los radio button
        JPanel panel_botones = new JPanel();
        panel_botones.setLayout(new MigLayout("wrap 3"));
        botones_distritos = new ArrayList<>();
        for(Distrito distrito : Distrito.values()){
            JRadioButton boton_distrito = new JRadioButton(distrito.toString());
            boton_distrito.setFont(Styles.getFont_label());
            botones_distritos.add(boton_distrito);
            panel_botones.add(boton_distrito);
        }

        // Por ultimo creamos el boton de crear proyecto infraestructura
        boton_crear_infraestructura = new JButton("Crear proyecto");
        boton_crear_infraestructura.setFont(Styles.getFont_botones());
        boton_crear_infraestructura.addActionListener(new ControladorCrearProyecto(this, aplicacion, panelMainUsuario));
        boton_crear_infraestructura.setActionCommand("Infraestructura");

        // Anadimos los componentes al panel
        panel_infraestructura.add(fichero_label);
        panel_infraestructura.add(boton_esquema, "gapleft 20");
        panel_infraestructura.add(nombre_fichero_label, "gapleft 20");
        panel_infraestructura.add(distritos_label, "wrap");
        panel_infraestructura.add(panel_botones, "span 3");
        panel_infraestructura.add(boton_crear_infraestructura, "span 3, left, gapbottom 20");

        /* **************************** Panel social  ***********************************/
        JPanel panel_social = new JPanel();
        panel_social.setLayout(new MigLayout("insets 0 0 0 0, wrap 2"));
        JLabel grupoSocial_etiqueta = new JLabel("Grupo social:");
        grupoSocial_etiqueta.setFont(Styles.getFont_botones());
        grupoSocial = new JTextArea();
        // Creamos un scroll pane para el grupo social
        JScrollPane scroll_grupoSocial =  new JScrollPane(grupoSocial);
        grupoSocial.setFont(Styles.getFont_label());
        scroll_grupoSocial.setBorder(nombre.getBorder());
        JLabel ambito_etiqueta = new JLabel("Ambito");
        ambito_etiqueta.setFont(Styles.getFont_botones());
        String[] ambito_comboBox = new String[Ambito.values().length];
        int i = 0;
        for(Ambito ambito : Ambito.values()){
            ambito_comboBox[i] = (ambito.toString());
            i++;
        }
        ambitos = new JComboBox(ambito_comboBox);
        ambitos.setSelectedIndex(0);
        ambitos.setFont(Styles.getFont_label());
        // Por ultimo creamos el boton de crear proyecto social
        boton_crear_social = new JButton("Crear proyecto");
        boton_crear_social.setFont(Styles.getFont_botones());
        boton_crear_social.setActionCommand("Social");
        boton_crear_social.addActionListener(new ControladorCrearProyecto(this, aplicacion, panelMainUsuario));

        panel_social.add(ambito_etiqueta);
        panel_social.add(ambitos, "gapleft 20");
        panel_social.add(grupoSocial_etiqueta);
        panel_social.add(scroll_grupoSocial, "gapleft 20, height 20%, pushx, growx, gapbottom 20");
        panel_social.add(boton_crear_social, "span 2, left, gapbottom 20");

        // Anadimos ambos paneles al panel secundario
        panel_secundario.add(panel_infraestructura, "infraestructura");
        panel_secundario.add(panel_social, "social");
        layout_secundario.show(panel_secundario, "vacio");

        // Anadimos los controladores de los radio button
        infraestructura_boton.addActionListener(actionEvent -> {
            layout_secundario.show(panel_secundario, "infraestructura");
        });

        social_boton.addActionListener(actionEvent -> {
            layout_secundario.show(panel_secundario, "social");
        });

        // Anadimos los componentes al panel
        JPanel panel_textos = new JPanel();
        panel_textos.setLayout(new MigLayout("wrap 2, fill"));
        panel_textos.add(nombre_etiqueta, "gaptop 5");
        panel_textos.add(nombre);
        panel_textos.add(importe_etiqueta);
        panel_textos.add(importe);
        panel_textos.add(descripcion_etiqueta, "top");
        panel_textos.add(scroll_descripcion, "push, grow");
        contenido.add(panel_textos, "pushx, growx, height 40%");
        // Es necesario crear un panel para los botones para que no desplacen el resto de elementos
        JPanel panel_eleccion = new JPanel();
        panel_eleccion.setLayout(new MigLayout("insets 0 0 0 0"));
        panel_eleccion.add(social_boton);
        panel_eleccion.add(infraestructura_boton);
        contenido.add(panel_eleccion, "span 2, pushx, growx");
        contenido.add(panel_secundario, "span 2, pushx, growx");
    }

    // Getters

    /**
     * Getter del nombre del proyecto del formulario
     * @return String que contiene el nombre del proyecto
     */
    public String getNombre(){
        return nombre.getText();
    }

    /**
     * Getter de la descripcion del proyecto del formulario
     * @return String que contiene la desctipcion del proyecto
     */
    public String getDescripcion(){
        return descripcion.getText();
    }

    /**
     * Getter del importe del proyecto del formulario
     * @return String que contiene el importe del proyecto
     */
    public String getImporte(){
        return importe.getText();
    }


    /**
     * Getter del ArrayList de distritos del formulario
     * @return ArrayList que contiene todos los distritos seleccionados en el form
     */
    public ArrayList<Distrito> getDistritos(){
        ArrayList<Distrito> distritos = new ArrayList<>();

        for(JRadioButton boton : botones_distritos){
            if(boton.isSelected()){
                distritos.add(Distrito.valueOf(boton.getActionCommand()));
            }
        }
        return distritos;
    }

    /**
     * Getter del fichero del esquemaGrafico del form
     * @return File que cotniene el esquema grafico
     */
    public File getEsquemaGrafico(){
        return fichero;
    }

    /**
     * Getter del BottonGroup que contiene que tipo de proyecto se ha seleccionado
     * @return ButtonGroup con los botones social e infraestructura
     */
    public ButtonGroup getBotones_tipo_proyecto() {
        return botones_tipo_proyecto;
    }

    /**
     * Getter del grupo social del formulario
     * @return String que contiene el grupo social
     */
    public String getGrupoSocial() {
        return grupoSocial.getText();
    }

    /**
     * Getter del Ambito del formulario
     * @return Ambito del proyecto
     */
    public Ambito getAmbito(){
        if(ambitos.getSelectedIndex() == 0){
            return Ambito.NACIONAL;
        }
        else{
            return Ambito.INTERNACIONAL;
        }
    }

    /**
     * Metodo que limpia las posibles entradas del usuario en el formulario
     */
    public void clean(){
        clean_nombre();
        clean_descripcion();
        clean_importe();
        clean_grupoSocial();
        clean_fichero();
        clean_distritos();
        clean_tipoProyecto();
    }

    /**
     * Metodo que limpia el JTextField del nombre
     */
    private void clean_nombre(){
        nombre.setText("");
    }

    /**
     * Metodo que limpia el JTextField de la descripcion
     */
    private void clean_descripcion(){
        descripcion.setText("");
    }

    /**
     * Metodo que limpia el JTextField del importe
     */
    public void clean_importe(){
        importe.setText("");
    }

    /**
     * Metodo que limpia el JTextField del grupo social
     */
    private void clean_grupoSocial(){
        grupoSocial.setText("");
    }

    /**
     * Metodo que limpia el JFileChosser del esquemaGrafico
     */
    private void clean_fichero(){
        fichero = null;
        nombre_fichero_label.setText("");
    }

    /**
     * Metodo que limpia el ButtonGroup con el tipo de proyecto
     */
    private void clean_tipoProyecto(){
        botones_tipo_proyecto.clearSelection();
        layout_secundario.show(panel_secundario, "vacio");
    }

    /**
     * Meotodo que limpia todos los JRadioButton de los distritos
     */
    private void clean_distritos(){
        for(JRadioButton boton : botones_distritos){
            boton.setSelected(false);
        }
    }
}
