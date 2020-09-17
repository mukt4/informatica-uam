package es.uam.padsof.gui;

import es.uam.eps.sadp.grants.GrantRequest;
import es.uam.padsof.controlador.ControladorElemento;
import es.uam.padsof.gui.estilos.Styles;
import es.uam.padsof.modelo.Aplicacion;
import es.uam.padsof.modelo.colectivo.Colectivo;
import es.uam.padsof.modelo.excepcion.AplicacionExcepcion;
import es.uam.padsof.modelo.proyecto.Distrito;
import es.uam.padsof.modelo.proyecto.Infraestructura;
import es.uam.padsof.modelo.proyecto.Proyecto;
import es.uam.padsof.modelo.proyecto.ProyectoSocial;
import es.uam.padsof.modelo.usuario.registrado.UsuarioRegistrado;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Clase PanelElemento que muestra los diferentes elementos de la aplicacion, colectivo o proyecto
 * @author Tomas Higuera Viso
 * @version 1.0
 */
public class PanelElemento extends JPanel {
    private PanelMain panelMain;
    private Aplicacion aplicacion;
    private JLabel nombre_label;
    private JLabel descripcion_label;
    private JLabel importe_label;
    private JLabel tipo_proyecto_label;
    private JLabel numero_votos_label;
    private JLabel seguidores_label;
    private JLabel representante_label;
    private JLabel colectivos_seguidores_label;
    private JLabel colectivos_hijo_label;
    private JLabel grupo_social_label;
    private JLabel ambito_label;
    private JLabel fecha_label;
    private JLabel distrito_label;
    private JLabel esquemaGrafico_label;
    private JLabel estadoProyecto_label;
    private JLabel rechazo_label;
    private JLabel importe_concedido_label;
    private JLabel creador_label;
    private JButton seguir_proyecto;
    private JButton abandonar_proyecto;
    private JButton seguir_colectivo;
    private JButton abandonar_colectivo;
    private JButton votar_proyecto;
    private JButton crear_proyecto;
    private JButton crear_colectivo_hijo;
    private JButton solicitar_financiacion;
    private JButton comprobar_financiacion;
    private JPanel panel_acciones;
    private JPanel panel_informacion;
    private Proyecto proyecto;
    private Colectivo colectivo;
    private JComboBox comboBox_proyectos;
    private JComboBox comboBox_colectivos;

    /**
     * Constructor de la clase PanelElemento
     * @param panelMain PanelMain que hace de contendor del resto de paneles
     * @param aplicacion Aplicacion que contiene toda la informacion
     */
    PanelElemento(PanelMain panelMain, Aplicacion aplicacion){
        this.panelMain = panelMain;
        this.aplicacion = aplicacion;
        this.colectivo = null;
        this.proyecto = null;

        // Creamos los paneles que contendran toda la informacion y los anadimos
        panel_acciones = new JPanel();
        panel_acciones.setLayout(new MigLayout("wrap 1, fillx", "right"));

        panel_informacion = new JPanel();
        panel_informacion.setLayout(new MigLayout("wrap 2", "left"));

        add(panel_informacion);
        add(panel_acciones, "top, right, pushx, growx");

        // Creamos los componenetes estaticos del panel
        nombre_label = new JLabel("Nombre: ");
        nombre_label.setFont(Styles.getFont_botones());

        descripcion_label = new JLabel("Descripcion: ");
        descripcion_label.setFont(Styles.getFont_botones());

        importe_label = new JLabel("Importe: ");
        importe_label.setFont(Styles.getFont_botones());

        tipo_proyecto_label = new JLabel("Tipo proyecto: ");
        tipo_proyecto_label.setFont(Styles.getFont_botones());

        numero_votos_label = new JLabel("Numero de votos: ");
        numero_votos_label.setFont(Styles.getFont_botones());

        seguidores_label = new JLabel("Seguidores: ");
        seguidores_label.setFont(Styles.getFont_botones());

        representante_label = new JLabel("Representante: ");
        representante_label.setFont(Styles.getFont_botones());

        colectivos_seguidores_label= new JLabel("Colectivos seguidores: ");
        colectivos_seguidores_label.setFont(Styles.getFont_botones());

        colectivos_hijo_label = new JLabel("Colectivos hijo: ");
        colectivos_hijo_label.setFont(Styles.getFont_botones());

        grupo_social_label = new JLabel("Grupo social: ");
        grupo_social_label.setFont(Styles.getFont_botones());

        ambito_label = new JLabel("Ambito: ");
        ambito_label.setFont(Styles.getFont_botones());

        fecha_label = new JLabel("Fecha creacion: ");
        fecha_label.setFont(Styles.getFont_botones());

        esquemaGrafico_label = new JLabel("Esquema grafico: ");
        esquemaGrafico_label.setFont(Styles.getFont_botones());

        distrito_label = new JLabel("Distritos: ");
        distrito_label.setFont(Styles.getFont_botones());

        estadoProyecto_label = new JLabel("Estado: ");
        estadoProyecto_label.setFont(Styles.getFont_botones());

        rechazo_label = new JLabel("Motivo rechazo: ");
        rechazo_label.setFont(Styles.getFont_botones());

        importe_concedido_label = new JLabel("Importe concedido: ");
        importe_concedido_label.setFont(Styles.getFont_botones());

        creador_label = new JLabel("Creador");
        creador_label.setFont(Styles.getFont_botones());

        // Creamos los posibles botones del panel
        seguir_proyecto = new JButton("Seguir proyecto");
        seguir_proyecto.setFont(Styles.getFont_botones());

        abandonar_proyecto = new JButton("Abandonar proyecto");
        abandonar_proyecto.setFont(Styles.getFont_botones());

        seguir_colectivo = new JButton("Seguir colectivo");
        seguir_colectivo.setFont(Styles.getFont_botones());

        abandonar_colectivo = new JButton("Abandonar colectivo");
        abandonar_colectivo.setFont(Styles.getFont_botones());

        votar_proyecto = new JButton("Votar proyecto");
        votar_proyecto.setFont(Styles.getFont_botones());

        crear_proyecto = new JButton("Crear proyecto");
        crear_proyecto.setFont(Styles.getFont_botones());

        crear_colectivo_hijo = new JButton("Crear colectivo hijo");
        crear_colectivo_hijo.setFont(Styles.getFont_botones());

        solicitar_financiacion = new JButton("Solicitar financiacion");
        solicitar_financiacion.setFont(Styles.getFont_botones());

        comprobar_financiacion = new JButton("Comprobar financiacion");
        comprobar_financiacion.setFont(Styles.getFont_botones());

        // Creamos el controlador y se lo anadimos a los botones
        ControladorElemento controladorElemento = new ControladorElemento(aplicacion, this, panelMain);
        seguir_colectivo.addActionListener(controladorElemento);
        abandonar_colectivo.addActionListener(controladorElemento);
        seguir_proyecto.addActionListener(controladorElemento);
        abandonar_proyecto.addActionListener(controladorElemento);
        votar_proyecto.addActionListener(controladorElemento);
        crear_proyecto.addActionListener(controladorElemento);
        crear_colectivo_hijo.addActionListener(controladorElemento);
        solicitar_financiacion.addActionListener(controladorElemento);
        comprobar_financiacion.addActionListener(controladorElemento);

        setLayout(new MigLayout("wrap 2, fillx"));
    }

    /**
     * Metodo que refresca el panel y muestra un proyecto
     * @param proyecto Proyecto que se mostrara en el panel
     */
    public void refrescar(Proyecto proyecto){
        removeAll();
        updateUI();

        aplicacion.comprobarVotos();

        colectivo = null;
        this.proyecto = proyecto;

        add(panel_informacion);
        add(panel_acciones, "top, pushx, growx");

        panel_informacion.removeAll();
        panel_informacion.updateUI();

        panel_acciones.removeAll();
        panel_acciones.updateUI();

        // Anadimos las posibles acciones al panel de acciones
        if(aplicacion.getAdmin() == null){
            if(!proyecto.comprobarVotante(aplicacion.getUsuarioActual())){
                panel_acciones.add(votar_proyecto, "right");
            }
            if(proyecto.comprobarCreador(aplicacion.getUsuarioActual())){
                if(proyecto.isSolicitable()){
                    panel_acciones.add(solicitar_financiacion, "right");
                }
                else if(proyecto.isEspera()){
                    panel_acciones.add(comprobar_financiacion, "right");
                }
            }
            if(proyecto.comprobarSeguidor(aplicacion.getUsuarioActual())){
                panel_acciones.add(abandonar_proyecto, "right");
            }
            else{
                // Si el proyecto no esta pendiente ni rechazado se la da al usuario
                // la opcin de seguirlo
                if(!proyecto.isPendiente() && !proyecto.isRechazado())
                    panel_acciones.add(seguir_proyecto, "right");
            }
        }
        // Colocamos los componentes del panel de acciones del administrador
        else{
            if(proyecto.isPendiente()){
                JButton boton_aceptar = new JButton("Aceptar");
                boton_aceptar.setFont(Styles.getFont_botones());
                boton_aceptar.addActionListener(new ControladorElemento(aplicacion, this, panelMain));
                JButton boton_rechazar = new JButton("Rechazar");
                boton_rechazar.setFont(Styles.getFont_botones());
                boton_rechazar.addActionListener(new ControladorElemento(aplicacion, this, panelMain));
                panel_acciones.add(boton_aceptar, "right");
                panel_acciones.add(boton_rechazar, "right");
            }
        }

        // Creamos los componentes del panel de informacion
        JLabel nombre = new JLabel(proyecto.getProjectTitle());
        nombre.setFont(Styles.getFont_label());

        JLabel descripcion = new JLabel(proyecto.getProjectDescription());
        descripcion.setFont(Styles.getFont_label());

        JLabel importe = new JLabel(Double.toString(proyecto.getRequestedAmount()));
        importe.setFont(Styles.getFont_label());

        JLabel numero_votos = new JLabel(Integer.toString(proyecto.getVotos()));
        numero_votos.setFont(Styles.getFont_label());

        JLabel fecha = new JLabel(proyecto.getFechaProyecto().toString());
        fecha.setFont(Styles.getFont_label());

        JLabel estadoProyecto = new JLabel(proyecto.getEstado().toString());
        estadoProyecto.setFont(Styles.getFont_label());

        // Anadimos los componentes a la vista
        panel_informacion.add(nombre_label);
        panel_informacion.add(nombre);
        panel_informacion.add(descripcion_label);
        panel_informacion.add(descripcion);
        panel_informacion.add(importe_label);
        panel_informacion.add(importe);
        panel_informacion.add(fecha_label);
        panel_informacion.add(fecha);
        panel_informacion.add(creador_label);
        if(proyecto.getCreador() == null){
            JLabel creador = new JLabel(proyecto.getColectivoCreador().getNombre());
            creador.setFont(Styles.getFont_label());
            panel_informacion.add(creador);
        }
        else{
            JLabel creador = new JLabel(proyecto.getCreador().getNombreUsuario());
            creador.setFont(Styles.getFont_label());
            panel_informacion.add(creador);
        }
        panel_informacion.add(estadoProyecto_label);
        panel_informacion.add(estadoProyecto);
        if(proyecto.isRechazado()){
            panel_informacion.add(rechazo_label);
            JLabel rechazo = new JLabel(proyecto.getMotivoRechazo());
            rechazo.setFont(Styles.getFont_label());
            panel_informacion.add(rechazo);
        }

        if(proyecto.isFinanciado()){
            panel_informacion.add(importe_concedido_label);
            JLabel importe_concedido = new JLabel(Double.toString(proyecto.getImporteConcedido()));
            importe_concedido.setFont(Styles.getFont_label());
            panel_informacion.add(importe_concedido);

        }

        panel_informacion.add(tipo_proyecto_label);
        if(proyecto.getProjectKind().equals(GrantRequest.ProjectKind.Infrastructure)){
            JLabel tipo_proyecto = new JLabel("Infraestructura");
            tipo_proyecto.setFont(Styles.getFont_label());
            panel_informacion.add(tipo_proyecto);
            panel_informacion.add(esquemaGrafico_label);
            JLabel esquemaGrafico = new JLabel(((Infraestructura)proyecto).getEsquemaGrafico().getName());
            esquemaGrafico.setFont(Styles.getFont_label());
            panel_informacion.add(esquemaGrafico);
            panel_informacion.add(distrito_label, "top");
            JPanel distritos = new JPanel();
            distritos.setLayout(new MigLayout("insets 0 0 0 0"));
            for(Distrito d : ((Infraestructura)proyecto).getDistritos()){
                JPanel contenido_distrito = new JPanel();
                JLabel distrito = new JLabel(d.toString());
                distrito.setFont(Styles.getFont_label());
                contenido_distrito.add(distrito);
                contenido_distrito.setBorder(BorderFactory.createLineBorder(Color.black));
                distritos.add(contenido_distrito, "wrap 0, pushx, growx");
            }
            JScrollPane scrollable_distritos = new JScrollPane(distritos);
            scrollable_distritos.setBorder(BorderFactory.createEmptyBorder());
            panel_informacion.add(scrollable_distritos, "height 200");

        }else{
            JLabel tipo_proyecto = new JLabel("Social");
            tipo_proyecto.setFont(Styles.getFont_label());
            panel_informacion.add(tipo_proyecto);
            panel_informacion.add(grupo_social_label);
            JLabel grupo_social = new JLabel(((ProyectoSocial)proyecto).getGrupoSocial());
            grupo_social.setFont(Styles.getFont_label());
            panel_informacion.add(grupo_social);
            panel_informacion.add(ambito_label);
            JLabel ambito = new JLabel(((ProyectoSocial)proyecto).getAmbito().toString());
            ambito.setFont(Styles.getFont_label());
            panel_informacion.add(ambito);
        }
        // Solo mostraremos el numero de votos si el usuario actual ha votado el proyecto
        if (aplicacion.getAdmin() == null) {
            if(proyecto.comprobarVotante(aplicacion.getUsuarioActual())){
                panel_informacion.add(numero_votos_label);
                panel_informacion.add(numero_votos);
            }
        }
        else{
            panel_informacion.add(numero_votos_label);
            panel_informacion.add(numero_votos);
        }
        panel_informacion.add(seguidores_label, "top");

        if(proyecto.getSeguidores().size() == 0){
            JLabel no_seguidores = new JLabel("Ningun usuario seguidor");
            no_seguidores.setFont(Styles.getFont_label());
            panel_informacion.add(no_seguidores);
        }
        else{
            JPanel seguidores = new JPanel();
            seguidores.setLayout(new MigLayout("insets 0 0 0 0"));
            for(UsuarioRegistrado u : proyecto.getSeguidores()){
                JPanel contenedor_usuario = new JPanel();
                JLabel usuario = new JLabel(u.getNombreUsuario());
                usuario.setFont(Styles.getFont_label());
                contenedor_usuario.add(usuario);
                contenedor_usuario.setBorder(BorderFactory.createLineBorder(Color.black));
                seguidores.add(contenedor_usuario, "pushx, growx, wrap 0");
            }
            JScrollPane scrollable = new JScrollPane(seguidores);
            scrollable.setBorder(BorderFactory.createEmptyBorder());
            panel_informacion.add(scrollable, "width 200, height 200");
        }

        panel_informacion.add(colectivos_seguidores_label, "top");

        if(proyecto.getColectivosSeguidores().size() == 0){
            JLabel no_colectivos = new JLabel("Ningun colectivo seguidor");
            no_colectivos.setFont(Styles.getFont_label());
            panel_informacion.add(no_colectivos);
        }
        else{
            JPanel colectivos_seguidores = new JPanel();
            colectivos_seguidores.setLayout(new MigLayout("insets 0 0 0 0"));
            for(Colectivo c : proyecto.getColectivosSeguidores()){
                JPanel contenedor_colectivo = new JPanel();
                JLabel colectivo = new JLabel(c.getNombre());
                colectivo.setFont(Styles.getFont_label());
                contenedor_colectivo.add(colectivo);
                contenedor_colectivo.setBorder(BorderFactory.createLineBorder(Color.black));
                contenedor_colectivo.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent mouseEvent) {
                        panelMain.mostrar_elemento(c);
                    }
                });
                colectivos_seguidores.add(contenedor_colectivo, "pushx, growx, wrap 0");

            }
            JScrollPane scrollable_colectivos = new JScrollPane(colectivos_seguidores);
            scrollable_colectivos.setBorder(BorderFactory.createEmptyBorder());
            panel_informacion.add(scrollable_colectivos, "height 200");
        }
    }

    /**
     * Metodo que refresca el panel para mostrar un colectivo
     * @param colectivo Colectivo que se va a mostrar
     */
    public void refrescar(Colectivo colectivo){
        removeAll();
        updateUI();

        proyecto = null;
        this.colectivo = colectivo;

        add(panel_informacion);
        add(panel_acciones, "top, pushx, growx");

        panel_informacion.removeAll();
        panel_informacion.updateUI();

        panel_acciones.removeAll();
        panel_acciones.updateUI();

        // Colocamos los compontes del panel de acciones del usuario
        if (aplicacion.getAdmin() == null) {
            if(colectivo.comprobarCreador(aplicacion.getUsuarioActual().getNombreUsuario())){
                panel_acciones.add(crear_colectivo_hijo, "right");
                panel_acciones.add(crear_proyecto, "right");
            }
            // Comprobamos si el usuario puede seguir al colectivo
            if(aplicacion.comprobarSeguir(colectivo)){
                panel_acciones.add(seguir_colectivo, "right");
            }
            // Si no puede seguirlo comprobamos si puede abandonarlo
            else{
                if(colectivo.comprobarUsuario(aplicacion.getUsuarioActual())){
                    panel_acciones.add(abandonar_colectivo, "right");
                }
            }
        }

        // Creamos los componentes del panel de informacion
        JLabel nombre = new JLabel(colectivo.getNombre());
        nombre.setFont(Styles.getFont_label());

        JLabel representante = new JLabel(colectivo.getRepresentante().getNombreUsuario());
        representante.setFont(Styles.getFont_label());

        // Anadimos los componentes al panel
        panel_informacion.add(nombre_label);
        panel_informacion.add(nombre);
        panel_informacion.add(representante_label);
        panel_informacion.add(representante);
        panel_informacion.add(seguidores_label, "top");
        if(colectivo.getParticipantes().size() == 0){
            JLabel no_seguidores = new JLabel("Ningun seguidor");
            no_seguidores.setFont(Styles.getFont_label());
            panel_informacion.add(no_seguidores);
        }
        else {
            JPanel seguidores = new JPanel();
            seguidores.setLayout(new MigLayout("insets 0 0 0 0"));
            for(UsuarioRegistrado u : colectivo.getParticipantes()){
                JPanel contenedor_usuario = new JPanel();
                JLabel usuario = new JLabel(u.getNombreUsuario());
                usuario.setFont(Styles.getFont_label());
                contenedor_usuario.add(usuario);
                contenedor_usuario.setBorder(BorderFactory.createLineBorder(Color.black));
                seguidores.add(contenedor_usuario, "pushx, growx, wrap 0");
            }

            // Anadimos el scrollable
            JScrollPane scrollable = new JScrollPane(seguidores);
            scrollable.setBorder(BorderFactory.createEmptyBorder());
            panel_informacion.add(scrollable, "height 200");
        }
        panel_informacion.add(colectivos_hijo_label, "top");
        if(colectivo.getColectivosHijo().size() == 0){
            JLabel no_hijos = new JLabel("Ningun colectivo hijo");
            no_hijos.setFont(Styles.getFont_label());
            panel_informacion.add(no_hijos);
        }
        else{
            JPanel colectivos_hijo = new JPanel();
            colectivos_hijo.setLayout(new MigLayout("insets 0 0 0 0"));

            for(Colectivo c : colectivo.getColectivosHijo()){
                JPanel panel_contenedor = new JPanel();
                panel_contenedor.setBorder(BorderFactory.createLineBorder(Color.black));
                JLabel colectivo_hijo = new JLabel(c.getNombre());
                colectivo_hijo.setFont(Styles.getFont_label());
                panel_contenedor.add(colectivo_hijo);
                panel_contenedor.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent mouseEvent) {
                        panelMain.mostrar_elemento(c);
                    }
                });
                colectivos_hijo.add(panel_contenedor, "pushx, growx, wrap 0");
            }

            JScrollPane scrollable_colectivos = new JScrollPane(colectivos_hijo);
            scrollable_colectivos.setBorder(BorderFactory.createEmptyBorder());
            scrollable_colectivos.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            panel_informacion.add(scrollable_colectivos, "height 200");
        }

        // Anadimos al panel para seguir proyecto desde colectivo si el usuario actual es el creador
        if(colectivo.comprobarCreador(aplicacion.getUsuarioActual().getNombreUsuario())){
            int tamano = aplicacion.getProyectosDisponibles().size() - aplicacion.getProyectosSeguidos(colectivo).size();
            if (tamano  != 0) {
                JLabel proyectos = new JLabel("Proyectos:");
                proyectos.setFont(Styles.getFont_botones());
                panel_informacion.add(proyectos);
                JPanel comboBox_panel = new JPanel();
                comboBox_panel.setLayout(new MigLayout("insets 0 0 0 10"));
                String[] comboBox_text = new String[tamano];
                int i = 0;
                for (Proyecto p : aplicacion.getProyectosDisponibles()) {
                    if(!p.isRechazado()){
                        if(!p.comprobarColectivoSeguidor(colectivo)){
                            comboBox_text[i] = p.getProjectTitle();
                            i++;
                        }
                    }
                }
                comboBox_proyectos = new JComboBox(comboBox_text);
                comboBox_proyectos.setFont(Styles.getFont_label());
                comboBox_panel.add(comboBox_proyectos);
                JButton seguir_proyecto = new JButton("Seguir proyecto");
                seguir_proyecto.setFont(Styles.getFont_botones());
                seguir_proyecto.addActionListener(new ControladorElemento(aplicacion, this, panelMain));
                comboBox_panel.add(seguir_proyecto);
                panel_informacion.add(comboBox_panel);
            }
        }

        // Anadimos el panel de consultar afinidad
        if(colectivo.comprobarUsuario(aplicacion.getUsuarioActual()) && aplicacion.getColectivosSeguidos().size() > 1) {
            JLabel afinidad = new JLabel("Afinidad: ");
            afinidad.setFont(Styles.getFont_botones());
            panel_informacion.add(afinidad);
            JPanel combobox_colectivosPanel = new JPanel();
            combobox_colectivosPanel.setLayout(new MigLayout("insets 0 0 0 10"));
            String[] comoBox_colectivosText = new String[aplicacion.getColectivosSeguidos().size() - 1];
            int i = 0;
            for(Colectivo c : aplicacion.getColectivosSeguidos()){
                if(!c.equals(colectivo)){
                    comoBox_colectivosText[i] = c.getNombre();
                    i++;
                }
            }
            comboBox_colectivos = new JComboBox(comoBox_colectivosText);
            comboBox_colectivos.setFont(Styles.getFont_label());
            JButton boton_afinidad = new JButton("Consultar afinidad");
            boton_afinidad.setFont(Styles.getFont_botones());
            boton_afinidad.addActionListener(new ControladorElemento(aplicacion, this, panelMain));
            combobox_colectivosPanel.add(comboBox_colectivos);
            combobox_colectivosPanel.add(boton_afinidad);
            panel_informacion.add(combobox_colectivosPanel);
        }
    }

    /**
     * Metodo general para refrescar el panel
     */
    public void refrescar(){
        if(colectivo == null) {
            refrescar(proyecto);
        }
        else{
            refrescar(colectivo);
        }
    }

    // Getters

    /**
     * Getter del colectivo que se muestra en el panel
     * @return Colectivo mostrado
     */
    public Colectivo getColectivo(){
        return colectivo;
    }


    /**
     * Getter del proyecto que se muestra en el panel
     * @return Proyecto mostrado
     */
    public Proyecto getProyecto(){
        return proyecto;
    }

    /**
     * Getter del proyecto seleccionador
     * @return Proyecto seleccionado
     */
    public Proyecto getSelectedProyecto() throws AplicacionExcepcion {
        return aplicacion.getProyecto_i(comboBox_proyectos.getSelectedIndex());
    }

    /**
     * Getter del colectivo seleccionado
     * @return Colectivo seleccionado
     */
    public Colectivo getSelectedColectivo() throws AplicacionExcepcion {
        return aplicacion.getColectivo_i(comboBox_colectivos.getSelectedIndex());
    }
}
