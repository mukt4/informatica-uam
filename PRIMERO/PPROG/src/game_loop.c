/** 
 * @brief It defines the game loop 
 * 
 * @file game_loop.c
 * @author Profesores PPROG, Alvaro Lopez, Tomas Higuera & Guillermo Hoyo
 * @version 1.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "graphic_engine.h"
#include "command.h"
#include "menu.h"
#include "game_management.h"
#include "dialogue.h"
#include "music.h"

int main(int argc, char **argv){
	Game *game=game_create();
	STATUS status=OK;
	Graphic_engine *gengine;
	extern char *cmd_to_str[];
	FILE *pf;
	char command[55]="";
	char commandAux[55]="";
	char complement[55]="";
	char complementAux[55]="";
	T_Command *cmdAux=NULL;
	int option=-1;
	int aux, code=0;
	char file[50];
	char end[25];
	char conf;
	/*Carga del juego*/
	/*if (argc < 5){
		fprintf(stderr, "Use: %s <space_data_file> <object_data_file> <link_data_file> <player_data_file> <enemies_data_file>\n", argv[0]);
		game_destroy(game);
		return 1;
	}*/
	if ((gengine = graphic_engine_create()) == NULL){
		fprintf(stderr, "Error while initializing graphic engine.\n");
		game_destroy(game);
		return 1;
	}
	menu_logo();
	sleep(2);
	system("clear");
	/*Juego ejecutado con argumento -l que guarda en un fichero las acciones del juego con interfaz grafica*/
	if( argc>1 && (strcmp(argv[1],"-l")==0)){
		if (game_create_from_file(game, "spaces.dat","objects.dat","links.dat","players.dat","enemies.dat") == ERROR){
			fprintf(stderr, "Error while initializing game.\n");
			game_destroy(game);
			return 1;
		}
	    pf=fopen(argv[2],"w");
	    while ( (command_get_input(game_get_last_command(game)) != QUIT) && !game_is_over(game) ){
		    graphic_engine_paint_game(gengine, game,status);
		    get_user_input(game_get_last_command(game));
		    status=game_update(game, game_get_last_command(game), code);
		    if(command_get_input(game_get_last_command(game))!=NO_CMD){
                if(status==OK)
                    fprintf(pf, " %s %s: OK\n", cmd_to_str[command_get_input(game_get_last_command(game))-NO_CMD],command_get_complement1(game_get_last_command(game)));
                else
                    fprintf(pf, " %s %s: ERROR\n", cmd_to_str[command_get_input(game_get_last_command(game))-NO_CMD],command_get_complement1(game_get_last_command(game)));
			}
	    }
	    fclose(pf);
	}
	/*Juego ejecutado con argumento -nv -l que guarda en un fichero las acciones del juego sin interfaz grafica*/
	else if(argc>1 && (strcmp(argv[1],"-nv")==0)){
			if(argc>6 && (strcmp(argv[2],"-l")==0)){
				if (game_create_from_file(game, "spaces.dat","objects.dat","links.dat","players.dat","enemies.dat") == ERROR){
					fprintf(stderr, "Error while initializing game.\n");
					game_destroy(game);
					return 1;
				}
				pf=fopen(argv[3],"w");
	    		while ( (command_get_input(game_get_last_command(game)) != QUIT) && !game_is_over(game) ){
		    		get_user_input(game_get_last_command(game));
		    		status=game_update(game, game_get_last_command(game), code);
		    		if(command_get_input(game_get_last_command(game))!=NO_CMD){
		                if(status==OK)
		                    fprintf(pf, " %s %s: OK\n", cmd_to_str[command_get_input(game_get_last_command(game))-NO_CMD],command_get_complement1(game_get_last_command(game)));
		                else
			                fprintf(pf, " %s %s: ERROR\n", cmd_to_str[command_get_input(game_get_last_command(game))-NO_CMD],command_get_complement1(game_get_last_command(game)));
					}	
				}
				fclose(pf);
			}
			else{
	    		fprintf(stdout,"Incorrect format -nv -l <file>\n");
			}
		}
		/*Juego que carga las acciones de un fichero y muestra los resultados en otro fichero que se introduce como parametro*/
		else if(argc>3){
			if (game_create_from_file(game, "spaces.dat","objects.dat","links.dat","players.dat","enemies.dat") == ERROR){
				fprintf(stderr, "Error while initializing game.\n");
				game_destroy(game);
				return 1;
			}
			pf=fopen(argv[3],"w");
			cmdAux=game_get_last_command(game);
			if(pf!=NULL){
				strcpy(command,"UNKNOWN");
				strcpy(complement,"\0");
				command_set_input(cmdAux,command);
				command_set_complement1(cmdAux,complement);
				while ( (command_get_input(game_get_last_command(game)) != QUIT) && !game_is_over(game) ){
					scanf("%s",command);
					command_set_input(cmdAux,command);
					if(command_get_input(cmdAux)==QUIT || command_get_input(cmdAux) == UNKNOWN){
						command_set_complement1(cmdAux,"\0");
					}
					else{
						scanf("%s",complement);
						command_set_complement1(cmdAux,complement);
					}
					status=game_update(game, cmdAux, code);
			    	if(command_get_input(game_get_last_command(game))!=NO_CMD){
				        if(status==OK)
				            fprintf(pf, "%s: OK\n",dialogue_get(game_get_dialogue(game)));
				         else
					        fprintf(pf, "%s: ERROR\n",dialogue_get(game_get_dialogue(game)));
					}	
				}
				fclose(pf);
			}
		}
	/*Juego interactivo con interfaz grafica*/
	else{
		while(option<=0 || option>6){
			aux=-1;
			menu_ini();
			scanf("%d",&option);
			switch(option){
				case 1:
					if (game_create_from_file(game, "spaces.dat","objects.dat","links.dat","players.dat","enemies.dat") == ERROR){
						fprintf(stderr, "Error while initializing game.\n");
						game_destroy(game);
						return 1;
					}
					break;
				case 2:
					system("clear");
					fprintf(stdout, "\n\n\n\n\nIntroduce el nombre del fichero de carga:");
					scanf("\n%s",file);
					menu_load();
					game_management_load(game,file);
					if(!game){
						fprintf(stdout,"There is no file with that name\n");
						option=-1;
					}
					break;
				case 3:
					while(aux<=0 || aux>6){
					menu_options();
					scanf("%d",&aux);
						switch(aux){
							case 1:
								system("clear");
								fprintf(stdout,"Deseas activar la musica que sonara al final del juego(y or n):");
								scanf("\n%c",&conf);
								if(conf=='y')
									game_set_music(game,TRUE);
								else
									game_set_music(game,FALSE);
								option=-1;
								aux=-1;
								break;
							case 2:
								system("clear");
								fprintf(stdout,"Solo con la version premium\n");
								sleep(2);
								system("clear");
								option=-1;
								aux=-1;
								break;
							case 3:
								system("clear");
								fprintf(stdout,"Deseas activar reglas extras cada turno en el juego, si lo haces se volvera mas complicado...(y or n):");
								scanf("\n%c",&conf);
								if(conf=='y')
									game_set_rule(game,TRUE);
								else
									game_set_rule(game,FALSE);
								option=-1;
								aux=-1;
								break;
							case 4:
								system("clear");
								fprintf(stdout,"Deseas activar la funcion de autoguardado(y or n):");
								scanf("\n%c",&conf);
								if(conf=='y')
									game_set_save(game,TRUE);
								else
									game_set_save(game,FALSE);
								option=-1;
								aux=-1;
								break;
							case 5:
								system("clear");
								fprintf(stdout,"El idioma esta restringido a spaninglish, con la version premium gozaras de muchas mas opciones\n");
								sleep(2);
								system("clear");
								option=-1;
								aux=-1;
								break;
							case 6:
								option=-1;
								break;
							default:
								option=-1;
								break;
						}
					}
					break;
				case 4:
					do{
						menu_controls();
						scanf("\n%s",end);
					}
					while(strcmp(end,"exit")!=0);
					option=-1;
					break;
				case 5:
					menu_additional_information();
					sleep(3);
					option=-1;
					break;
				case 6:
					return 0;
				default:
					option=-1;
					break;
			}
		}
		if(game_get_space(game,1)!=NULL){
			while ( (command_get_input(game_get_last_command(game)) != QUIT) && !game_is_over(game) && !game_finished(game)){
				graphic_engine_paint_game(gengine, game,status);
				get_user_input(game_get_last_command(game));
				strcpy(commandAux,command);
				strcpy(complementAux, complement);
				strcpy(command,cmd_to_str[command_get_input(game_get_last_command(game))-NO_CMD]);
				strcpy(complement,command_get_complement1(game_get_last_command(game)));
	            
	            if (!strcmp(commandAux, command) && !strcmp(complement, complementAux)) {
	            	if(status==ERROR)
	            		code++;
	            }
	            else{
	            	code = 0;
	            }
				status=game_update(game, game_get_last_command(game), code);
			}
		}
		if(game_is_over(game)==TRUE){
			if(game_get_music(game)==TRUE)
				music_start("end");
			menu_game_over();
			sleep(3);
		}
		else if(game_finished(game)==TRUE){
			if(game_get_music(game)==TRUE)
				music_start("win");
			menu_end();
			sleep(3);
		}
	}
	if(game)
		game_destroy(game);
	graphic_engine_destroy(gengine);
	return 0;
}