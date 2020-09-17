/** 
 * @brief It defines the music of the game
 * 
 * @file music.c
 * @author Tomas Higuera
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "music.h"

void music_start(char * song){
	char command[500];
	if(!song)	
		return;
	system("echo off");
	sprintf(command, "cvlc Audio/%s.mp3 &",song);
	system(command);
	system("clear");
}

void music_stop(){
	system("echo off");
	system("pkill -n vlc");
}
