/** 
 * @brief It defines the music of the game
 * 
 * @file music.h
 * @author Tomas Higuera
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */
 
#ifndef MUSIC_H
#define MUSIC_H

/**
* @brief It starts the music
* @author Tomas Higuera Viso
* 
* music_start(char *song) It plays a song with a determined name
* @param The nme of the song
*/
void music_start(char *song);

/**
* @brief It stops the music player the music
* @author Tomas Higuera Viso
* 
* music_stop It stops the music player
* @param None
*/
void music_stop();

#endif