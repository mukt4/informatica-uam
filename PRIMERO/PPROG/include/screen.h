/** 
 * @brief It defines a screen
 * 
 * @file screen.h
 * @author Profesores PPROG,Tomas Higuera
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#ifndef __SCREEN__
#define __SCREEN__

#define SCREEN_MAX_STR 200 /*!< Max number of characters in the screen */

typedef struct _Area Area;

/**
* @brief It initialises the screen
* @author Tomas Higuera
*
* screen_init() It initialises the screen with '~'
* @param None
* @return None
*/
void  screen_init();

/**
* @brief It destroy the screen
* @author Tomas Higuera
*
* screen_destroy() It liberates the memory allocated by __data
* @param None
* @return None
*/
void  screen_destroy();

/**
* @brief It paints the screen
* @author Tomas Higuera
*
* screen_paint() It clears the terminal and paint the screen
* @param None
* @return None
*/
void  screen_paint();

/**
* @brief It paints the zone of the screen where you write commands
* @author Tomas Higuera
*
* screen_gets(char * str) It paints the zone of the screen where you write commands
* @param str 
* @return None
*/
void  screen_gets(char *str);

/**
* @brief It initialises the screen
* @author Tomas Higuera
*
* screen_area_init(int x,int y, int width, int height) It initialises the 
* screen with it proper edges
* @param x
* @param y
* @param width
* @param height
* @return Area* 
*/
Area* screen_area_init(int x, int y, int width, int height);

/**
* @brief It destroy an objet of the type Area
* @author Tomas Higuera
*
* screen_area_destroy(Area * area) It liberates the memory allocated by an 
* object of the type Area
* @param area An object of the type Area
* @return None
*/
void  screen_area_destroy(Area* area);

/**
* @brief It clears the screen
* @author Tomas Higuera
*
* screen_area_clear(Area * area) It clears the screen
* @param area An object of the type Area
* @return None
*/
void  screen_area_clear(Area* area);

/**
* @brief It resets the cursor
* @author Tomas Higuera
*
* screen_area_reset_cursor(Area * area)  It resets the cursor
* @param area An object of the type Area
* @return None
*/
void  screen_area_reset_cursor(Area* area);

/**
* @brief It puts a char* int the area
* @author Tomas Higuera
*
* screen_area_puts(Area * area, char *str) It puts a char* int the area
* @param area An object of the type Area
* @param str The string that is going to be added to area
* @return None
*/
void  screen_area_puts(Area* area, char *str);

#endif
