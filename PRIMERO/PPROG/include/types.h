/** 
 * @brief It defines common types
 * 
 * @file types.h
 * @author Profesores PPROG
 * @version 1.0 
 * @date 13-01-2015
 * @copyright GNU Public License
 */

#ifndef TYPES_H
#define TYPES_H

#define WORD_SIZE 1000 /*!< The MAX size of the map */
#define NO_ID -1  /*!< The value of an Id that is not in the game */
#define KILL -2

typedef long Id;

typedef enum {
  FALSE, TRUE  /*!< FALSE=0, TRUE=1 */
} BOOL; 

typedef enum {
    ERROR, OK, ERROR2, ERROR3 /*!< ERROR=0, OK=1, ERROR2=2, ERROR3=3 */
} STATUS;

typedef enum {
    N, S, E, W /*!< N=0, S=1, E=2, W=3 */
} DIRECTION;

#endif
