/** 
 * @brief It declares the constants used in tests
 * 
 * @file test.h
 * @author Profesores Pprog
 * @version 2.0 
 * @date 19-01-2016
 * @copyright GNU Public License
 */

#ifndef _TEST_H
#define _TEST_H

#pragma GCC diagnostic ignored "-Wpedantic"

#ifndef NOCOLOR
#define KRED  "\x1B[31m" /*!< Red color*/
#define KGRN  "\x1B[32m" /*!< Green color*/
#define KYEL  "\x1B[33m" /*!< Yellow color*/
#define KCYN  "\x1B[36m" /*!< Blue color*/
#define RESET "\033[0m" /*!< Defect color*/
#else
#define KRED
#define KGRN
#define KYEL
#define KCYN
#define RESET
#endif

#define PRINT_TEST_RESULT(x) do{\
    __test_counter++;\
    __pass = (x);\
    __test_passed = (__pass)? __test_passed + 1 : __test_passed;\
    printf(KYEL "%s" RESET " line "  "%d " KCYN "%s" RESET ": %s\n", \
	   __FILE__, __LINE__ , __FUNCTION__, \
	   ((!__pass) ? KRED "NOT PASS" RESET : KGRN "PASS" RESET));	\
  } while (0)

#define PRINT_PASSED_PERCENTAGE printf("Tests passed %d%%\n", ((__test_passed * 100) / __test_counter))
      
static int __test_counter = 0;
static int __test_passed  = 0;
static int __pass = 0;

#endif