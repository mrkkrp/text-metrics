#ifndef TEXT_METRICS_H
#define TEXT_METRICS_H

#include <stdint.h>
#include <stdlib.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define VLEN_MAX 255 /* Up to this length we use alloca. */

/* Levenshein variants */

unsigned int tmetrics_levenshtein (unsigned int, uint16_t *, unsigned int, uint16_t *);
unsigned int tmetrics_damerau_levenshtein (unsigned int, uint16_t *, unsigned int, uint16_t *);

/* Other */

unsigned int tmetrics_hamming (unsigned int, uint16_t *, uint16_t *);
void tmetrics_jaro (unsigned int *, unsigned int *, unsigned int, uint16_t *, unsigned int, uint16_t *);
unsigned int tmetrics_common_prefix (unsigned int, uint16_t *, unsigned int, uint16_t *);

#endif /* TEXT_METRICS_H */
