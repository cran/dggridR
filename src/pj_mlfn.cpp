#ifndef DGGRIDR
#define DGGRIDR
#endif
#ifndef lint
//static const char SCCSID[]="@(#)pj_mlfn.c	4.3	93/06/12	GIE	REL";
#endif
#include "proj4.h"
/* meridinal distance for ellipsoid and inverse
**	8th degree - accurate to < 1e-5 meters when used in conjuction
**		with typical major axis values.
**	Inverse determines phi to EPS (1e-11) radians, about 1e-6 seconds.
*/

#define C00 1.
#define C02 .25
#define C04 .046875
#define C06 .01953125
#define C08 .01068115234375
#define C22 .75
#define C44 .46875
#define C46 .01302083333333333333
#define C48 .00712076822916666666
#define C66 .36458333333333333333
#define C68 .00569661458333333333
#define C88 .3076171875
#define EPS 1e-11
#define MAX_ITER 10
#define EN_SIZE 5
	long double *
pj_enfn(long double es) {
	long double t, *en;

	if ( (en = (long double *)malloc(EN_SIZE * sizeof(long double))) ) {
		en[0] = C00 - es * (C02 + es * (C04 + es * (C06 + es * C08)));
		en[1] = es * (C22 - es * (C04 + es * (C06 + es * C08)));
		en[2] = (t = es * es) * (C44 - es * (C46 + es * C48));
		en[3] = (t *= es) * (C66 - es * C68);
		en[4] = t * es * C88;
	} /* else return NULL if unable to allocate memory */
	return en;
}
	long double
pj_mlfn(long double phi, long double sphi, long double cphi, long double *en) {
	cphi *= sphi;
	sphi *= sphi;
	return(en[0] * phi - cphi * (en[1] + sphi*(en[2]
		+ sphi*(en[3] + sphi*en[4]))));
}
	long double
pj_inv_mlfn(long double arg, long double es, long double *en) {
	long double s, t, phi, k = 1.-es;
	int i;

	phi = arg;
	for (i = MAX_ITER; i ; --i) { /* rarely goes over 5 iterations */
		s = sin(phi);
		t = 1. - es * s * s;
		t = (pj_mlfn(phi, s, cos(phi), en) - arg) / ( k * t * sqrt(t));
		phi -= t;
		if (fabsl(t) < EPS)
			break;
	}
/*
	if (i <= 0)
		pj_errno = -17;
*/
	return phi;
}
