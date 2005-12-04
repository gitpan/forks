#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#define MY_CXT_KEY "threads::shared::_guts" XS_VERSION

typedef struct {
    int dummy;          /* you can access this elsewhere as MY_CXT.dummy */
} my_cxt_t;

START_MY_CXT

void
exec_leave(pTHX_ U32 both) {
    U16 process;
    U16 ordinal;

    dSP;
    ENTER;
    SAVETMPS;

    process = both >> 16;
    ordinal = both & 0xFFFF;
  /*  printf ("unlock: ordinal = %d, process = %d\n",ordinal,process); */

    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSViv(ordinal)));
    PUTBACK;

    if (process == getpid()) {
        call_pv( "threads::shared::_unlock",G_DISCARD );
    }

    SPAGAIN;
    PUTBACK;
    FREETMPS;
    LEAVE;
}

MODULE = forks               PACKAGE = threads::shared

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)
# OUT: 1 reference to that variable

SV*
share(SV *ref)
    PROTOTYPE: \[$@%]
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newRV(ref)));
        PUTBACK;

        call_pv( "threads::shared::_share",G_DISCARD );

        FREETMPS;
        LEAVE;
        RETVAL = newRV(ref);
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)
# OUT: 1 reference to that variable

SV*
share_disabled(SV *ref)
    PROTOTYPE: \[$@%]
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);
        RETVAL = newRV(ref);
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
lock(SV *ref)
    PROTOTYPE: \[$@%]
    PPCODE:
    int count;
        U16 ordinal;
        U16 process;

        LEAVE;

        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_lock",0)));
        XPUSHs(sv_2mortal(newRV(ref)));
        PUTBACK;

        process = getpid();
        count = call_pv( "threads::shared::_remote",G_SCALAR );

        SPAGAIN;
    ordinal = POPi;
   /*     printf ("lock: ordinal = %d, process = %d\n",ordinal,process); */
        PUTBACK;

        FREETMPS;
        LEAVE;

        SAVEDESTRUCTOR_X(exec_leave,(process << 16) | ordinal);
        ENTER;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob) -- signal variable
#      2 any variable (scalar,array,hash,glob) -- lock variable

void
cond_wait(SV *ref, ...)
    PROTOTYPE: \[$@%];\[$@%]
    PREINIT:
        SV *ref2;
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);
        if (items > 1)
        {
            ref2 = SvRV(ST(1));
            if(SvROK(ref2))
                ref2 = SvRV(ref2);
        }
        
        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_wait",0)));
        XPUSHs(sv_2mortal(newRV(ref)));
        if (items > 1)
            XPUSHs(sv_2mortal(newRV(ref2)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob) -- signal variable
#      2 epoch time of event expiration
#      3 any variable (scalar,array,hash,glob) -- lock variable

bool
cond_timedwait(SV *ref, double epochts, ...)
    PROTOTYPE: \[$@%]$;\[$@%]
    PREINIT:
        SV *ref2;
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);
        if (items > 2)
        {
            ref2 = SvRV(ST(2));
            if(SvROK(ref2))
                ref2 = SvRV(ref2);
        }

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_timedwait",0)));
        XPUSHs(sv_2mortal(newRV(ref)));
        XPUSHs(sv_2mortal(newSVnv(abs(epochts))));
        if (items > 2)
            XPUSHs(sv_2mortal(newRV(ref2)));
        PUTBACK;

        int count = call_pv( "threads::shared::_remote",G_ARRAY );

        SPAGAIN;
        if (count != 2)
            croak ("Error receiving response value from _remote\n");

        bool retval = POPi;
        U16 ordinal = POPi;
        PUTBACK;

        FREETMPS;
        LEAVE;
        RETVAL = retval;
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
cond_signal(SV *ref)
    PROTOTYPE: \[$@%]
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_signal",0)));
        XPUSHs(sv_2mortal(newRV(ref)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
cond_broadcast(SV *ref)
    PROTOTYPE: \[$@%]
    CODE:
        ref = SvRV(ref);
        if(SvROK(ref))
            ref = SvRV(ref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_broadcast",0)));
        XPUSHs(sv_2mortal(newRV(ref)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------

BOOT:
{
    MY_CXT_INIT;
}
