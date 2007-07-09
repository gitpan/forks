#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define NEED_PL_signals
#include "ppport.h"

#define MY_CXT_KEY "threads::shared::_guts" XS_VERSION

typedef struct {
    int dummy;          /* you can access this elsewhere as MY_CXT.dummy */
} my_cxt_t;

START_MY_CXT

void
exec_leave(pTHX_ SV *both) {
    U16 process;
    U32 ordinal;
    AV *av_ord_lock;

    dSP;
    ENTER;
    SAVETMPS;

    av_ord_lock = (AV*)SvRV(both);
    process = (U16)SvUV((SV*)*av_fetch(av_ord_lock, 1, 0));
    ordinal = (U32)SvUV((SV*)*av_fetch(av_ord_lock, 2, 0));
  /*  printf ("unlock: ordinal = %d, process = %d\n",ordinal,process); */
    SvREFCNT_dec(av_ord_lock);
    SvREFCNT_dec(both);

    PUSHMARK(SP);
    XPUSHs(sv_2mortal(newSVuv(ordinal)));
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
# OUT: 1 boolean value indicating whether unsafe signals are in use 

bool
_check_pl_signal_unsafe_flag()
    PREINIT:
        U32 flags;
    CODE:
        flags = PL_signals & PERL_SIGNALS_UNSAFE_FLAG;
        if (flags == 0) {
            RETVAL = 0;
        } else {
            RETVAL = 1;
        }
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)
# OUT: 1 reference to that variable

SV*
share(SV *myref)
    PROTOTYPE: \[$@%]
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        PUTBACK;

        call_pv( "threads::shared::_share",G_DISCARD );

        FREETMPS;
        LEAVE;
        RETVAL = newRV_inc(myref);
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)
# OUT: 1 reference to that variable

SV*
share_disabled(SV *myref)
    PROTOTYPE: \[$@%]
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);
        RETVAL = newRV_inc(myref);
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
lock(SV *myref)
    PROTOTYPE: \[$@%]
    PPCODE:
        int count;
        U16 process;
        U32 ordinal;
        AV *av_ord_lock;

        LEAVE;

        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_lock",0)));
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        PUTBACK;

        process = getpid();
        count = call_pv( "threads::shared::_remote",G_SCALAR );

        SPAGAIN;
        ordinal = POPl;
   /*     printf ("lock: ordinal = %d, process = %d\n",ordinal,process); */
        PUTBACK;

        FREETMPS;
        LEAVE;
        
        av_ord_lock = newAV();
        av_store(av_ord_lock, 1, newSVuv(process));
        av_store(av_ord_lock, 2, newSVuv(ordinal));

        SAVEDESTRUCTOR_X(exec_leave,newRV((SV*)av_ord_lock));
        ENTER;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob) -- signal variable
#      2 any variable (scalar,array,hash,glob) -- lock variable

void
cond_wait(SV *myref, ...)
    PROTOTYPE: \[$@%];\[$@%]
    PREINIT:
        SV *myref2;
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);
        if (items > 1)
        {
            myref2 = SvRV(ST(1));
            if(SvROK(myref2))
                myref2 = SvRV(myref2);
        }
        
        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_wait",0)));
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        if (items > 1)
            XPUSHs(sv_2mortal(newRV_inc(myref2)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob) -- signal variable
#      2 epoch time of event expiration
#      3 any variable (scalar,array,hash,glob) -- lock variable

bool
cond_timedwait(SV *myref, double epochts, ...)
    PROTOTYPE: \[$@%]$;\[$@%]
    PREINIT:
        SV *myref2;
        int count;
        bool retval;
        U32 ordinal;
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);
        if (items > 2)
        {
            myref2 = SvRV(ST(2));
            if(SvROK(myref2))
                myref2 = SvRV(myref2);
        }

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_timedwait",0)));
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        XPUSHs(sv_2mortal(newSVnv(epochts)));
        if (items > 2)
            XPUSHs(sv_2mortal(newRV_inc(myref2)));
        PUTBACK;

        count = call_pv( "threads::shared::_remote",G_ARRAY );

        SPAGAIN;
        if (count != 2)
            croak ("Error receiving response value from _remote\n");

        retval = POPi;
        ordinal = POPi;
        PUTBACK;

        FREETMPS;
        LEAVE;
        RETVAL = retval;
    OUTPUT:
        RETVAL

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
cond_signal(SV *myref)
    PROTOTYPE: \[$@%]
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_signal",0)));
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------
#  IN: 1 any variable (scalar,array,hash,glob)

void
cond_broadcast(SV *myref)
    PROTOTYPE: \[$@%]
    CODE:
        myref = SvRV(myref);
        if(SvROK(myref))
            myref = SvRV(myref);

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newSVpv("_broadcast",0)));
        XPUSHs(sv_2mortal(newRV_inc(myref)));
        PUTBACK;

        call_pv( "threads::shared::_remote",G_DISCARD );

        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------
#  IN: 1 scalar
#  IN: 1 optional scalar

void
bless(SV *myref, ...)
    PROTOTYPE: $;$
    PREINIT:
        HV* stash;
        SV* classname;
        STRLEN len;
        char *ptr;
        SV* myref2;
    CODE:
        if (items == 1) {
            stash = CopSTASH(PL_curcop);
        } else {
            classname = ST(1);

            if (classname &&
                ! SvGMAGICAL(classname) &&
                ! SvAMAGIC(classname) &&
                SvROK(classname))
            {
                Perl_croak(aTHX_ "Attempt to bless into a reference");
            }
            ptr = SvPV(classname, len);
            if (ckWARN(WARN_MISC) && len == 0) {
                Perl_warner(aTHX_ packWARN(WARN_MISC),
                        "Explicit blessing to '' (assuming package main)");
            }
            stash = gv_stashpvn(ptr, len, TRUE);
        }
        SvREFCNT_inc(myref);
        (void)sv_bless(myref, stash);
        ST(0) = sv_2mortal(myref);
        
        myref2 = SvRV(myref);
        if(SvROK(myref2)) {
            myref2 = SvRV(myref2);
        }

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv_2mortal(newRV(myref2)));
        XPUSHs(sv_2mortal(newSVpv(HvNAME(stash), 0)));
        PUTBACK;

        call_pv( "threads::shared::_bless",G_DISCARD );
        
        FREETMPS;
        LEAVE;

#----------------------------------------------------------------------

BOOT:
{
    MY_CXT_INIT;
}
