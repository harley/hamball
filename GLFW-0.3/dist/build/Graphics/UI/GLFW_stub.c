#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
void GraphicsziUIziGLFW_d3gc(StgStablePtr the_stableptr)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,(StgClosure*)deRefStablePtr(the_stableptr)) ,&ret);
rts_checkSchedStatus("GraphicsziUIziGLFW_d3gc",cap);
rts_unlock(cap);
}
 
void GraphicsziUIziGLFW_d3gx(StgStablePtr the_stableptr, HsInt a1)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkInt(cap,a1))) ,&ret);
rts_checkSchedStatus("GraphicsziUIziGLFW_d3gx",cap);
rts_unlock(cap);
}
 
void GraphicsziUIziGLFW_d3gG(StgStablePtr the_stableptr, HsInt32 a1, HsInt32 a2)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkInt32(cap,a1)),rts_mkInt32(cap,a2))) ,&ret);
rts_checkSchedStatus("GraphicsziUIziGLFW_d3gG",cap);
rts_unlock(cap);
}
 
void GraphicsziUIziGLFW_d3gP(StgStablePtr the_stableptr, HsInt a1, HsInt a2)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,(StgClosure*)deRefStablePtr(the_stableptr),rts_mkInt(cap,a1)),rts_mkInt(cap,a2))) ,&ret);
rts_checkSchedStatus("GraphicsziUIziGLFW_d3gP",cap);
rts_unlock(cap);
}
#ifdef __cplusplus
}
#endif

