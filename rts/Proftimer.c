/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Profiling.h"
#include "Proftimer.h"
#include "Capability.h"
#include "Trace.h"

#if defined(PROFILING)
static bool do_prof_ticks = false;       // enable profiling ticks
#endif

static bool do_heap_prof_ticks = false;  // enable heap profiling ticks

// Sampling of Ticky-Ticky profiler to eventlog
#if defined(TICKY_TICKY) && defined(TRACING)
static int ticks_to_ticky_sample = 0;
bool performTickySample = false;
#endif

// Number of ticks until next heap census
static int ticks_to_heap_profile;

// Time for a heap profile on the next context switch
bool performHeapProfile;

void
stopProfTimer( void )
{
#if defined(PROFILING)
    RELAXED_STORE(&do_prof_ticks, false);
#endif
}

void
startProfTimer( void )
{
#if defined(PROFILING)
    RELAXED_STORE(&do_prof_ticks, true);
#endif
}

void
stopHeapProfTimer( void )
{
    RELAXED_STORE(&do_heap_prof_ticks, false);
}

void
startHeapProfTimer( void )
{
    if (RtsFlags.ProfFlags.doHeapProfile &&
        RtsFlags.ProfFlags.heapProfileIntervalTicks > 0) {
        do_heap_prof_ticks = true;
    }
}

void
initProfTimer( void )
{
    performHeapProfile = false;

    ticks_to_heap_profile = RtsFlags.ProfFlags.heapProfileIntervalTicks;

    startHeapProfTimer();
}

uint32_t total_ticks = 0;

void
handleProfTick(void)
{
#if defined(PROFILING)
    total_ticks++;
    if (RELAXED_LOAD(&do_prof_ticks)) {
        uint32_t n;
        for (n=0; n < n_capabilities; n++) {
            capabilities[n]->r.rCCCS->time_ticks++;
            traceProfSampleCostCentre(capabilities[n], capabilities[n]->r.rCCCS, total_ticks);
        }
    }
#endif

#if defined(TICKY_TICKY) && defined(TRACING)
    if (RtsFlags.TraceFlags.ticky) {
        ticks_to_ticky_sample--;
        if (ticks_to_ticky_sample <= 0) {
            ticks_to_ticky_sample = RtsFlags.ProfFlags.heapProfileIntervalTicks;
            performTickySample = true;
        }
    }
#endif

    if (RELAXED_LOAD(&do_heap_prof_ticks)) {
        ticks_to_heap_profile--;
        if (ticks_to_heap_profile <= 0) {
            ticks_to_heap_profile = RtsFlags.ProfFlags.heapProfileIntervalTicks;
            performHeapProfile = true;
        }
    }
}
