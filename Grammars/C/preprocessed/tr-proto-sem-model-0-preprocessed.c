static int g1;

static int g2;

static const char PromelaModelProtoSem[] = "/PML-ProtoSem";


int update1(int *p1, int *p2) {
  *p1 = *p2+10 ;
  *p2 = *p1*2 ;
}

int update2(int* p1, int *p2) {
  *p1 = *p2+5 ;
  *p2 = *p1*3 ;
}

rtems_event_set GetPending( Context *ctx )
{
  rtems_event_set pending;
  rtems_status_code sc;

  sc = ( *ctx->receive )(
    RTEMS_PENDING_EVENTS,
    RTEMS_DEFAULT_OPTIONS,
    0,
    &pending
  );
  T_quiet_rsc_success( sc );

  return pending;
}

rtems_id mapid( Context *ctx, int pid )
{
  rtems_id mapped_id;

  switch ( pid ) {
    case 1 : mapped_id = ctx->worker_id ; break;
    case 2 : mapped_id = ctx->runner_id; break;
    default : mapped_id = 0xffffffff; break;
  }
  return mapped_id;
}

void initialise_pending( rtems_event_set pending[], int max )
{
  int i;

  for( i=0; i < max; i++ ) {
    pending[i] = 0;
  }
}



static void TestSegment0( Context* ctx ) {


  T_log(T_NORMAL,"@@@ 0 INIT");



}



static void TestSegment1( Context* ctx ) { }



static void TestSegment2( Context* ctx ) { }

static void TestSegment3( Context* ctx ) {

  T_log(T_NORMAL,"@@@ 3 TASK Runner");
  checkTaskIs( ctx->runner_id );

  T_log(T_NORMAL,"@@@ 3 SCALAR g1 15");
  T_eq_int( g1, 15 );
  T_log(T_NORMAL,"@@@ 3 SCALAR g2 5");
  T_eq_int( g2, 5 );
  T_log(T_NORMAL,"@@@ 3 CALL update1");
  T_log( T_NORMAL, "Calling update1()" );
  update1(&g1,&g2);

  T_log(T_NORMAL,"@@@ 3 SCALAR g1 15");
  T_eq_int( g1, 15 );
  T_log(T_NORMAL,"@@@ 3 SCALAR g2 30");
  T_eq_int( g2, 30 );
  T_log(T_NORMAL,"@@@ 3 STATE 1 Zombie");
}



static void TestSegment4( Context* ctx ) {

  T_log(T_NORMAL,"@@@ 4 TASK Worker");
  checkTaskIs( ctx->worker_id );

  T_log(T_NORMAL,"@@@ 4 SCALAR g1 0");
  T_eq_int( g1, 0 );
  T_log(T_NORMAL,"@@@ 4 SCALAR g2 0");
  T_eq_int( g2, 0 );
  T_log(T_NORMAL,"@@@ 4 CALL update2");
  T_log( T_NORMAL, "Calling update2()" );
  update2(&g1,&g2);

  T_log(T_NORMAL,"@@@ 4 SCALAR g1 15");
  T_eq_int( g1, 15 );
  T_log(T_NORMAL,"@@@ 4 SCALAR g2 5");
  T_eq_int( g2, 5 );
  T_log(T_NORMAL,"@@@ 4 STATE 2 Zombie");

}

static void Runner( RtemsModelProtoSem_Context *ctx )
{
  T_log( T_NORMAL, "Runner running" );
  TestSegment3( ctx );
  T_log( T_NORMAL, "Runner finished" );
}

static void Worker0( rtems_task_argument arg )
{
  Context *ctx;
  rtems_event_set events;

  ctx = (Context *) arg;

  T_log( T_NORMAL, "Worker Running" );
  TestSegment4( ctx );
  T_log( T_NORMAL, "Worker finished" );



  ReleaseTestSyncSema( ctx->worker_wakeup );
  ReleaseTestSyncSema( ctx->runner_wakeup );

  rtems_event_receive( RTEMS_ALL_EVENTS, RTEMS_DEFAULT_OPTIONS, 0, &events );

}
