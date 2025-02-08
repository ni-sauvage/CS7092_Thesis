static rtems_status_code sendrc = 0;

static rtems_status_code recrc = 0;

static rtems_event_set pending[3];

static rtems_event_set recout[3];

static rtems_id test_sync_sema[2];



static void TestSegment0( Context* ctx ) {


  T_log(T_NORMAL,"@@@ 0 INIT");
  initialise_pending( pending, 3 );
  initialise_semaphore( 0, ctx->runner_wakeup, test_sync_sema );
  initialise_semaphore( 1, ctx->worker_wakeup, test_sync_sema );

}



static void TestSegment3( Context* ctx ) {
  
  size_t i;
  rtems_status_code sc;
  rtems_task_priority prio;
  T_thread_switch_log *log;

  T_log(T_NORMAL,"@@@ 3 TASK Worker");
  checkTaskIs( ctx->worker_id );

  T_log(T_NORMAL,"@@@ 3 CALL EqualPriority");
  SetSelfPriority( M_PRIO_NORMAL );
  sc = rtems_task_set_priority( RTEMS_SELF, RTEMS_CURRENT_PRIORITY, &prio );
  T_rsc_success( sc );
  T_eq_u32( prio, M_PRIO_NORMAL );

  T_log(T_NORMAL,"@@@ 3 WAIT 0");
  ObtainTestSyncSema( test_sync_sema[0] );

  T_log(T_NORMAL,"@@@ 3 CALL StartLog");
  log = T_thread_switch_record_4( &ctx->thread_switch_log );

  T_log(T_NORMAL,"@@@ 3 CALL event_send 1 3 10 sendrc");
  T_log( T_NORMAL, "Calling Send(%d,%d)", mapid( ctx, 3), 10 );
  sendrc = ( *ctx->send )( mapid( ctx, 3 ), 10 );
  T_log( T_NORMAL, "Returned 0x%x from Send", sendrc );

  T_log(T_NORMAL,"@@@ 3 CALL CheckNoPreemption");
  log = &ctx->thread_switch_log;
  T_le_sz( log->header.recorded, 1 );
  for ( i = 0; i < log->header.recorded; ++i ) {
    T_ne_u32( log->events[ i ].executing, ctx->worker_id );
    T_eq_u32( log->events[ i ].heir, ctx->runner_id );
  }

  T_log(T_NORMAL,"@@@ 3 SCALAR sendrc 4");
  T_rsc( sendrc, 4 );
  T_log(T_NORMAL,"@@@ 3 SIGNAL 1");
  ReleaseTestSyncSema( test_sync_sema[1] );

  T_log(T_NORMAL,"@@@ 3 STATE 1 Zombie");

}



static void TestSegment4( Context* ctx ) {

  rtems_status_code sc;

  rtems_task_priority prio;
  T_log(T_NORMAL,"@@@ 4 TASK Runner");
  checkTaskIs( ctx->runner_id );

  T_log(T_NORMAL,"@@@ 4 SIGNAL 0");
  ReleaseTestSyncSema( test_sync_sema[0] );

  T_log(T_NORMAL,"@@@ 4 WAIT 1");
  ObtainTestSyncSema( test_sync_sema[1] );

  T_log(T_NORMAL,"@@@ 4 SIGNAL 0");
  ReleaseTestSyncSema( test_sync_sema[0] );

  T_log(T_NORMAL,"@@@ 4 STATE 2 Zombie");

}





static void Runner( RtemsModelEventsMgr_Context *ctx )
{
  T_log( T_NORMAL, "Runner running" );
  TestSegment4( ctx );
  T_log( T_NORMAL, "Runner finished" );
}


static void Worker0( rtems_task_argument arg )
{
  Context *ctx;
  rtems_event_set events;

  ctx = (Context *) arg;

  T_log( T_NORMAL, "Worker Running" );
  TestSegment3( ctx );
  T_log( T_NORMAL, "Worker finished" );



  ReleaseTestSyncSema( ctx->worker_wakeup );
  ReleaseTestSyncSema( ctx->runner_wakeup );

  rtems_event_receive( RTEMS_ALL_EVENTS, RTEMS_DEFAULT_OPTIONS, 0, &events );

}
