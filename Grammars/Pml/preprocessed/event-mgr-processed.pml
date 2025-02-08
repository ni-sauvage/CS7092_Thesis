mtype = {
  Wait, NoWait
, All, Any
}
mtype = {
  Zombie, Dormant, Ready, Blocked, TimeWait, OtherWait
}


typedef Task {
  byte nodeid;
  byte pmlid;
  mtype state = Zombie ;
  bool preemptable;
  byte prio;
  int ticks;
  bool tout;
  bool isr;

  byte start;
  bool HoldingMutex;
  bool mutexs[2];
};
Task tasks[3];
inline waitUntilReady(id){
  atomic{
    printf("@@@ %d LOG Task %d waiting, state = ",_pid,id);
    printm(tasks[id].state); nl()
  }
  tasks[id].state == Ready;
  printf("@@@ %d STATE %d Ready\n",_pid,id)
}

inline nl() { printf("\n") }

bool stopclock = false;

mtype scenario;
proctype System () {
  byte taskid ;
  bool liveSeen;

  printf("@@@ %d LOG System running...\n",_pid);

  loop:
  taskid = 1;
  liveSeen = false;

  printf("@@@ %d LOG Loop through tasks...\n",_pid);
  atomic {
    printf("@@@ %d LOG Scenario is ",_pid);
    printm(scenario); nl();
  }
  do
  :: taskid == 3 -> break;
  :: else ->
      atomic {
        printf("@@@ %d LOG Task %d state is ",_pid,taskid);
        printm(tasks[taskid].state); nl()
      }
      if
      :: tasks[taskid].state == Zombie -> taskid++
      :: else ->
         if
         :: tasks[taskid].state == OtherWait
             -> tasks[taskid].state = Ready
                printf("@@@ %d STATE %d Ready\n",_pid,taskid)
         :: else -> skip
         fi
         liveSeen = true;
         taskid++
      fi
  od

  printf("@@@ %d LOG ...all visited, live:%d\n",_pid,liveSeen);

  if
  :: liveSeen -> goto loop
  :: else
  fi
  printf("@@@ %d LOG All are Zombies, game over.\n",_pid);
  stopclock = true;
}
proctype Clock () {
  int tid, tix;
  printf("@@@ %d LOG Clock Started\n",_pid)
  do
  :: stopclock -> goto stopped
  :: !stopclock ->
      printf(" (tick) \n");
      tid = 1;
      do
      :: tid == 3 -> break
      :: else ->
          atomic{printf("Clock: tid=%d, state=",tid); printm(tasks[tid].state); nl()};
          if
          :: tasks[tid].state == TimeWait ->
              tix = tasks[tid].ticks - 1;

              if
              :: tix == 0
                  tasks[tid].tout = true
                  tasks[tid].state = Ready
                  printf("@@@ %d STATE %d Ready\n",_pid,tid)
              :: else
                  tasks[tid].ticks = tix
              fi
          :: else
          fi
          tid = tid + 1
      od
  od
stopped:
  printf("@@@ %d LOG Clock Stopped\n",_pid);
}
bool test_sync_sema[2];

inline TestSyncObtain(sem_id){
  atomic{
    printf("@@@ %d WAIT %d\n",_pid,sem_id);
    test_sync_sema[sem_id] ;
    test_sync_sema[sem_id] = false;
    printf("@@@ %d LOG WAIT %d Over\n",_pid,sem_id);
  }
}

inline TestSyncRelease(sem_id){
  atomic{
    printf("@@@ %d SIGNAL %d\n",_pid,sem_id);
    test_sync_sema[sem_id] = true ;
  }
}

inline TestSyncReleased(sem_id)
{
  test_sync_sema[sem_id] = true ;
}
inline outputDefines () {
   printf("@@@ %d DEF NO_OF_EVENTS %d\n",_pid,4);
   printf("@@@ %d DEF EVTS_NONE %d\n",_pid,0);
   printf("@@@ %d DEF EVTS_PENDING %d\n",_pid,0);
   printf("@@@ %d DEF EVT_0 %d\n",_pid,1);
   printf("@@@ %d DEF EVT_1 %d\n",_pid,2);
   printf("@@@ %d DEF EVT_2 %d\n",_pid,4);
   printf("@@@ %d DEF EVT_3 %d\n",_pid,8);
   printf("@@@ %d DEF EVTS_ALL %d\n",_pid,15);
   printf("@@@ %d DEF NO_TIMEOUT %d\n",_pid,0);
   printf("@@@ %d DEF TASK_MAX %d\n",_pid,3);
   printf("@@@ %d DEF BAD_ID %d\n",_pid,3);
   printf("@@@ %d DEF SEMA_MAX %d\n",_pid,2);
   printf("@@@ %d DEF RC_OK RTEMS_SUCCESSFUL\n",_pid);
   printf("@@@ %d DEF RC_InvId RTEMS_INVALID_ID\n",_pid);
   printf("@@@ %d DEF RC_InvAddr RTEMS_INVALID_ADDRESS\n",_pid);
   printf("@@@ %d DEF RC_Unsat RTEMS_UNSATISFIED\n",_pid);
   printf("@@@ %d DEF RC_Timeout RTEMS_TIMEOUT\n",_pid);
}

mtype{ EventWait } ;


typedef EventState {
  unsigned wanted : 4 ;
  unsigned pending : 4 ;
  bool all;
};

EventState evtstate[3];

byte sendrc;
byte recrc;
byte recout[3] ;
inline outputDeclarations () {
  printf("@@@ %d DECL byte sendrc 0\n",_pid);
  printf("@@@ %d DECL byte recrc 0\n",_pid);

  printf("@@@ %d DCLARRAY EvtSet pending TASK_MAX\n",_pid);
  printf("@@@ %d DCLARRAY byte recout TASK_MAX\n",_pid);
  printf("@@@ %d DCLARRAY Semaphore test_sync_sema SEMA_MAX\n",_pid);
}

inline printevents (evts) {
  printf("{%d,%d,%d,%d}",(evts)/8%2,(evts)/4%2,(evts)/2%2,(evts)%2);
}

inline events(evts,e3,e2,e1,e0) {
  evts = (8*e3+4*e2+2*e1+e0);
}

inline setminus(diff,minuend,subtrahend) {
  diff = (minuend) & (15-(subtrahend))
}
inline satisfied(estate,out,sat) {
  out = estate.pending & estate.wanted;
  if
  :: estate.all && out == estate.wanted -> sat = true
  :: !estate.all && out != 0 -> sat = true
  :: else -> sat = false
  fi
  printf("@@@ %d LOG satisfied(<pnd:%d wnt:%d all:%d>,out:%d,SAT:%d)\n",
          _pid,estate.pending,estate.wanted,estate.all,out,sat)
}
inline preemptIfRequired(sendid,rcvid) {
  if
  :: tasks[sendid].preemptable &&

      tasks[rcvid].prio < tasks[sendid].prio &&
      tasks[rcvid].state == Ready
      -> tasks[sendid].state = OtherWait;
          printf("@@@ %d STATE %d OtherWait\n",_pid,sendid)
  :: else
  fi
}
inline event_send(self,tid,evts,rc) {
  atomic{
    if
    :: tid >= 3 -> rc = 4
    :: tid < 3 ->
        evtstate[tid].pending = evtstate[tid].pending | evts

        unsigned got : 4;
        bool sat;;
        satisfied(evtstate[tid],got,sat);
        if
        :: sat ->
            tasks[tid].state = Ready;
            printf("@@@ %d STATE %d Ready\n",_pid,tid)
            preemptIfRequired(self,tid) ;





            waitUntilReady(self);
        :: else -> skip
        fi
        rc = 0;
    fi
  }
}

inline event_receive(self,evts,wait,wantall,interval,out,rc){
  atomic{
    printf("@@@ %d LOG pending[%d] = ",_pid,self);
    printevents(evtstate[self].pending); nl();
    evtstate[self].wanted = evts;
    evtstate[self].all = wantall
    if
    :: out == 0 ->
        printf("@@@ %d LOG Receive NULL out.\n",_pid);
        rc = 9 ;
    :: evts == 0 ->
        printf("@@@ %d LOG Receive Pending.\n",_pid);
        recout[out] = evtstate[self].pending;
        rc = 0
    :: else ->
        bool sat;;
        retry: satisfied(evtstate[self],recout[out],sat);
        if
        :: sat ->
            printf("@@@ %d LOG Receive Satisfied!\n",_pid);
            setminus(evtstate[self].pending,evtstate[self].pending,recout[out]);
            printf("@@@ %d LOG pending'[%d] = ",_pid,self);
            printevents(evtstate[self].pending); nl();
            rc = 0;
        :: !sat && !wait ->
            printf("@@@ %d LOG Receive Not Satisfied (no wait)\n",_pid);
            rc = 13;
        :: !sat && wait && interval > 0 ->
            printf("@@@ %d LOG Receive Not Satisfied (timeout %d)\n",_pid,interval);
            tasks[self].ticks = interval;
            tasks[self].tout = false;
            tasks[self].state = TimeWait;
            printf("@@@ %d STATE %d TimeWait %d\n",_pid,self,interval)
            waitUntilReady(self);
            if
            :: tasks[self].tout -> rc = 6
            :: else -> goto retry
            fi
        :: else ->
            printf("@@@ %d LOG Receive Not Satisfied (wait).\n",_pid);
            tasks[self].state = EventWait;
            printf("@@@ %d STATE %d EventWait\n",_pid,self)
            if
            :: sendTwice && !sentFirst -> TestSyncReleased(sendSema);
            :: else
            fi
            waitUntilReady(self);
            goto retry
        fi
    fi
    printf("@@@ %d LOG pending'[%d] = ",_pid,self);
    printevents(evtstate[self].pending); nl();
  }
}
typedef SendInputs {
  byte target_id ;
  unsigned send_evts : 4 ;
} ;
SendInputs send_in[3];
typedef ReceiveInputs {
  unsigned receive_evts : 4 ;
  bool will_wait;
  bool everything;
  byte wait_length;
};
mtype = {Send,Receive,SndRcv,RcvSnd,SndRcvSnd,SndPre, MultiCore};
ReceiveInputs receive_in[3];






bool doSend;
bool sendTwice;
bool sentFirst;
byte sendPrio;
bool sendPreempt;
byte sendTarget;
unsigned sendEvents : 4
unsigned sendEvents1 : 4
unsigned sendEvents2 : 4






bool doReceive;
unsigned rcvEvents : 4;
bool rcvWait;
bool rcvAll;
int rcvInterval;
int rcvOut;
byte rcvPrio;





int sendSema;
int rcvSema;
int startSema;




bool multicore;
int sendCore;
int rcvCore;


inline chooseScenario() {


  doSend = true;
  doReceive = true;
  sendTwice = false;
  sentFirst = false;
  test_sync_sema[0] = false;
  test_sync_sema[1] = false;
  sendPrio = 2;
  sendPreempt = false;
  sendTarget = 2;
  rcvPrio = 2;
  rcvWait = true;
  rcvAll = true;
  rcvInterval = 0;
  rcvOut = 2 ;
  tasks[1].state = Ready;
  tasks[2].state = Ready;

  sendEvents1 = 10;
  sendEvents2 = 0;
  sendEvents = sendEvents1;
  rcvEvents = 10;
  sendSema = 0;
  rcvSema = 1;
  startSema = sendSema;

  multicore = false;
  sendCore = 0;
  rcvCore = 0;

  if
  :: scenario = Send;
  :: scenario = Receive;
  :: scenario = SndRcv;
  :: scenario = SndPre;
  :: scenario = SndRcvSnd;
  :: scenario = MultiCore;
  fi
  atomic{printf("@@@ %d LOG scenario ",_pid); printm(scenario); nl()} ;


  if
  :: scenario == Send ->
        doReceive = false;
        sendTarget = 3;
  :: scenario == Receive ->
        doSend = false
        if
        :: rcvWait = false
        :: rcvWait = true; rcvInterval = 4
        :: rcvOut = 0;
        fi
        printf( "@@@ %d LOG sub-senario wait:%d interval:%d, out:%d\n",
                _pid, rcvWait, rcvInterval, rcvOut )
  :: scenario == SndRcv ->
        if
        :: sendEvents = 14;
        :: sendEvents = 11;
        :: rcvEvents = 0;
        :: rcvEvents = 15;
            rcvWait = false;
            rcvAll = false;
        fi
        printf( "@@@ %d LOG sub-senario send/receive events:%d/%d\n",
                _pid, sendEvents, rcvEvents )
  :: scenario == SndPre ->
        sendPrio = 3;
        sendPreempt = true;
        startSema = rcvSema;
        printf( "@@@ %d LOG sub-senario send-preemptable events:%d\n",
                _pid, sendEvents )
  :: scenario == SndRcvSnd ->
        sendEvents1 = 2;
        sendEvents2 = 8;
        sendEvents = sendEvents1;
        sendTwice = true;
        printf( "@@@ %d LOG sub-senario send-receive-send events:%d\n",
                _pid, sendEvents )
  :: scenario == MultiCore ->
        multicore = true;
        sendCore = 1;
        printf( "@@@ %d LOG sub-senario multicore send-receive events:%d\n",
                _pid, sendEvents )
  :: else
  fi

}


proctype Sender (byte nid, taskid) {

  byte sc;
  printf( "@@@ %d DECL byte sc\n", _pid );
  byte prio ;
  printf( "@@@ %d DECL byte prio\n", _pid );

  tasks[taskid].nodeid = nid;
  tasks[taskid].pmlid = _pid;
  tasks[taskid].prio = sendPrio;
  tasks[taskid].preemptable = sendPreempt;
  tasks[taskid].state = Ready;
  printf("@@@ %d TASK Worker\n",_pid);


  if
  :: multicore ->

       printf("@@@ %d CALL SetProcessor %d\n", _pid, sendCore);
  :: else
  fi

  if
  :: sendPrio > rcvPrio -> printf("@@@ %d CALL LowerPriority\n", _pid);
  :: sendPrio == rcvPrio -> printf("@@@ %d CALL EqualPriority\n", _pid);
  :: sendPrio < rcvPrio -> printf("@@@ %d CALL HigherPriority\n", _pid);
  :: else
  fi

repeat:

  TestSyncObtain(sendSema);

  if
  :: doSend ->
    if
    :: !sentFirst -> printf("@@@ %d CALL StartLog\n",_pid);
    :: else
    fi
    printf("@@@ %d CALL event_send %d %d %d sendrc\n",_pid,taskid,sendTarget,sendEvents);

    if
    :: sendPreempt && !sentFirst -> printf("@@@ %d CALL CheckPreemption\n",_pid);
    :: !sendPreempt && !sentFirst -> printf("@@@ %d CALL CheckNoPreemption\n",_pid);
    :: else
    fi


    event_send(taskid,sendTarget,sendEvents,sendrc);

    printf("@@@ %d SCALAR sendrc %d\n",_pid,sendrc);
  :: else
  fi

  TestSyncRelease(rcvSema);

  if
  :: sendTwice && !sentFirst ->
     sentFirst = true;
     sendEvents = sendEvents2;
     goto repeat;
  :: else
  fi

  printf("@@@ %d LOG Sender %d finished\n",_pid,taskid);
  tasks[taskid].state = Zombie;
  printf("@@@ %d STATE %d Zombie\n",_pid,taskid)
}


proctype Receiver (byte nid, taskid) {

  byte sc;
  printf( "@@@ %d DECL byte sc\n", _pid );
  byte prio ;
  printf( "@@@ %d DECL byte prio\n", _pid );

  tasks[taskid].nodeid = nid;
  tasks[taskid].pmlid = _pid;
  tasks[taskid].prio = rcvPrio;
  tasks[taskid].preemptable = false;
  tasks[taskid].state = Ready;
  printf("@@@ %d TASK Runner\n",_pid,taskid);

  if
  :: multicore ->

       printf("@@@ %d CALL SetProcessor %d\n", _pid, rcvCore);
  :: else
  fi

  TestSyncRelease(startSema);


  TestSyncObtain(rcvSema);






  if
  :: rcvPrio < sendPrio -> TestSyncRelease(sendSema);
  :: else
  fi

  if
  :: doReceive ->
    printf("@@@ %d SCALAR pending %d %d\n",_pid,taskid,evtstate[taskid].pending);

    if
    :: sendTwice && !sentFirst -> TestSyncRelease(sendSema)
    :: else
    fi

    printf("@@@ %d CALL event_receive %d %d %d %d %d recrc\n",
           _pid,rcvEvents,rcvWait,rcvAll,rcvInterval,rcvOut);


    event_receive(taskid,rcvEvents,rcvWait,rcvAll,rcvInterval,rcvOut,recrc);

    printf("@@@ %d SCALAR recrc %d\n",_pid,recrc);
    if
    :: rcvOut > 0 ->
      printf("@@@ %d SCALAR recout %d %d\n",_pid,rcvOut,recout[rcvOut]);
    :: else
    fi
    printf("@@@ %d SCALAR pending %d %d\n",_pid,taskid,evtstate[taskid].pending);
  :: else
  fi
  TestSyncRelease(sendSema);

  printf("@@@ %d LOG Receiver %d finished\n",_pid,taskid);
  tasks[taskid].state = Zombie;
  printf("@@@ %d STATE %d Zombie\n",_pid,taskid)
}

init {
  pid nr;

  printf("Event Manager Model running.\n");
  printf("Setup...\n");

  printf("@@@ %d NAME Event_Manager_TestGen\n",_pid)
  outputDefines();
  outputDeclarations();

  printf("@@@ %d INIT\n",_pid);
  chooseScenario();



  printf("Run...\n");

  run System();
  run Clock();

  run Sender(0,1);
  run Receiver(0,2);

  _nr_pr == 1;





  printf("Event Manager Model finished !\n")
}