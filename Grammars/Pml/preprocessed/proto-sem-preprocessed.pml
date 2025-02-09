mtype = {
  Wait, NoWait
, All, Any
}

mtype = {
  Zombie, Dormant, Ready, Blocked, TimeWait, OtherWait
}

inline nl() { printf("\n") }

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

mtype scenario;

bool stopclock = false;

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

   atomic{printf("Nothing DEFINEd at present\n");}
}

int g1,g2 ;

inline outputDeclarations () {
  printf("@@@ %d DECL int g1\n",_pid);
  printf("@@@ %d DECL int g2\n",_pid);
}

inline update1() {
  atomic{ g1 = g2+10; g2 = g1*2 }
}

inline update2() {
  atomic{ g2 = g1+5 ; g1 = g2*3 }
}

inline chooseScenario() {
  atomic{printf("No scenarios to be chosen right now\n")}
}

proctype Runner (byte nid, taskid) {

  printf("Runner running...\n") ;
  printf("@@@ %d TASK Runner\n",_pid);

  printf("Runner: before update1\n");
  printf("@@@ %d SCALAR g1 %d\n",_pid,g1);
  printf("@@@ %d SCALAR g2 %d\n",_pid,g2);

  printf("@@@ %d CALL update1\n",_pid);
  update1();

  printf("Runner: after update1\n");
  printf("@@@ %d SCALAR g1 %d\n",_pid,g1);
  printf("@@@ %d SCALAR g2 %d\n",_pid,g2);

  printf("@@@ %d LOG Sender %d finished\n",_pid,taskid);
  tasks[taskid].state = Zombie;
  printf("@@@ %d STATE %d Zombie\n",_pid,taskid)

}


proctype Worker (byte nid, taskid) {

  printf("Worker running...\n") ;
  printf("@@@ %d TASK Worker\n",_pid);

  printf("Worker: before update2\n");
  printf("@@@ %d SCALAR g1 %d\n",_pid,g1);
  printf("@@@ %d SCALAR g2 %d\n",_pid,g2);

  printf("@@@ %d CALL update2\n",_pid);
  update2();

  printf("Worker: after update2\n");
  printf("@@@ %d SCALAR g1 %d\n",_pid,g1);
  printf("@@@ %d SCALAR g2 %d\n",_pid,g2);

  printf("@@@ %d LOG Sender %d finished\n",_pid,taskid);
  tasks[taskid].state = Zombie;
  printf("@@@ %d STATE %d Zombie\n",_pid,taskid)
}

init {
  pid nr;

  printf("Prototype Semantics Model running.\n");
  printf("Setup...\n");

  printf("@@@ %d NAME Prototype_Semantics_TestGen\n",_pid)
  outputDefines();
  outputDeclarations();

  printf("@@@ %d INIT\n",_pid);
  chooseScenario();

  printf("Run...\n");

  run System();
  run Clock();

  run Runner(0,1);
  run Worker(0,2);

  _nr_pr == 1;

  printf("Prototype Semantics Model finished !\n")
}
