/*
   C function for Ciao library time.

   Author: Remy Haemmerle
*/
   

#include <ciao_prolog.h>
#include <ciao/tasks.h>
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <signal.h>



#define INITIALIZED  0
#define SCHEDULED    1
#define DONE         2
#define STOPPED       3

struct alarm_data{
  int number, state, sent, time;
  pthread_t thread;
  pthread_mutex_t mutex;
  struct alarm_data * next;
};

struct alarm_data * ciao_alarms;

// int main_pid;
worker_t *main_worker;
int older_sent_signal = 0;
pthread_mutex_t * older_sent_signal_mutex = NULL;

int stop_alarm(struct alarm_data * data);

void garbage(struct alarm_data ** data){

  // printf("garbage(&%d) called\n", *data);   

  if ((*data) != NULL) {

    // printf("stop alarm %d\n", *data);   
    stop_alarm(*data);
    // printf("alarm %d stoped\n", *data);   

    garbage(&(*data)->next);

    pthread_mutex_destroy(&((*data)->mutex));
    pthread_join((*data)->thread, NULL);

    pthread_mutex_lock(older_sent_signal_mutex);
    if (older_sent_signal >= (*data)->number) {
      older_sent_signal = 0 ;
    }
    pthread_mutex_unlock(older_sent_signal_mutex);

    ciao_free((*data));
  } 
  *data = NULL; 
  //printf("garbage() returns\n"); 
}

void force_garbage(struct alarm_data * data){

  garbage(&data);

}

CVOID__PROTO(interrupt_worker, int signal_number); /* NOTE: do not use kill(), it will not work in daemons since we do not set SIGINT handler for nontty! (JF) */

void * ciao_alarm(void * ptr){
 
  struct alarm_data * data = (struct alarm_data *) ptr;
  
  // printf("ciao_alarm(%d) called\n", ptr); 
  
  pthread_mutex_lock(&(data->mutex));
  pthread_mutex_unlock(&(data->mutex));

  // printf("ciao_alarm(%d) (%d) alarm scheduled \n", ptr, data->number); 
  
  usleep(data->time * 1000);

  pthread_mutex_lock(&(data->mutex));
  if (data->state == SCHEDULED)
    { 
      pthread_mutex_lock(older_sent_signal_mutex);
      if ((older_sent_signal == 0) || (data->number < older_sent_signal))
          older_sent_signal = data->number;
      pthread_mutex_unlock(older_sent_signal_mutex);
      data->state = DONE;
      data->sent = ciao_true; 
      // printf("%d kill %d (%d-%d) \n", getpid(), main_pid, data, data->number);
      // kill(main_pid, SIGINT);
      interrupt_worker(main_worker, SIGINT);

      /*  printf("signal send : %d", data->number); */
    }  
  pthread_mutex_unlock(&(data->mutex));

  return (void *)NULL;
}

struct alarm_data * init_alarm(int time, struct alarm_data * last){
 
  struct alarm_data ** ptr;
  int number;

  // printf("init_alarm(%d, %d) called\n", time, last); 


  if (last == NULL) 
    {
      if ( older_sent_signal_mutex == NULL )  
        // first call: initialize library.
        {
          // printf("initialize library\n");

          // main_pid = getpid();
          main_worker = get_my_worker();

          older_sent_signal_mutex = ciao_malloc(sizeof(pthread_mutex_t));
          pthread_mutex_init(older_sent_signal_mutex, NULL);

          // printf("library initialized\n");
        }
      else 
        {
          //printf("garbage all alarms\n");
          
          garbage(&(ciao_alarms));

          // printf("all alarms garbaged\n");
        }

      older_sent_signal = 0; 
      number = 1;
      ptr = &ciao_alarms;
    }
  else
    { 
      // printf("garbage all alarms older that %d\n", last->next);
          
      garbage(&(last->next));
      
      number = last->number + 1;
      ptr = &(last->next);

      // printf("all alarms older that %d garbaged\n", last->next);
    }
  
  *ptr = 
    (struct alarm_data*) ciao_malloc(sizeof(struct alarm_data));

  last = *ptr;

  last->state = INITIALIZED;
  last->sent = ciao_false;
  last->number = number;
  last->time = time;  
  last->next = NULL; 
  pthread_mutex_init(&(last->mutex), NULL);

  pthread_mutex_lock(&(last->mutex));
  
  // printf(" last->number = %d, older_sent_signal = %d\n",  last->number,  older_sent_signal);
  
  if (pthread_create(&(last->thread), NULL, ciao_alarm, (void*) last)){ 
    perror("init_alarm:");
    return 0;
  }
  
  // printf("init_alarm returns %d\n", last); 

  return (void *) last;
}


int start_alarm(struct alarm_data * data){

  if (data->state == INITIALIZED)
    {
      data->state = SCHEDULED;
      pthread_mutex_unlock(&(data->mutex));
      return 1;
    }
  else return 0;
}

int stop_alarm(struct alarm_data * data){
  
  // printf("stop_alarm(%d) called\n", data);   

  switch (data->state) {
  case SCHEDULED:
    //  main thread does not hold lock
    pthread_mutex_lock(&(data->mutex));
  case INITIALIZED:  
    //  main thread does not hold lock
    if(pthread_cancel(data->thread)){
      perror("stop_alarm: ");
      return 2; 
    }
    data->state = STOPPED;
    pthread_mutex_unlock(&(data->mutex));
    return 0;
  default:
    // alarm already stopped or done
    return 1;
  }
}

int alarm_stat(struct alarm_data * data){

  int value;
  
  // printf(" alarm_stat(%d) called\n", data);
  // printf(" data->number = %d, older_sent_signal = %d\n",  data->number,  older_sent_signal);


  if (data->state == INITIALIZED)
    // main thread holds the lock
    value = data->state;
  else 
    // main thread does not holds the lock
    {
      pthread_mutex_lock(&(data->mutex));
      if (data->sent) value = (data->state | (1 << 2));
      else value = data->state;
      pthread_mutex_unlock(&(data->mutex));
    }

  pthread_mutex_lock(older_sent_signal_mutex);
  if (older_sent_signal == data->number)
  {
    value = (value | (1 << 3));         
    older_sent_signal = 0;
  }
  pthread_mutex_unlock(older_sent_signal_mutex);

  return value;
}






