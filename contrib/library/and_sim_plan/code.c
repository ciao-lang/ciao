//ERROR 1

#include "code.h"

struct levent *goals;
struct lprec precs;
unsigned int goalsSize;
unsigned int nthreads;
long int *lthreads;
struct lwam *lwams;
struct lready *lready;

CBOOL__PROTO(print_info_c)
{
  int i,j,k;

  for (i = 0; i < goalsSize; i++)
    {
      printf("\nID %d: ",i);
      for (j = 0; j < goals[i].size; j++)
	{
	  if (j != 0) printf(", ");
	  switch(goals[i].levent[j].type)
	    {
	    case FAIL:
	      printf("FAIL");
	      break;
	    case ANSWER:
	      printf("ANSWER");
	      break;
	    case TIME:
	      printf("TIME(%d)",goals[i].levent[j].values[0]);	    
	      break;
	    case START:
	      printf("START(");
	      for (k = 0; k < goals[i].levent[j].size; k++)
		{
		  if (k != 0) printf(",");
		  printf("%d",goals[i].levent[j].values[k]-1);
		}
	      printf(")");
	      break;
	    case RESTART:
	      printf("RESTART(");
	      for (k = 0; k < goals[i].levent[j].size; k++)
		{
		  if (k != 0) printf(",");
		  printf("%d",goals[i].levent[j].values[k]-1);
		}
	      printf(")");
	      break;
	    default:
	      printf("\nPRINTING ERROR: evento desconocido\n");
	      exit(1);
	    }
	}
      printf(".");
    }

//  printf("\nINCOMPATIBILIDADES:"); fflush(stdout);
//  for (i = 0; i < goalsSize; i++)
//    {
//      printf("\nGoal %d - father %d incompatible con:", i+1,precs.lprec[i].father);
//      for (j = 0; j < precs.lprec[i].size; j++)
//	printf(" %d",precs.lprec[i].values[j]);
//    }
//  printf("\n");
  return TRUE;
}

CBOOL__PROTO(free_info_c)
{
  int i,j;

  for (i = 0; i < goalsSize; i++)
    {
      for (j = 0; j < goals[i].size; j++)
	  if (goals[i].levent[j].values != NULL) free(goals[i].levent[j].values);

      free(goals[i].levent);
    }
  free(goals);
  goals = NULL;
  goalsSize = 0;
  return TRUE;
}

CBOOL__PROTO(sim_seq_c)
{

  printf("\nTiempo de secuencial %d\n",sim_seq_aux_c(1));
  return TRUE;

}

int sim_seq_aux_c(int idaux)
{
  int i,j,k;
  int total = 0;
  int id = idaux - 1;
  int tmp_total;
  int num_ans;

  for (j = 0; j < goals[id].size; j++)
    {
      switch(goals[id].levent[j].type)
	{
	case TIME:
	  total += goals[id].levent[j].values[0];
	  break;
	case START:
	  tmp_total = 0;
	  for (k = goals[id].levent[j].size - 1; k >= 0 ; k--)
	    {
	      num_ans = 0;
	      for (i = 0; i < goals[(goals[id].levent[j].values[k])-1].size; i++)
		if (goals[(goals[id].levent[j].values[k])-1].levent[i].type == ANSWER) 
		  num_ans++;

	      tmp_total = sim_seq_aux_c(goals[id].levent[j].values[k]) + num_ans * tmp_total;
	    }
	  
	  total += tmp_total;
	  break;
	default:
	  break;
	}
    }

  return total;
}

//FALTA REGISTRAR EL FALLO!!!!
CBOOL__PROTO(sim_seq_memo_c)
{
  int i,j,k,l;
  unsigned int total = 0;

  int *out = (int*) malloc (goalsSize * (sizeof(int)));
  for (i = 0; i < goalsSize; i++)
    out[i] = 0;
			    
  for (i = 0; i < goalsSize; i++)
    {
      // comprobar si alguien lo esta lanzando!
      if (i != 0) 
	{
	  for (k = 0; k < i; k++)
	    {
	      if (out[k]) continue;
	      for (j = 0; j < goals[k].size; j++)
		if (goals[k].levent[j].type == START)
		  for (l = 0; l < goals[k].levent[j].size; l++)
		    if (goals[k].levent[j].values[l] == i+1)
		      goto cont;
	    }

	    cont:
	  if (k == i)
	    {
	      out[i] = 1;
	      continue;
	    }
	}
      for (j = 0; j < goals[i].size; j++)
	{
	  switch(goals[i].levent[j].type)
	    {
	    case TIME:
	      total += goals[i].levent[j].values[0];	    
	      break;
	    default:
	      break;
	    }
	}
    }
  printf("\nTiempo de memorizacion secuencial %d\n",total);

  return TRUE;
}

//FALTA REGISTRAR EL FALLO!!!!
CBOOL__PROTO(sim_par_memo_c)
{

  printf("\nTiempo de memorizacion paralela %d\n",sim_par_memo_aux_c(1));
  return TRUE;
}

int sim_par_memo_aux_c(int idaux)
{
  int i,j,k;
  int total = 0;
  int id = idaux - 1;
  int max, aux;

  for (j = 0; j < goals[id].size; j++)
    {
      switch(goals[id].levent[j].type)
	{
	case TIME:
	  total += goals[id].levent[j].values[0];
	  break;
	case START:
	  max = 0;
	  for (k = 0 ; k < goals[id].levent[j].size; k++)
	    {
	      aux = sim_par_memo_aux_c(goals[id].levent[j].values[k]);
	      if (aux > max) max = aux;
	    }	  
	  total += max;
	  break;
	default:
	  break;
	}
    }

  return total;
}

CBOOL__PROTO(sim_par_c)
{
  int aux = 0;
  printf("\nTiempo de paralela AMADEO %d\n",sim_par_aux_c(1,0,&aux));

  return TRUE;
}

int sim_par_aux_c(int idaux, int prev_total, int *first_ans)
{
  int j,k;
  int total = 0;
  int id = idaux - 1;
  int new_first_ans = -1;
  int tmp_ans = 0;
  int time_par, ans_par;

  for (j = 0; j < goals[id].size; j++)
    {
      switch(goals[id].levent[j].type)
	{
	case ANSWER:
	  if (new_first_ans < 0) new_first_ans = total;
	  if (tmp_ans > *first_ans)
	    total = total + prev_total - *first_ans + tmp_ans;
	  else total += prev_total;
	  tmp_ans =0;
	case TIME:
	  total += goals[id].levent[j].values[0];
	  tmp_ans += goals[id].levent[j].values[0];
	  break;
	case START:
	  time_par = 0;
	  ans_par = 0;

	  for (k = goals[id].levent[j].size - 1 ; k >= 0; k--)
	      time_par = sim_par_memo_aux_c(goals[id].levent[j].values[k]);

	  total += time_par;
	  tmp_ans += time_par;
	  break;
	default:
	  break;
	}
    }

  if (new_first_ans > *first_ans) *first_ans = new_first_ans; 
  return total;
}

int pending_work(void)
{
  int i;

  for (i = 0; i < nthreads; i++) 
    if (lthreads[i] != -1) return TRUE;

  return FALSE;
}

void planificar_goal(unsigned int id)
{
  int j;

  //si esta en la lista de objetivos ya no hago nada
  int i;
  for (i = 0; i < lready->size; i++)
    if (lready->goals[i] == id) return;

  for (j = 0; j < nthreads; j++)
    if (lthreads[j] == -1) 
      {
	lthreads[j] = id;
	return;
      }
  //sino meter objetivos en la lista de objetivos (DONE)
  lready->size++;
  lready->goals = (unsigned int*) 
    realloc (lready->goals, lready->size * sizeof(unsigned int));
  lready->goals[lready->size-1] = id;
}

void get_goal(unsigned int iTh)
{
  if (lready->size)
    {
      ITH_W = lready->goals[0];
      int k;
      for (k = 1; k < lready->size; k++)
	lready->goals[k-1] = lready->goals[k];
      
      lready->size--;
      lready->goals = (unsigned int*) 
	realloc (lready->goals, lready->size * sizeof(unsigned int));
    }
  else ITH_W = -1;
}

void drop_goal(unsigned int id)
{
  int i,j;
  int ok = FALSE;
  for (i = 0; i < lready->size; i++)
    if (lready->goals[i] == id)
      {
	for (j = i + 1; j < lready->size; j++)
	  lready->goals[j-1] = lready->goals[j];
	ok = TRUE;
      }
  if (ok)
    {
      lready->size--;
      lready->goals = (unsigned int*) 
	realloc (lready->goals, lready->size * sizeof(unsigned int));
    }
}

void matar_goal(unsigned int id)
{
  int i;

  if (id == -1) return;

  //matar el id en question
  for (i = 0; i < nthreads; i++)
    if (lthreads[i] == id)
      {
	get_goal(i);
	break;
      }
  if (i == nthreads) drop_goal(id);

  //buscar a los hijos y matarlos
  for (i = 0; i < goalsSize; i++)
    if (WAM_DEP(i) == id) matar_goal(i);

  WAM_GIND(id) = 0;
}

void matarPARBACK_goal(unsigned int id)
{
  int i;

//  printf("\nLlamo a matar %d\n",id);
  if (id == -1) return;

  //matar el id en question
  for (i = 0; i < nthreads; i++)
    if (lthreads[i] == id)
      {
	get_goal(i);
	break;
      }
  if (i == nthreads) drop_goal(id);

  //buscar a los hijos y matarlos
  for (i = 0; i < goalsSize; i++)
    if (WAM_DEP(i) == id) matarPARBACK_goal(i);
}

CBOOL__PROTO(sim_par_back_c)
{
  int i;
  unsigned long int TTot = 0;
  unsigned long int TAcum = 0;
  unsigned long int NAns = 0;

  //inicializar los threads 
  for (i = 1; i < nthreads; i++) lthreads[i] = -1;
  //primer thread a ejecutar primer objetivo
  lthreads[0] = 0;

  lwams->Nwams = goalsSize;
  lwams->wams = (struct wam*) malloc(lwams->Nwams * sizeof(struct wam));
  for (i = 0; i < lwams->Nwams; i++)
    {
      lwams->wams[i].ind = 0;
      lwams->wams[i].wait = NULL;
      lwams->wams[i].waitS = 0;
      lwams->wams[i].time = 0;
      lwams->wams[i].ans = FALSE;
      lwams->wams[i].iAns = 0;
      lwams->wams[i].NAns = 0;
      lwams->wams[i].depen = -1;
    }

  while(pending_work())
    {
      int iWAM;
      int iTh;
//      printf("\n\n%d Threads: ",TTot);
//      fflush(stdout);
//      for (iTh = 0; iTh < nthreads; iTh++)
//	{
//	  //imprimir los threads
//	  printf("%d ",ITH_W); fflush(stdout);
//	}
//      printf("\nWAMS:");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < lwams->Nwams; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("\nWAM %d: ind = %d, time = %d, FIN = %d, iAns = %d, NAns = %d, waitS = %d",
//		 iWAM,lwams->wams[iWAM].ind,lwams->wams[iWAM].time,
//		 lwams->wams[iWAM].ans,lwams->wams[iWAM].iAns,
//               lwams->wams[iWAM].NAns,lwams->wams[iWAM].waitS);
//	  fflush(stdout);
//	  int k;
//	  for (k = 0; k < lwams->wams[iWAM].waitS; k++)
//	    {
//	      int kk;
//	      printf("\nWAIT %d: last %d, waits: ",k,lwams->wams[iWAM].wait[k].last);
//	      fflush(stdout);
//	      for (kk = 0; kk < lwams->wams[iWAM].wait[k].idsS; kk++)
//		{
//		  printf("%d ",lwams->wams[iWAM].wait[k].idsO[kk]);
//		  fflush(stdout);
//		}
//	    }
//	}
//      printf("\nstartW: ");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < goalsSize; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("%d ",WAM_DEP(iWAM));
//	  fflush(stdout);
//	}
//      printf("\nREADY: ");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < lready->size; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("%d ",lready->goals[iWAM]);
//	  fflush(stdout);
//	}

      int nextOP = TIME;
      for (iTh = 0; iTh < nthreads; iTh++)
	{
	  if (ITH_W == -1) continue;
	  if (Th_NextType(ITH_W) > nextOP) 
	    {
	      nextOP = Th_NextType(ITH_W);
	      break;
	    }
	}

      if (nextOP == TIME)
	{
	  int minTime = 1000;
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      if ((Th_NextValue(ITH_W,0) - WAM_TIME(ITH_W)) < minTime)
		minTime = Th_NextValue(ITH_W,0) - WAM_TIME(ITH_W);
	    }
	  TTot += minTime;
//	  printf("\nAvanzo time %d\n",minTime);
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      WAM_TIME(ITH_W) += minTime;
	      if (Th_NextValue(ITH_W,0) == WAM_TIME(ITH_W))
		{
		  WAM_GIND(ITH_W)++;
		  WAM_TIME(ITH_W) = 0;
		}
	    }
	}
      else
	{
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      switch(Th_NextType(ITH_W))
		{
		case START:
//		  printf("\nEjecuto START en %d\n",ITH_W); fflush(stdout);
		  //marcar a los que vamos a esperar de ANSWER (DONE)
		  WAM_WAITS(ITH_W)++;
		  WAM_WAIT(ITH_W) = (struct wait *) 
		    realloc (WAM_WAIT(ITH_W), WAM_WAITS(ITH_W) * sizeof(struct wait));
		  WAM_IWAITS(ITH_W) = Th_NextSize(ITH_W);
		  WAM_IWAITID_O(ITH_W) = (unsigned int*)
		    malloc(Th_NextSize(ITH_W)*sizeof(unsigned int));
		  //ver si hay threads libres y asignarles los objetivos (DONE)
		  for (i = 0; i < Th_NextSize(ITH_W) - 1; i++)
		    {
		      if (WAM_NAns(Th_NextValue(ITH_W,i)-1) != 0) 
			printf("\nProblemas en Ini NAns\n");
		      WAM_DEP(Th_NextValue(ITH_W,i)-1) = ITH_W;
		      WAM_IWAIT_O(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		      planificar_goal(Th_NextValue(ITH_W,i)-1);
		    }
		  //i eszta apuntando al ultimo, que lo coge este thread
		  WAM_DEP(Th_NextValue(ITH_W,i)-1) = ITH_W;
		  WAM_IWAIT_O(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		  int aux = Th_NextValue(ITH_W,i)-1;
		  WAM_GIND(ITH_W)++;
		  ITH_W = aux;
		  break;
		case RESTART:
//		  printf("\nEjecuto RESTART en %d\n",ITH_W);
		  //hemos leido todas las respuestas y se ha acabado
		  printf("");
		  int k, salir;
		  for (k = 0; k < WAM_IWAITS(ITH_W); k++)
		    {
		      salir = TRUE;
		      if (WAM_FAns(WAM_IWAIT_O(ITH_W,k)) && 
			  (WAM_iAns(WAM_IWAIT_O(ITH_W,k)) == WAM_NAns(WAM_IWAIT_O(ITH_W,k))))
			salir = FALSE;
		      if (salir) break;
		    }
		  if (k == WAM_IWAITS(ITH_W))
		    {
//		      printf("\nHEMOS LEIDO TODAS LAS RESPUESTAS\n");
		      WAM_GIND(ITH_W)++;
		      free(WAM_IWAITID_O(ITH_W));
		      WAM_WAITS(ITH_W)--;
		      WAM_WAIT(ITH_W) = (struct wait *) 
			realloc (WAM_WAIT(ITH_W), WAM_WAITS(ITH_W) * sizeof(struct wait));
		      goto break1;
		    }
		  //comprobar si podemos leer directamente por memoriazacion
		  for (k = WAM_IWAITS(ITH_W) - 1; k >= 0; k--)
		    {
		      //comprobar si quedan por leer
		      if (WAM_iAns(WAM_IWAIT_O(ITH_W,k)) != WAM_NAns(WAM_IWAIT_O(ITH_W,k)))
			{
//			  printf("\nEstoy checkeando a %d\n",WAM_IWAIT_O(ITH_W,k));
			  WAM_iAns(WAM_IWAIT_O(ITH_W,k))++;
			  int kk;
			  for (kk = k + 1; kk < WAM_IWAITS(ITH_W); kk++)
			    WAM_iAns(WAM_IWAIT_O(ITH_W,kk)) = 0;
			  WAM_GIND(ITH_W)++;
			  goto break1;
			}
		      //Si no ha terminado, relanzamos todos los que no hayan terminado
		      if (!WAM_FAns(WAM_IWAIT_O(ITH_W,k)))
			{
//			  printf("\n%d no ha terminado\n",WAM_IWAIT_O(ITH_W,k));
			  for (i = 0; i < k; i++)
			    if (!WAM_FAns(WAM_IWAIT_O(ITH_W,i))) 
			      planificar_goal(WAM_IWAIT_O(ITH_W,i));
			  int aux = WAM_IWAIT_O(ITH_W,k);
			  WAM_GIND(ITH_W)++;
//			  printf("\nRelaqnzo %d\n",aux);
			  ITH_W = aux;
			  goto break1;
			}
		      //Sino seguimos con el siguiente (no apuntamos a cero aqui)
		    }
		  break1:
		    break;
		case ANSWER:
//		  printf("\nEjecuto ANSWER en %d con FIN %d\n",ITH_W,WAM_FAns(ITH_W));
		  //desmarcar de su STARTM
		  WAM_NAns(ITH_W)++;	
		  //respuesta del padre principal
		  if (WAM_DEP(ITH_W) == -1)
		    {
		      TAcum += TTot;
		      NAns++;
		      WAM_GIND(ITH_W)++;
		      goto break2;
		    }
		      
		  //comprobar si es la primer que hace a todos completos
		  int iSTARTM = 0;
		  if (WAM_NAns(ITH_W) == 1)
		    for (iSTARTM = 0; iSTARTM < WAM_IWAITS(WAM_DEP(ITH_W)); iSTARTM++)
		      if (!WAM_NAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iSTARTM))) break;
		  
		  //es la primera respuesta general??
		  if (iSTARTM == WAM_IWAITS(WAM_DEP(ITH_W)))
		    {
//		      printf("\nPrimera general\n");
		      int iWAM = ITH_W;
		      WAM_GIND(ITH_W)++;
		      int k;
		      for (k = 0; k < WAM_IWAITS(WAM_DEP(iWAM)); k++)
			{
			  WAM_iAns(WAM_IWAIT_O(WAM_DEP(iWAM),k)) = 1;
			  if (WAM_IWAIT_O(WAM_DEP(iWAM),k) != iWAM)
			    matarPARBACK_goal(WAM_IWAIT_O(WAM_DEP(iWAM),k));
			}
		      //seguir con el padre
		      ITH_W = WAM_DEP(iWAM);
		      goto break2;
		    }

		  //Todos los de mi derecha han terminado
		  for (iSTARTM = WAM_IWAITS(WAM_DEP(ITH_W)) - 1;
		       WAM_IWAIT_O(WAM_DEP(ITH_W),iSTARTM) != ITH_W; 
		       iSTARTM--)
		    if (!WAM_FAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iSTARTM))) break;

		  //Han terminado todos a mi derecha??
		  if (WAM_IWAIT_O(WAM_DEP(ITH_W),iSTARTM) == ITH_W)
		    {
//		      printf("\nTodos han terminado\n");
		      //Si todos tienen una respuesta??
		      for (iSTARTM--; iSTARTM >= 0; iSTARTM--)
			if (!WAM_NAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iSTARTM))) break;
		      if (iSTARTM < 0)
			{			  
//			  printf("\nIncremento indice %d\n",WAM_iAns(ITH_W));
			  WAM_iAns(ITH_W)++;	
//			  printf("\nNuevo indice %d\n",WAM_iAns(ITH_W));
			  int iWAM = ITH_W;
			  WAM_GIND(ITH_W)++;
			  int k;
			  for (k = 0; k < WAM_IWAITS(WAM_DEP(iWAM)); k++)
			    if (iWAM != WAM_IWAIT_O(WAM_DEP(iWAM),k))
			      matarPARBACK_goal(WAM_IWAIT_O(WAM_DEP(iWAM),k));
			  //seguir con el padre
			  ITH_W = WAM_DEP(iWAM);
			  goto break2;
			}
		      //else replanifico
//		      printf("\nReplanifico %d\n",iSTARTM);
		      WAM_GIND(ITH_W)++;
		      goto break2;
		    }
		  //saco el objetivo del procesador y replanifico
		  WAM_GIND(ITH_W)++;
		break2:
//		  printf("\nSalgo ANSWER en %d con FIN %d y iAns %d\n",
//			 ITH_W,WAM_FAns(ITH_W),WAM_iAns(ITH_W));
		  break;
		case FAIL:
//		  printf("\nEjecuto FAIL en %d\n",ITH_W);
		  //Ver si es fallo en el padre
		  WAM_FAns(ITH_W) = TRUE;
		  if (ITH_W == 0) 
		    {
//		      printf("\nEjecuto Fail del PADRE\n");
		      ITH_W = -1;
		      int k;
		      for (k = 0; k < nthreads; k++)
			if (lthreads[k] != -1) printf("\nFallo en Fail del PADRE\n");
		      goto break3;
		    }

		  //Si falla uno sin respuestas
		  if (WAM_NAns(ITH_W) == 0)
		    {
//		      printf("\n Esta cortando busqueda\n");
		      int new = WAM_DEP(ITH_W);
		      int iWAM = ITH_W;
		      int k;
		      for (k = 0; k < WAM_IWAITS(WAM_DEP(iWAM)); k++)	
			if (iWAM != WAM_IWAIT_O(WAM_DEP(iWAM),k))
			  matarPARBACK_goal(WAM_IWAIT_O(WAM_DEP(iWAM),k));

		      free(WAM_IWAITID_O(new));
		      WAM_WAITS(new)--;
		      WAM_WAIT(new) = (struct wait *) 
			realloc (WAM_WAIT(new), WAM_WAITS(new) * sizeof(struct wait));
		      ITH_W = new;
		      goto break3;
		    }
		  
		  //Si falla uno con todo a la derecha FINALIZADO
		  int iWait;
		  for (iWait = WAM_IWAITS(WAM_DEP(ITH_W)) - 1;
		       WAM_IWAIT_O(WAM_DEP(ITH_W),iWait) != ITH_W; 
		       iWait--)
		    if (!WAM_FAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iWait))) break;

		  //Han terminado todos a mi derecha??
		  if (WAM_IWAIT_O(WAM_DEP(ITH_W),iWait) == ITH_W)
		    {
		      for (iWait--; iWait >= 0; iWait--)
			{
			  if (WAM_NAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iWait)) !=
			      WAM_iAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iWait)))
			    {
			      WAM_iAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iWait))++;
			      int k;
			      for (k = iWait+1; k < WAM_IWAITS(WAM_DEP(ITH_W)); k++)
				WAM_iAns(WAM_IWAIT_O(WAM_DEP(ITH_W),k)) = 0;
			      ITH_W = WAM_DEP(ITH_W);
			      goto break3;
			    }
			  if (!WAM_FAns(WAM_IWAIT_O(WAM_DEP(ITH_W),iWait)))
			    {
			      get_goal(iTh);
			      goto break3;
			    }
			}
//		      free(WAM_IWAITID_O(ITH_W));
//		      WAM_WAITS(ITH_W)--;
//		      WAM_WAIT(ITH_W) = (struct wait *) 
//			realloc (WAM_WAIT(ITH_W), WAM_WAITS(ITH_W) * sizeof(struct wait));
		      ITH_W = WAM_DEP(ITH_W);
		      goto break3;
		    }

		  //No han terminado todos los de mi derecha -> replanificar
		  get_goal(iTh);

		break3:
		  break;
		default:
		  break;
		}
	    }
	}
    }

  printf("\nTiempo ideal PAR BACK %d\n", TTot);
  return TRUE;
}

CBOOL__PROTO(sim_pred_s_amadeo_c)
{
  int i;
  unsigned long int TTot = 0;
  unsigned long int TAcum = 0;
  unsigned long int NAns = 0;
  
  //inicializar los threads 
  for (i = 1; i < nthreads; i++) lthreads[i] = -1;
  //primer thread a ejecutar primer objetivo
  lthreads[0] = 0;

  lwams->Nwams = goalsSize;
  lwams->wams = (struct wam*) malloc(lwams->Nwams * sizeof(struct wam));
  for (i = 0; i < lwams->Nwams; i++)
    {
      lwams->wams[i].ind = 0;
      lwams->wams[i].wait = NULL;
      lwams->wams[i].waitS = 0;
      lwams->wams[i].time = 0;
      lwams->wams[i].ans = FALSE;
      lwams->wams[i].depen = -1;      
    }


  while(pending_work())
    {
      int iWAM;
      int iTh;
//      printf("\n\n%d Threads: ",TTot);
//      fflush(stdout);
//      for (iTh = 0; iTh < nthreads; iTh++)
//	{
//	  //imprimir los threads
//	  printf("%d ",ITH_W); fflush(stdout);
//	}
//      printf("\nWAMS:");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < lwams->Nwams; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("\nWAM %d: ind = %d, time = %d, ans = %d, waitS = %d",
//		 iWAM,lwams->wams[iWAM].ind,lwams->wams[iWAM].time,
//		 lwams->wams[iWAM].ans,lwams->wams[iWAM].waitS);
//	  fflush(stdout);
//	  int k;
//	  for (k = 0; k < lwams->wams[iWAM].waitS; k++)
//	    {
//	      int kk;
//	      printf("\nWAIT %d: last %d, waits: ",k,lwams->wams[iWAM].wait[k].last);
//	      fflush(stdout);
//	      for (kk = 0; kk < lwams->wams[iWAM].wait[k].idsS; kk++)
//		{
//		  printf("%d ",lwams->wams[iWAM].wait[k].ids[kk]);
//		  fflush(stdout);
//		}
//	    }
//	}
//      printf("\nstartW: ");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < goalsSize; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("%d ",lwams->wams[iWAM].depen);
//	  fflush(stdout);
//	}
//      printf("\nREADY: ");
//      fflush(stdout);
//      for (iWAM = 0; iWAM < lready->size; iWAM++)
//	{
//	  //imprimir las WAMS
//	  printf("%d ",lready->goals[iWAM]);
//	  fflush(stdout);
//	}

      int nextOP = TIME;
      for (iTh = 0; iTh < nthreads; iTh++)
	{
	  if (ITH_W == -1) continue;
	  if (Th_NextType(ITH_W) > nextOP) 
	    {
	      nextOP = Th_NextType(ITH_W);
	      break;
	    }
	}

      if (nextOP == TIME)
	{
	  int minTime = 1000;
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      if ((Th_NextValue(ITH_W,0) - WAM_TIME(ITH_W)) < minTime)
		minTime = Th_NextValue(ITH_W,0) - WAM_TIME(ITH_W);
	    }
	  TTot += minTime;
//	  printf("\nAvanzo time %d\n",minTime);
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      WAM_TIME(ITH_W) += minTime;
	      if (Th_NextValue(ITH_W,0) == WAM_TIME(ITH_W))
		{
		  WAM_GIND(ITH_W)++;
		  WAM_TIME(ITH_W) = 0;
		}
	    }
	}
      else
	{
	  for (iTh = 0; iTh < nthreads; iTh++)
	    {
	      if (ITH_W == -1) continue;
	      switch(Th_NextType(ITH_W))
		{
		case START:
//		  printf("\nEjecuto START en %d\n",ITH_W);
		  //marcar a los que vamos a esperar de ANSWER (DONE)
		  WAM_WAITS(ITH_W)++;
		  WAM_WAIT(ITH_W) = (struct wait *) 
		    realloc (WAM_WAIT(ITH_W), WAM_WAITS(ITH_W) * sizeof(struct wait));
		  WAM_Last(ITH_W) = Th_NextValue(ITH_W,Th_NextSize(ITH_W)-1)-1;
		  WAM_IWAITS(ITH_W) = Th_NextSize(ITH_W);
		  WAM_IWAITID(ITH_W) = (unsigned int*)
		    malloc(Th_NextSize(ITH_W)*sizeof(unsigned int));
		  WAM_IWAITID_O(ITH_W) = (unsigned int*)
		    malloc(Th_NextSize(ITH_W)*sizeof(unsigned int));
		  //ver si hay threads libres y asignarles los objetivos (DONE)
		  for (i = 0; i < Th_NextSize(ITH_W) - 1; i++)
		    {
		      WAM_DEP(Th_NextValue(ITH_W,i)-1) = ITH_W;
		      WAM_IWAIT(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		      WAM_IWAIT_O(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		      planificar_goal(Th_NextValue(ITH_W,i)-1);
		    }
		  //i eszta apuntando al ultimo, que lo coge este thread
		  WAM_DEP(Th_NextValue(ITH_W,i)-1) = ITH_W;
		  WAM_IWAIT(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		  WAM_IWAIT_O(ITH_W,i) = Th_NextValue(ITH_W,i)-1;
		  int aux = Th_NextValue(ITH_W,i)-1;
		  WAM_GIND(ITH_W)++;
		  ITH_W = aux;
		  break;
		case RESTART:
//		  printf("\nEjecuto RESTART en %d\n",ITH_W); fflush(stdout);
		  //seguir con el local y marcar la espera
		  WAM_IWAIT(ITH_W,WAM_IWAITS(ITH_W)-1) = WAM_IWAIT_O(ITH_W,WAM_IWAITS(ITH_W)-1);
		  WAM_GIND(ITH_W)++;
		  ITH_W = WAM_Last(ITH_W);
		  break;
		case ANSWER:
//		  printf("\nEjecuto ANSWER en %d\n",ITH_W);
		  //desmarcar de su STARTM
		  WAM_Ans(ITH_W) = TRUE;		  
		  if (WAM_DEP(ITH_W) == -1)
		    {
		      TAcum += TTot;
		      NAns++;
		      WAM_GIND(ITH_W)++;
		      break;
		    }
		      
		  int iSTARTM;
		  for (iSTARTM = 0; iSTARTM < WAM_IWAITS(WAM_DEP(ITH_W)); iSTARTM++)
		    {
		      if (WAM_IWAIT(WAM_DEP(ITH_W),iSTARTM) == ITH_W)
			{
			  WAM_IWAIT(WAM_DEP(ITH_W),iSTARTM) = -1;
			  break;
			}
		    }
		  if (iSTARTM == WAM_IWAITS(WAM_DEP(ITH_W))) printf("\nFallo en ANSWER\n");
		  
		  int fin = TRUE;
		  for (iSTARTM = 0; iSTARTM < WAM_IWAITS(WAM_DEP(ITH_W)); iSTARTM++)
		    {
		      if (WAM_IWAIT(WAM_DEP(ITH_W),iSTARTM) != -1)
			{
			  fin = FALSE;
			  break;
			}
		    }
		  
		  WAM_GIND(ITH_W)++;
		  if (fin) ITH_W = WAM_DEP(ITH_W);		  
		  else get_goal(iTh);
		  break;
		case FAIL:
//		  printf("\nEjecuto FAIL en %d\n",ITH_W);
		  //Si no ha generado respuestas, fallan todos los paralelos
		  if (ITH_W == 0) 
		    {
//		      printf("\nEjecuto Fail del PADRE\n");
		      ITH_W = -1;
		      int k;
		      for (k = 0; k < nthreads; k++)
			if (lthreads[k] != -1) printf("\nFallo en Fail del PADRE\n");
		      break;
		    }
		  //Si falla uno sin respuestas o el de más a la derecha
		  if ((!WAM_Ans(ITH_W)) || (WAM_IWAIT(WAM_DEP(ITH_W),0) == ITH_W))
		    {
//		      printf("\nBorrar todo\n");
		      int new = WAM_DEP(ITH_W);
		      int iWAM = ITH_W;
		      int k;
		      for (k = 0; k < WAM_IWAITS(WAM_DEP(iWAM)); k++)		    
			matar_goal(WAM_IWAIT_O(WAM_DEP(iWAM),k));

		      free(WAM_IWAITID(new));
		      free(WAM_IWAITID_O(new));
		      WAM_WAITS(new)--;
		      WAM_WAIT(new) = (struct wait *) 
			realloc (WAM_WAIT(new), WAM_WAITS(new) * sizeof(struct wait));
		      ITH_W = new;
		    }
		  else
		    {
		      //planificar el de uno más a la izda
		      int k;
		      for (k = 0; k < WAM_IWAITS(WAM_DEP(ITH_W)); k++)
			if (WAM_IWAIT(WAM_DEP(ITH_W),k) == ITH_W) break;
		      if (k == WAM_IWAITS(WAM_DEP(ITH_W))) printf("\nFallo en FAIL 1\n");

		      k--;
		      WAM_IWAIT(WAM_DEP(ITH_W),k) = WAM_IWAIT_O(WAM_DEP(ITH_W),k);
		      fflush(stdout);
		      planificar_goal(WAM_IWAIT(WAM_DEP(ITH_W),k));
		      //reiniciar el thread actual
		      WAM_IWAIT(WAM_DEP(ITH_W),k+1) = WAM_IWAIT_O(WAM_DEP(ITH_W),k+1);
		      WAM_GIND(ITH_W) = 0;

		      //liberar los threads de la derecha si estan en ejecucion
		      int aux = k;
		      for (k = aux+2; k < WAM_IWAITS(WAM_DEP(ITH_W)); k++)			
			  matar_goal(WAM_IWAIT_O(WAM_DEP(ITH_W),k));

		      //planificar convenientemente los nuevos objetivos
		      for (k = aux+2; k < WAM_IWAITS(WAM_DEP(ITH_W)); k++)
			planificar_goal(WAM_IWAIT(WAM_DEP(ITH_W),k));
		    }
		  break;
		default:
		  break;
		}
	    }
	}
    }

  printf("\nTiempo ideal AMADEO %d\n", TTot);
  return TRUE;
}



void get_incompatibles(int id,  struct prec *precs)
{
  int i, l;

  precs->size = precs->size + 1;
  precs->values = (unsigned int*) realloc (precs->values, precs->size*sizeof(unsigned int));
  precs->values[precs->size-1] = id;

  for (i = 0; i < goals[id-1].size; i++)
    if (goals[id-1].levent[i].type == START)
      for (l = 0; l < goals[id-1].levent[i].size; l++)
	get_incompatibles(goals[id-1].levent[i].values[l],  precs);
}

CBOOL__PROTO(precedences_info_c)
{
  int i, id, j, k, l;

  //Quitamos los RESTART sobrantes
  int cambio = TRUE;
  while(cambio)
    {
      cambio = FALSE;
      for (i = 0; i < goalsSize; i++)
	for (j = goals[i].size - 1; j >= 0; j--) 
	  if (goals[i].levent[j].type == RESTART)
	    {
	      for (k = 0; k < goals[i].levent[j].size; k++)
		{
		  int id = goals[i].levent[j].values[k] - 1;
		  if ((goals[id].levent[goals[id].size-1].type != FAIL) ||
		      (goals[id].levent[goals[id].size-2].type != ANSWER)) break;
		}
	      if (k == goals[i].levent[j].size)
		{
		  for (k = 0; k < goals[i].levent[j].size; k++)
		    {
		      int id = goals[i].levent[j].values[k] - 1;
		      goals[id].levent[goals[id].size-1].type = FAIL_FAKE;
		    }
		  goals[i].size--;
		  free(goals[i].levent[j].values);
		  for (k = j + 1; k <= goals[i].size; k++)
		    goals[i].levent[k-1] = goals[i].levent[k];
		  goals[i].levent = (struct event*) 
		    realloc (goals[i].levent, goals[i].size * sizeof(struct event));
		  if ((goals[i].levent[j].type == FAIL) && 
		      (goals[i].levent[j-1].type == ANSWER))
		    cambio = TRUE;
		}
	    }
    }

  //quitamos FAIL_FAKEs
  for (i = 0; i < goalsSize; i++)
    if (goals[i].levent[goals[i].size-1].type == FAIL_FAKE)
      goals[i].levent[goals[i].size-1].type = FAIL;

  //Inicializar el array de threads
  DEREF(X(0),X(0));
  nthreads = ENGINE_IntOfTerm(X(0)); 
  lthreads = (long int*) malloc (nthreads * sizeof(long int));

//  //Construir la lista de precedencias
//  precs.lprec = (struct prec*) malloc (goalsSize * sizeof(struct prec));
//  precs.lprec[0].values = NULL;
//  precs.lprec[0].size = 0;
//  precs.lprec[0].father = 1;
//  for (i = 1; i < goalsSize; i++)
//    {
//      precs.lprec[i].values = NULL;
//      precs.lprec[i].size = 0;
//      id = i + 1;
//      //Buscar donde se llama a ese ID
//      for (j = 0; j < goalsSize; j++)
//	{
//	  for (k = 0; k < goals[j].size; k++)
//	    {
//	      if (goals[j].levent[k].type == START)
//		{
//		  for (l = 0; l < goals[j].levent[k].size; l++)
//		    {
//		      if (goals[j].levent[k].values[l] == id) goto incompatibles;
//		    }
//		}
//	    }
//	}
//    incompatibles:
//      precs.lprec[i].father = j + 1;
//      //Buscar todos los que se crean en su llamada paralela
//      for (l = 0; l < goals[j].levent[k].size; l++)
//	{
//	  if (goals[j].levent[k].values[l] == id) continue;
//	  get_incompatibles(goals[j].levent[k].values[l],  &(precs.lprec[i]));
//	}
//    }

  return TRUE;
}

CBOOL__PROTO(insert_info_c)
{
  tagged_t lgoals = X(0);
  tagged_t head;
  DEREF(lgoals,lgoals);

  goalsSize++;
  goals = (struct levent*) realloc(goals, goalsSize * sizeof(struct levent));
  IEVENTS.size = 0;
  IEVENTS.levent = NULL;

  while(lgoals != EMPTY_LIST)
    {
      head = ENGINE_HeadOfTerm(lgoals);
      DEREF(head, head);

      IEVENTS.size++;
      IEVENTS.levent = 
	  (struct event*) realloc(IEVENTS.levent, IEVENTS.size * sizeof(struct event));

	
      if (ENGINE_IsAtomTerm(head))
	{
	  if (!strcmp(ENGINE_AtomName(head),"fail"))
	    {
	      IEVENT.type = FAIL;
	      IEVENT.size = 0;
	      IEVENT.values = NULL;
	    }
	  else if (!strcmp(ENGINE_AtomName(head),"answer"))
	    {
	      IEVENT.type = ANSWER;
	      IEVENT.size = 0;
	      IEVENT.values = NULL;
	    }
	  else 
	    {
	      printf("\nERROR to C: ATOMO que no es ni FAIL ni ANSWER!\n");
	      exit(1);
	    }
	}
      else
	{
	  if (!strcmp(ENGINE_NameOfFunctor(head),"t_exec"))
	    {	      
	      IEVENT.type = TIME;
	      IEVENT.size = 1;
	      IEVENT.values = (unsigned int *) malloc (sizeof(unsigned int));
	      IEVENT.values[0] = ENGINE_IntOfTerm(ENGINE_ArgOfTerm(1,head));
	    }
	  else if (!strcmp(ENGINE_NameOfFunctor(head),"start"))
	    {
	      IEVENT.type = START;
	      IEVENT.size = 0;
	      IEVENT.values = NULL;
	      
	      tagged_t value = ENGINE_ArgOfTerm(1,head);
	      DEREF(value, value);

	      while(value != EMPTY_LIST)
		{
		  IEVENT.size++;
		  IEVENT.values = 
		    (unsigned int*) realloc (IEVENT.values, IEVENT.size * sizeof(unsigned int));

		  IEVENT.values[IEVENT.size - 1] = ENGINE_IntOfTerm(ENGINE_HeadOfTerm(value));
		  
		  value = ENGINE_TailOfTerm(value);
		  DEREF(value,value);       
		}
	    }
	  else if (!strcmp(ENGINE_NameOfFunctor(head),"restart"))
	    {
	      IEVENT.type = RESTART;
	      IEVENT.size = 0;
	      IEVENT.values = NULL;
	      
	      tagged_t value = ENGINE_ArgOfTerm(1,head);
	      DEREF(value, value);

	      while(value != EMPTY_LIST)
		{
		  IEVENT.size++;
		  IEVENT.values = 
		    (unsigned int*) realloc (IEVENT.values, IEVENT.size * sizeof(unsigned int));

		  IEVENT.values[IEVENT.size - 1] = ENGINE_IntOfTerm(ENGINE_HeadOfTerm(value));
		  
		  value = ENGINE_TailOfTerm(value);
		  DEREF(value,value);       
		}
	    }
	  else 
	    {
	      printf("\nERROR to C: FUNCTOR que no es ni TIME ni START ni RESTART!\n");
	      exit(1);
	    }
	}

      lgoals = ENGINE_TailOfTerm(lgoals);
      DEREF(lgoals,lgoals);       
    }

  return TRUE;  
}



CBOOL__PROTO(initial_c)
{
  goalsSize = 0;
  goals = NULL;
  nthreads = 0;
  lwams = (struct lwam*) malloc (sizeof(struct lwam));
  lwams->Nwams = 0;
  lwams->wams = NULL;
  lready = (struct lready*) malloc (sizeof(struct lready));
  lready->size = 0;
  lready->goals = NULL;

  return TRUE;
}
