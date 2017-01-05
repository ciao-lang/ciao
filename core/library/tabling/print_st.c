#if defined(TABLING)
void print_sf(struct sf *sf)
{
  int i;
  printf("\nSUB_FACT has %d variables\n",sf->size);
  for (i = 0; i < sf->size; i++)
    printf("\tVAR %d = %p\n",i,(void*)sf->vars[i]);
}
#endif
