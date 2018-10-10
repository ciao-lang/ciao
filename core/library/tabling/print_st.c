#if defined(TABLING)
void print_sf(struct sf *sf)
{
  intmach_t i;
  printf("\nSUB_FACT has %ld variables\n",(long)sf->size);
  for (i = 0; i < sf->size; i++)
    printf("\tVAR %ld = %p\n",(long)i,(void*)sf->vars[i]);
}
#endif
