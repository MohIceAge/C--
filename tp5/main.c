#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void qu1() {
  int x = 1;
  int *p;
  p = &x;
  printf("x vaut: %d\n", x);
  printf("p vaut: %d\n", p);
  printf("*p vaut: %d\n", *p);
  printf("&x vaut: %d\n", &x);
  *p = 2;
  printf("x vaut maintenant: %d\n", x);
  x = 5;
  printf("*p vaut maintenant: %d\n", *p);
}
/*
int main() {
  qu1();
  qu3();
  return 0;
}
*/
void swap(int *a, int *b){
  int c = *b;
  *b = *a; *a = c;
}

void qu3() {
  int tab[5];
  tab[0] = 2;
  tab[1] = 5;
  printf("\ntab[0] vaut: %d\n", tab[0]);
  printf("*tab vaut: %d\n", *tab);
  printf("tab[1] vaut: %d\n", tab[1]);
  printf("*(tab+1) vaut: %d\n", *(tab+1));
}

void minMax(int tab[], int taille, int* min, int* max) {
  if (taille > 0) {
    *min = *max = *tab;
    for(int i = 1; i < taille; i++) {
      if (tab[i] > *max) *max = tab[i];
      if (tab[i] < *min) *min = tab[i];
    }
  }
}

int* get_tab(int size, int i) {
    int tab[size];
    //int* tab = malloc(size*sizeof(int));
    int j=0;
    for (j=0;j<=size-1;++j) {
        tab[j]=i;
    };
    return tab;
}

/*
int main() {
    printf("tab content at pos 1 : %d",get_tab(5,3)[1]);
    return 0;
}
*/
///// ex 3

void dec(char *c, int i)
{
  if (*"a" <= *c && *c <= *"z") *c = *"a"+(*c-*"a"+i)%25;
  if (*"A" <= *c && *c <= *"Z") *c = *"A"+(*c-*"A"+i)%25;
}
void chiffr (char text[], int taille, int k1, int k2, char res[]) {
  for (int i = 0; i < taille; i++) res[i] = *(text+(i+k1)%taille);
  //for (int i = 0; i < taille; i++) dec(res+i, k2);
  printf("\n*%s\n",res);
}
int main(int argc, char* arg[]) {
  if (argc != 5) return 0;
  int l = strlen(arg[4]);
  char* res = malloc ((l+1)*sizeof(char));
  *(res+l) = 0;
  if (strcmp(arg[1], "-c"))
    chiffr(arg[4], l, atoi(arg[2]), atoi(arg[3]), res);
  printf("\n*%s\n", arg[4]);
  printf("\n*%s\n",res);
  return 0;
}
