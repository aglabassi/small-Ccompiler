/* fichier: "petit-comp.c" */
/* Auteur du code squelette : Marc Feeley */
/* Auteur: Abdel Ghani Labassi  */
/* IFT2035  */

/* Un petit compilateur et machine virtuelle pour un sous-ensemble de C.  */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <setjmp.h>

/* Code d'erreur: 1-syntaxe, 2-allocation, 3-erreur de branchement */
int err_code;
/*---------------------------------------------------------------------------*/

/* Analyseur lexical. */

enum { DO_SYM, ELSE_SYM, IF_SYM, WHILE_SYM, PRINT_SYM, GOTO_SYM, BREAK_SYM, CONTINUE_SYM, LBRA, RBRA, LPAR,
       RPAR, PLUS, MINUS, STAR, SLASH, PERCENT, LESS, LEQTEST, GREATER,
       GEQTEST, EQTEST, NEQTEST, SEMI, EQUAL, INT, ID, ETQ, EOI };

char *words[] = { "do", "else", "if", "while", "print", "goto", "break", "continue", NULL };

int ch = ' ';
int sym;
int int_val;
char id_name[100];

/*
    etqspos est un tableau servant a contenir la position de l'enonce identifie
    par une etiquette dans le code objet ( ordre important : indice 0 est relie au charactere a... )
    -1 indique que l'id n'est pas une etiquette, 0 indique que c'en est une mais la position
    dans l'objet reste a determiner.
*/
int etqspos[26];

/*
    Initialisation, -1 indique etiquette jamais utilise.
    0 indique utilise, Le generateur de code va remplacer 0 par
    la position dans le code objet
*/
void init_etqs() { int i; for (i = 0; i<26; i++) etqspos[i] = -1; }


void syntax_error() { fprintf(stderr, "syntax error\n"); err_code = 1; }

void next_ch() { ch = getchar(); }

void next_sym()
{
  while (ch == ' ' || ch == '\n') next_ch();
  switch (ch)
    { case '{': sym = LBRA;   next_ch(); break;
      case '}': sym = RBRA;   next_ch(); break;
      case '(': sym = LPAR;   next_ch(); break;
      case ')': sym = RPAR;   next_ch(); break;
      case '+': sym = PLUS;   next_ch(); break;
      case '-': sym = MINUS;  next_ch(); break;
      case '*': sym = STAR;   next_ch(); break;
      case '/': sym = SLASH;  next_ch(); break;
      case '%': sym = PERCENT;next_ch(); break;
      case ';': sym = SEMI;   next_ch(); break;

      /* Traitement de symbole d'operation a 2 caracteres */
      case '<': sym = LESS;  next_ch();
                if(ch == '=') { sym = LEQTEST; next_ch(); } break;
      case '>': sym = GREATER;  next_ch();
                if(ch == '=') { sym = GEQTEST; next_ch(); } break;
      case '=': sym = EQUAL; next_ch();
                if(ch == '=') { sym = EQTEST; next_ch(); }  break;
      case '!': next_ch();
                if(ch == '=') { sym = NEQTEST; next_ch(); }
                else syntax_error();                        break;


     /* Traitement des id et des nombres */
      case EOF: sym = EOI;   next_ch(); break;
      default:
        if (ch >= '0' && ch <= '9')
          {
            int_val = 0; /* overflow? */
      
            while (ch >= '0' && ch <= '9')
              {
                int_val = int_val*10 + (ch - '0');
                next_ch();
              }
      
            sym = INT;
          }
        else if (ch >= 'a' && ch <= 'z')
          {
           int i = 0; /* overflow? */
      
            while ((ch >= 'a' && ch <= 'z') || ch == '_' || ch == ':')
              {
                id_name[i++] = ch;
                next_ch();
              }
      
            id_name[i] = '\0';
            sym = 0;
      
            while (words[sym]!=NULL && strcmp(words[sym], id_name)!=0)
              sym++;
      
            if (words[sym] == NULL)
              {
                if (id_name[1] == '\0') sym = ID;

                else if (id_name[1] == ':' && id_name[2] == '\0' &&
                 etqspos[id_name[0] - 'a' ] == -1 )
                 /*
                     Pour qu'une etiquette soit valide, il faut que l'id nest pas
                     ete utilise prealablement comme un etiquette. Il peut etre utilise
                     comme variable mais une fois qu'on le declare comme etiquette,
                     ce n'est plus possible, ni en tant qu'etiquette, ni en tant que variable (cas
                     traite dans l'analyseur syntaxique).
                  */
                 {
                    etqspos[id_name[0] - 'a' ] = 0; /* Initialisation de l'etiquette */
                    sym = ETQ;
                 }

                else syntax_error();
              }
          }
        else syntax_error();
    }
}

/*---------------------------------------------------------------------------*/

/* Analyseur syntaxique. */


enum { VAR, CST, ADD, SUB, MULT, DIV, MOD, LT, LEQ, GT, GEQ, EQ, NEQ, ASSIGN,
       IF1, IF2, WHILE, DO, PRINT, GOTO_NODE, BREAK, CONTINUE, EMPTY, SEQ, EXPR, ETQSTAT, PROG };

struct node
  {
    int kind;
    struct node *o1;
    struct node *o2;
    struct node *o3;
    int val;
  };

typedef struct node node;

#define CATCH() catcher: if(err_code != 0){  nettoyageASA(x); return NULL; }
#define TEST() if(err_code!=0) goto catcher;

void malloc_error() { fprintf(stderr, "malloc returned NULL\n"); err_code = 2; }

/*
    On fait des liberations dans un parcours semblant a postfix, de maniere
    recursive
*/
void nettoyageASA(node* racine)
{
    if(racine == NULL) return; //cas de base

    nettoyageASA(racine->o1);
    nettoyageASA(racine->o2);
    nettoyageASA(racine->o3);

    free(racine);

}

node *new_node(int k)
{
  node *x = malloc(sizeof(node));
  if(x == NULL ){malloc_error(); return NULL; }/* raise error */
  x->kind = k;
  x->o1 = NULL; x->o2 = NULL; x->o3 = NULL; /* Iinitialisation */
  return x;
}

node *paren_expr(); /* forward declaration */

node *term() /* <term> ::= <id> | <int> | <paren_expr> */
{
  node *x = NULL;

  if (sym == ID)           /* <term> ::= <id> */
    {
      if(etqspos[id_name[0] - 'a'] != -1) syntax_error(); /* ID sert d'etiquette. */
      TEST()
      x = new_node(VAR);
      TEST()
      x->val = id_name[0]-'a';
      next_sym();
      TEST()
    }
  else if (sym == INT)     /* <term> ::= <int> */
    {
      x = new_node(CST);
      TEST()
      x->val = int_val;
      next_sym();
      TEST()
    }
  else{                  /* <term> ::= <paren_expr> */
        x = paren_expr();
        TEST()
       }
  CATCH()
  return x;
}

node *mult() /* <term> | <mult> "*" <term> | <mult> "/" <term> | <mult> "%" <term> */
{
    node *x;
    x= term();
    TEST()

    while (sym == STAR || sym == SLASH || sym == PERCENT)/* Conversion de la recurence */
      {
        node *t = x;
        switch(sym)
          {
            case STAR:    x = new_node(MULT); break;
            case SLASH:   x = new_node(DIV);  break;
            case PERCENT: x = new_node(MOD);  break;
          }
        if(err_code == 2){ nettoyageASA(t); return NULL;}

        next_sym();
        x->o1 = t;
        TEST();
        x->o2 = term();
        TEST()

      }

    CATCH()
    return x;
}

node *sum() /* <mult> | <sum> "+" <mult> | <sum> "-" <mult> */
{
  node *x = mult();
  TEST()

  while (sym == PLUS || sym == MINUS) /* Conversion de la recurence */
    {
      node *t = x;
      x = new_node(sym==PLUS ? ADD : SUB);

      if(err_code == 2){ nettoyageASA(t); return NULL;} /* MALLOC ECHEC */

      next_sym();
      x->o1 = t;
      TEST()
      x->o2 = mult();
      TEST()

    }
  CATCH()
  return x;
}

node *test() /* <test> ::= <sum> | <sum> "<" <sum> */
{
  node *x = sum();
  TEST()

  if (sym == LESS)
    {
      node *t = x;
      x = new_node(LT);
      if(err_code == 2) { nettoyageASA(t); return NULL;}
      next_sym();
      x->o1 = t;
      TEST()
      x->o2 = sum();
      TEST()

    }

    else if (sym == LEQTEST)
      {
        node *t = x;
        x = new_node(LEQ);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }

    else if (sym == GREATER)
      {
        node *t = x;
        x = new_node(GT);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }

    else if (sym == GEQTEST)
      {
        node *t = x;
        x = new_node(GEQ);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }

    else if (sym == EQTEST)
      {
        node *t = x;
        x = new_node(EQ);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }

    else if (sym == EQTEST)
      {
        node *t = x;
        x = new_node(EQ);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }
    else if (sym == NEQTEST)
      {
        node *t = x;
        x = new_node(NEQ);
        if(err_code == 2) { nettoyageASA(t); return NULL;}
        next_sym();
        x->o1 = t;
        TEST()
        x->o2 = sum();
        TEST()

      }
  CATCH()
  return x;

}

node *expr() /* <expr> ::= <test> | <id> "=" <expr> */
{
  node *x = NULL;

  if (sym != ID) return test();

  x = test();
  TEST()

  if (sym == EQUAL)
    {
      node *t = x;
      x = new_node(ASSIGN);
      if(err_code == 2) { nettoyageASA(t); return NULL;}
      next_sym();
      x->o1 = t;
      x->o2 = expr();
      TEST()

    }
  CATCH()
  return x;
}

node *paren_expr() /* <paren_expr> ::= "(" <expr> ")" */
{
  node *x = NULL;

  if (sym == LPAR) next_sym(); else syntax_error();
  TEST()

  x = expr();
  TEST()

  if (sym == RPAR) next_sym();
  else syntax_error();
  TEST()

  CATCH()
  return x;
}

node *statement()
{
  node *x = NULL;

  if (sym == IF_SYM)       /* "if" <paren_expr> <stat> */
    {
      x = new_node(IF1);
      TEST()
      next_sym();
      TEST()
      x->o1 = paren_expr();
      TEST()
      x->o2 = statement();
      TEST()
      if (sym == ELSE_SYM) /* ... "else" <stat> */
        { x->kind = IF2;
          next_sym();
          TEST()
          x->o3 = statement();
          TEST()
        }
    }
  else if (sym == WHILE_SYM) /* "while" <paren_expr> <stat> */
    {
      x = new_node(WHILE);
      TEST()
      next_sym();
      TEST()
      x->o1 = paren_expr();
      TEST()
      x->o2 = statement();
      TEST()
    }
  else if (sym == DO_SYM)  /* "do" <stat> "while" <paren_expr> ";" */
    {
      x = new_node(DO);
      TEST()
      next_sym();
      x->o1 = statement();
      TEST()
      if (sym == WHILE_SYM) next_sym(); else syntax_error();
      TEST()
      x->o2 = paren_expr();
      TEST()
      if (sym == SEMI) next_sym(); else syntax_error();
      TEST()
    }

   else if (sym == PRINT_SYM)  /* "print" <paren_expr> ";" */
    {
      x = new_node(PRINT);
      TEST()
      next_sym();
      TEST()
      x->o1 = paren_expr();
      TEST()
      if (sym == SEMI) next_sym(); else syntax_error();
      TEST()

    }

  else if (sym == SEMI)    /* ";" */
    {
      x = new_node(EMPTY);
      TEST()
      next_sym();
      TEST()
    }
  else if (sym == LBRA)    /* "{" { <stat> } "}" */
    {
      x = new_node(EMPTY);
      TEST()
      next_sym();
      TEST()

      while (sym != RBRA )
        {
          if(err_code != 0) goto catcher;
          node *t = x;
          x = new_node(SEQ);
          if(err_code == 2) { nettoyageASA(t); return NULL;}
          x->o1 = t;
          x->o2 = statement();
          TEST()
        }

      next_sym();
      TEST()
    }
  else if(sym == ETQ)
    {
      x = new_node(ETQSTAT);
      TEST()
      x->val = id_name[0] - 'a';
      next_sym();
      TEST()
      x->o1 = statement();
      TEST()
    }
   else if(sym == BREAK_SYM || sym == CONTINUE_SYM )  /* break [<id>]; ou continue [<id>]; */
    {
      x = new_node(sym == BREAK_SYM ? BREAK: CONTINUE);
      TEST()
      next_sym();
      TEST()
      if(sym == ID )
        {
          x->val = id_name[0] - 'a';
          next_sym();
          if(sym == SEMI) next_sym(); else syntax_error();
          TEST()
         }
      else if (sym == SEMI)
         {
          x->val = -1; /* valeur special pour indiquer un break/continue sans etiquette */
          next_sym();
          TEST()
         }
        else syntax_error();

    }
  else if(sym == GOTO_SYM)  /* goto <id>;*/
    {
        x = new_node(GOTO_NODE);
        TEST()
        next_sym();
        TEST()
        if(sym != ID ) syntax_error();
        TEST()
        x->val = id_name[0] - 'a';
        next_sym();
        if (sym == SEMI) next_sym(); else syntax_error();
        TEST()
    }

  else                     /* <expr> ";" */
    {
      x = new_node(EXPR);
      TEST()
      x->o1 = expr();
      TEST()
      if (sym == SEMI) next_sym(); else syntax_error();
      TEST()
    }

  /*
    Dans le cas d'un appel a une des fonction derreur, on va liberer tous le sous-arbre creer,
    et on retourne NULL pour permettre aux autres arbres de se liberer sans double free.
  */

  CATCH()
  return x;

}

node *program()  /* <program> ::= <stat> */
{
  node *x = NULL;
  x = new_node(PROG);
  TEST()

  next_sym();
  TEST()
  x->o1 = statement();
  TEST()
  if (sym != EOI) syntax_error();
  TEST()

  CATCH()
  return x;

}

/*---------------------------------------------------------------------------*/

/* Generateur de code. */

/*
    Noter que PSI veut dire pseudo instructions, i.e des instructions qui devront etre remplace
    par des non pseudo-instructions
*/
enum { ILOAD, ISTORE, BIPUSH, IFEQ, IFNE, IFLT, PSIBREAK, PSICONTINUE, GOTO, PSIGOTO,
        DUP, POP, IADD, ISUB, IMULT, IDIV, IMOD, IPRINT, RETURN };

typedef signed char code;

code object[1000], *here = object;
int begloops[1000]; /* Stocke la position absolue des boucles dans le code object */
int testloops[1000];
int endloops[1000];
int loop = -1; /* Boucle presentement en cours, i.e ce nombre identifie les boucles  */

void gen(code c) { *here++ = c; } /* overflow? */

#ifdef SHOW_CODE
#define g(c) do { printf(" %d",c); gen(c); } while (0)
#define gi(c) do { printf("\n%s", #c); gen(c); } while (0)
#else
#define g(c) gen(c)
#define gi(c) gen(c)
#endif

void fix(code *src, code *dst) { *src = dst-src; } /* overflow? */

jmp_buf env; /* En cas d'erreur a la compilations, on aimerait sauter a main pour
faire un traitement de nettoyage et de fermeture */


/*
    Erreur d'un goto avec mauvaise adresse, break et continue sans boucle englobante, ou
    adresse de branchement trop grande
 */
void branching_error() {  fprintf(stderr, "branching error\n"); err_code = 3; longjmp(env,1); }


void c(node *x)
{ switch (x->kind)
    { case VAR   : gi(ILOAD); g(x->val); break;

      case CST   : gi(BIPUSH); g(x->val); break;

      case ADD   : c(x->o1); c(x->o2); gi(IADD); break;

      case SUB   : c(x->o1); c(x->o2); gi(ISUB); break;

      case MULT  : c(x->o1); c(x->o2); gi(IMULT); break;

      case DIV   : c(x->o1); c(x->o2); gi(IDIV); break;

      case MOD   : c(x->o1); c(x->o2); gi(IMOD); break;

      case LT    : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case LEQ   : gi(BIPUSH); g(0); /* Equivalent a not ( o2 < o1) */
                   c(x->o2);
                   c(x->o1);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(1); break;

      case GT   :  gi(BIPUSH); g(1); /* Equivalent a ( o2 < o1) */
                   c(x->o2);
                   c(x->o1);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case GEQ   :  gi(BIPUSH); g(0); /* Equivalent a  not ( o1 < o2) */
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFLT); g(4);
                   gi(POP);
                   gi(BIPUSH); g(1); break;

      case EQ    : gi(BIPUSH); g(1);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFEQ); g(4);
                   gi(POP);
                   gi(BIPUSH); g(0); break;

      case NEQ   : gi(BIPUSH); g(0);
                   c(x->o1);
                   c(x->o2);
                   gi(ISUB);
                   gi(IFEQ); g(4);
                   gi(POP);
                   gi(BIPUSH); g(1); break;

      case ASSIGN: c(x->o2);
                   gi(DUP);
                   gi(ISTORE); g(x->o1->val); break;

      case IF1   : { code *p1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2); fix(p1,here); break;
                   }

      case IF2   : { code *p1, *p2;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2);
                     gi(GOTO); p2 = here++; fix(p1,here);
                     c(x->o3); fix(p2,here); break;
                   }

      case WHILE : { loop++;
                     int temp = loop; /* Eviter conflit en cas de boucles imbriquees */
                     begloops[temp] = here - object;
                     testloops[temp] = here - object;

                     code *p1 = here, *p2;
                     c(x->o1);
                     gi(IFEQ); p2 = here++;
                     c(x->o2);
                     gi(GOTO); fix(here++,p1); fix(p2,here);

                     endloops[temp] = here - object; break;
                   }

      case DO    : { loop++;
                     int temp = loop;
                     begloops[temp] = here - object;

                     code *p1 = here; c(x->o1);

                     testloops[temp] = here - object;

                     c(x->o2);
                     gi(IFNE); fix(here++,p1);

                     endloops[temp] = here- object; break;
                   }

      case PRINT :   c(x->o1);
                     gi(IPRINT); break;



      case BREAK :   gi(PSIBREAK); g( (x-> val == -1) ? -1 : x-> val ); /* Indiquer a c2 sans etiquette */
                     break;


      case CONTINUE :   gi(PSICONTINUE); g( (x-> val == -1) ? -1 : x-> val ); /* Indiquer a c2 sans etiquette */
                        break;


      case GOTO_NODE :   if(etqspos[x->val] == -1) branching_error();
                         gi(PSIGOTO); g(x->val); break;


      case EMPTY : break;

      case SEQ   : c(x->o1);
                   c(x->o2); break;

      case EXPR  : c(x->o1);
                   gi(POP); break;

      case ETQSTAT:   etqspos[x->val] = here - object; /* Position dans le code objet */
                      c(x->o1); break;


      case PROG  : c(x->o1);
                   gi(RETURN); break;
    }

}



/*
    Position relative : distance du saut a faire pour atteindre l'enonce identifie par l'etiquette a
    partir de ou on est presentement dans l'objet.

    On retablie la position relative dans une instruction GOTO2, qui prend comme parametre
    la position absolue de branchement, en convertissant celle-ci en instruction GOTO, qui elle
    prend comme parametre de branchement la position relative. Ajuste aussi les instruction temporaire C et B
    en GOTO, avec position relative de branchement.
*/
void c2()
{
    int i;
    for( i = 0; i < here - object; i++)
      {
        /* Overflow dans les distances de branchements */
        if( object[i + 1] > 127 || object[i] < -128  ) branching_error();

        if (object[i] == PSIGOTO)
          {
            /* Position de branchement relative */
            int t = etqspos[ object[i+1] ] - i -1 ;

            /* Overflow */
            if( t > 127 || t < -128  ) branching_error();

            /* on convertie goto2 en goto */
            object[i] = GOTO;
            object[i+1] = t;
            i++;


          }
        else if (object[i] == PSIBREAK || object[i] == PSICONTINUE )
          {
            int pos_loop;

            if(object[i+1] != -1)
                /* On cherche la position de la boucle identifiee par une etiquette */
                for( pos_loop = loop; pos_loop >= 0; pos_loop-- )
                    { if(begloops[pos_loop] == etqspos[ object[i+1] ] ) break; }

            else
               /* On cherche la premiere boucle la plus profonde englobante */
               for( pos_loop = loop; pos_loop >= 0; pos_loop--)
                    if(  i < endloops[pos_loop] && i > begloops[pos_loop] ) break;

            /* Boucle englobante ou etiquette non trouve */
            if(pos_loop == -1) branching_error();
            if(!(i < endloops[pos_loop] && i > begloops[pos_loop] )) branching_error();

            /* Position de branchement relative */
            int pos_rel = (object[i] == PSIBREAK ? endloops[pos_loop] - i -1 :
                    testloops[pos_loop] - i -1) ;

             /* Overflow */
            if( pos_rel > 127 || pos_rel < -128  ) branching_error();

            /* Ecriture de la bonne instruction */
            object[i]  = GOTO;
            object[i+1] = pos_rel;
            i++;


          }
        else if (object[i] < PSIGOTO) i++;  /* instructions 2 bytes */

      }

}



/*---------------------------------------------------------------------------*/

/* Machine virtuelle. */

int globals[26];

void run()
{
  int stack[1000], *sp = stack; /* overflow? */
  code *pc = object;

  for (;;)
    switch (*pc++)
      {
        case ILOAD : *sp++ = globals[*pc++];             break;
        case ISTORE: globals[*pc++] = *--sp;             break;
        case BIPUSH: *sp++ = *pc++;                      break;
        case DUP   : sp++; sp[-1] = sp[-2];              break;
        case POP   : --sp;                               break;
        case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;     break;
        case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;     break;
        case IMULT : sp[-2] = sp[-2] * sp[-1]; --sp;     break;
        case IDIV  : sp[-2] = sp[-2] / sp[-1]; --sp;     break;
        case IMOD  : sp[-2] = sp[-2] % sp[-1]; --sp;     break;
        case IPRINT: printf("%d\n", *--sp);              break;
        case GOTO  : pc += *pc;                          break;
        case IFEQ  : if (*--sp==0) pc += *pc; else pc++; break;
        case IFNE  : if (*--sp!=0) pc += *pc; else pc++; break;
        case IFLT  : if (*--sp< 0) pc += *pc; else pc++; break;
        case RETURN: return;
    }
}

/*---------------------------------------------------------------------------*/

/* Programme principal. */

int main()
{
  /* Erreur a la compilation */
  if(setjmp(env) == 1) goto catcher;

  init_etqs();
  int i;
  node* x = program();

  /* Erreur de syntax ou d'allocation */
  if (x == NULL ) return 1;

  /* Generation du code objet  correction des instructions GOTO2, B et C; */
  c(x);
  c2();

  /* Desallocation de lASA si succes de la compilation */
  nettoyageASA(x);



#ifdef SHOW_CODE
  printf("\n");
#endif

  for (i=0; i<26; i++)
    globals[i] = 0;

  run();


  /* Traitement d'erreur a la compilation */
  catcher:
  if(err_code!=0)
    {
        nettoyageASA(x);
        return 1;
    }

  return 0;
}

/*---------------------------------------------------------------------------*/
