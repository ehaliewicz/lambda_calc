// interpret lambda calculus in a few lines (^:
#include "stdio.h"
#include "stdlib.h"
#include "ctype.h"
#include "string.h"
#include "setjmp.h"


/*
 syntax

   lambda = '\' symbol expr 
   application = '(' expr expr ')'
   symbol = a-z[^\s\(\)\\]*

*/

typedef enum {
  APPLICATION, LAMBDA, SYMBOL
} type;

// initial ast
struct env {
  char* sym_name;
  struct expr* sym_val;
  struct env* next;
};

struct lambda {
  char* arg;
  struct env* environ;
  struct expr* body;
};


struct appl {
  struct expr* operator;
  struct expr* operand;
};

struct expr {
  type typ;
  union {
    char* sym;
    struct appl* app;
    struct lambda* lam;
  };
};

void print_expr(struct expr* exp) {
  switch(exp->typ) {
  case SYMBOL:
    printf("%s", exp->sym);
    break;
  case LAMBDA:
    printf("\\%s ", exp->lam->arg);
    print_expr(exp->lam->body);
    break;
  case APPLICATION:
    printf("("); print_expr(exp->app->operator);
    printf(" ");
    print_expr(exp->app->operand);
    printf(")");
    break;
  }
}

// lexing/parsing utility functions

char peekchar() {
  char c = getchar();
  ungetc(c, stdin);
  return c;
}

void skip_whitespace() {
  char c = getchar();
  if(isspace(c)) {
    while(isspace(c)) {
      c = getchar();
    }
    ungetc(c, stdin);
  } else {
    ungetc(c, stdin);
  }
}

// hack for error handling
// if we ever encounter an error in parsing or evaluation
// longjmp to this buffer
jmp_buf error;


struct expr* parse_expression();

char reserved_chars[] = "(\\).";

struct expr* parse_symbol() {
  char c = getchar();
  if(isalpha(c)) {
    
    int buf_size = 32;
    char *buf = malloc(sizeof(char)*buf_size);
    int idx = 0;
    buf[idx++] = c;
    
    c = getchar();
    while(!isspace(c) && (strchr(reserved_chars, c) == NULL)) {
      // always leave one spot for null terminator
      if(idx >= (buf_size-1)) {
        buf_size *= 1.5;
        buf = realloc(buf, sizeof(char)*buf_size);
      }
      buf[idx++] = c;
      c = getchar();
    }
    
    ungetc(c, stdin);
    buf[idx++] = '\0';
    
    struct expr* res = malloc(sizeof(struct expr));
    res->typ = SYMBOL;
    res->sym = buf;
    return res;
    
  } else {
    printf("Unexpected character '%c'\n", c);
    longjmp(error, 1);
  }
}

struct expr* parse_lambda() {
  // consume '\'
  getchar();

  skip_whitespace();
  struct expr* sym_expr = parse_symbol();
  
  // parse symbol
  struct expr* lam_expr = malloc(sizeof(struct expr));
  lam_expr->lam = malloc(sizeof(struct lambda));
  lam_expr->typ = LAMBDA;
  lam_expr->lam->arg = sym_expr->sym;
  
  // parse body of lambda
  struct expr* body = parse_expression();
  lam_expr->lam->body = body;
  
  return lam_expr;
}

struct expr* parse_application() {
  // consume '('
  getchar();
  
  struct expr* operator = parse_expression();
  struct expr* operand = parse_expression();
  
  skip_whitespace();
  char c = getchar();
  if(c != ')') {
    printf("Expected ')' to end application expression, but got '%c'!\n", c);
    longjmp(error, 1);
  }

  struct expr* res = malloc(sizeof(struct expr));
  
  res->typ = APPLICATION;
  res->app = malloc(sizeof(struct appl));
  res->app->operator = operator;
  res->app->operand = operand; 
  
  return res;
}

struct expr* parse_expression() {
  skip_whitespace();
  
  char c = peekchar();
  switch(c) {
  case '\\':
    return parse_lambda();
  case '(':
    return parse_application();
  default:
    return parse_symbol();
  }
}

// evaluation 

struct expr* eval_expression(struct expr* exp, struct env* environment);

struct expr* eval_symbol_lookup(struct expr* exp, struct env* environment) {
  char* sym = exp->sym;
  
  struct env* cur_env = environment;
  while(cur_env) {
    if(strcmp(cur_env->sym_name, sym) == 0) {
      return cur_env->sym_val;
    }
    cur_env = cur_env->next;
  }
  
  printf("Tried to reference symbol '%s' that is not bound!\n", sym);
  longjmp(error, 1);
}


struct expr* eval_application(struct expr* exp, struct env* environment) {
  // extract operator and operand, and evaluate them body
  struct expr* operator = eval_expression(exp->app->operator, environment);
  struct expr* operand = eval_expression(exp->app->operand, environment);
  
  struct env* operator_env = operator->lam->environ;
  
  // bind argument to arg name in freshly allocated environment frame
  // and link environment frame to parent in the lambda, not the
  // environment passed to this function 
  // (this gives us lexical scoping vs dynamic scoping)
  struct env* ext_env = malloc(sizeof(struct env));
  ext_env->next = operator_env;
  ext_env->sym_name = operator->lam->arg;
  ext_env->sym_val = operand;

  return eval_expression(operator->lam->body, ext_env);
}

struct expr* eval_expression(struct expr* exp, struct env* environment) {
  switch(exp->typ) {
  case SYMBOL:
    // lookup in environment
    return eval_symbol_lookup(exp, environment);
  case APPLICATION:
    // apply operator to operand
    return eval_application(exp, environment);
  case LAMBDA:
    // bind lambda to environment
    exp->lam->environ = environment;
    return exp;
  }
}

int main() {
  while(1) {
    // jump back to this point if we have an error
    if(setjmp(error) == 1) {
      while(getchar() != '\n');  // skip past unread input
    }
    
    printf("Enter expression> ");
    struct expr* exp = parse_expression();
    struct expr* res = eval_expression(exp, NULL);
    printf("Result> ");
    print_expr(res);
    printf("\n");
  }
  return 0;
}
