// compile lambda calculus in a 512 lines (^:
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>

/*
   syntax

   lambda = '\' symbol expr 
   application = '(' expr expr ')'
   symbol = a-z[^\s\(\)\\]*
*/

// syntax tree structures and utility functions

typedef enum {
  APPLICATION, LAMBDA, SYMBOL
} expr_type;

struct cenv {
  char* sym_name;
  struct cenv* next;
};

struct lambda {
  char* arg;
  struct expr* body;
};

struct appl {
  struct expr* operator;
  struct expr* operand;
};

struct expr {
  expr_type typ;
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


// buffer data structure and utility functions
struct buffer {
  int buf_size;
  int buf_ptr;
  int *buf;
};

struct buffer* alloc_buffer(int init_size) {
  struct buffer *buf = malloc(sizeof(struct buffer));
  buf->buf_size = init_size;
  buf->buf_ptr = 0;
  buf->buf = malloc(sizeof(int)*init_size);
  return buf;
}

void add_to_buffer(struct buffer *buf, int val) {
  if(buf->buf_ptr >= buf->buf_size) {
    buf->buf_size *= 1.5;
    buf->buf = realloc(buf->buf, sizeof(int)*buf->buf_size);
  }
  buf->buf[buf->buf_ptr++] = val;
}

void free_buf(struct buffer* buf) {
  free(buf->buf);
  free(buf);
}

struct buffer* merge_bufs(struct buffer* buf_a, struct buffer* buf_b) {
  struct buffer* res = malloc(sizeof(struct buffer));
  res->buf_ptr = (buf_a->buf_ptr + buf_b->buf_ptr);
  res->buf_size = (buf_a->buf_size)+(buf_b->buf_size) * 1.5;
  res->buf = malloc(sizeof(int)*res->buf_size);
  
  memcpy(res->buf, buf_a->buf, (buf_a->buf_ptr*sizeof(int)));
  memcpy(res->buf+buf_a->buf_ptr, buf_b->buf, (buf_b->buf_ptr*sizeof(int)));
  return res;
}

struct buffer* merge_and_free_bufs(struct buffer* buf_a, struct buffer* buf_b) {
  struct buffer* res = merge_bufs(buf_a, buf_b);
  free_buf(buf_a);
  free_buf(buf_b);
  return res;
}

typedef enum {
  DUP, SWAP,
  JMP, CALL, RET,
  ENV_LOOKUP, EXTEND_ENV,
  PUSH_ENV, POP_ENV, GET_ENV,
  MK_CLOSURE, 
  GET_CLOSURE_ENV, GET_CLOSURE_CODE,
  GET_REL_ADDR
} opcode;


struct buffer* compile_expression(struct expr* exp, struct cenv* environment);

struct buffer* compile_symbol_lookup(struct expr* exp, struct cenv* environment) {
  char* sym = exp->sym;
  
  struct cenv* cur_env = environment;
  int offset = 0;
  while(cur_env) {
    if(strcmp(cur_env->sym_name, sym) == 0) {
      struct buffer* buf = alloc_buffer(2);
      add_to_buffer(buf, ENV_LOOKUP);
      add_to_buffer(buf, offset);
      return buf;
    }
    offset++;
    cur_env = cur_env->next;
  }
  
  printf("Tried to reference symbol '%s' that is not bound!\n", sym);
  longjmp(error, 1);
}


struct buffer* compile_application(struct expr* exp, struct cenv* environment) {
  struct buffer* ev_operand = compile_expression(exp->app->operand, environment);
  struct buffer* ev_operator = compile_expression(exp->app->operator, environment);
  
  struct buffer* buf = merge_and_free_bufs(ev_operand, ev_operator);
  
  add_to_buffer(buf, DUP);
  add_to_buffer(buf, GET_CLOSURE_ENV);
  add_to_buffer(buf, PUSH_ENV);
  add_to_buffer(buf, SWAP);
  add_to_buffer(buf, EXTEND_ENV);
  add_to_buffer(buf, GET_CLOSURE_CODE);
  add_to_buffer(buf, CALL);
  return buf;
}

struct buffer* compile_lambda(struct expr* exp, struct cenv* environment) {
  // extended environment
  
  char* arg_name = exp->lam->arg;
  
  struct cenv* ext_env = malloc(sizeof(struct cenv));
  ext_env->next = environment;
  ext_env->sym_name = arg_name;
  
  struct buffer* body = compile_expression(exp->lam->body, ext_env);
  add_to_buffer(body, POP_ENV);
  add_to_buffer(body, RET);
  

  struct buffer* buf = alloc_buffer(32);
  add_to_buffer(buf, GET_REL_ADDR);
  add_to_buffer(buf, 6);
  add_to_buffer(buf, GET_ENV);
  add_to_buffer(buf, MK_CLOSURE);
  // jump past closure
  add_to_buffer(buf, JMP);
  add_to_buffer(buf, body->buf_ptr+2);

  return merge_and_free_bufs(buf, body);
}

struct buffer* compile_expression(struct expr* exp, struct cenv* environment) {
  switch(exp->typ) {
  case SYMBOL:
    return compile_symbol_lookup(exp, environment);
  case APPLICATION:
    return compile_application(exp, environment);
  case LAMBDA:
    return compile_lambda(exp, environment);
  }
}

struct renv {
  struct val* value;
  struct renv* next;
};

struct closure {
  int body_addr;
  struct renv* bound_env;
};

typedef enum {
  ENVIRONMENT,
  CLOSURE,
  ADDRESS
} val_type;

struct val {
  val_type typ;
  union {
    struct renv* env;
    struct closure* cls;
    int addr;
  };
};

void print_value(struct val* v) {
  switch(v->typ) {
  case ENVIRONMENT:
    printf("{environment}");
    break;
  
  case CLOSURE:
    printf("<lambda>");
    break;
    
  case ADDRESS:
    printf("[address: %x]", v->addr);
    break;
    
  default:
    printf("Unrecognized object type\n");
    break;
  }
}


struct renv *env_stack[1024];
int esp = 1024;

int return_stack[1024];
int rsp = 1024;

struct val *object_stack[1024];
int sp = 1024;

struct val* execute_program(struct buffer* buf) {
  printf("Executing program\n");
  int pc = 0;
  int *prog = buf->buf;
  int prog_len = buf->buf_ptr;
  struct renv *env = NULL;
  

  struct val *tmp, *tmp2;
  int tmp_addr;
  struct renv* tmp_env;
  
  
  while(pc < prog_len) {
    opcode code = prog[pc++];
    
    switch(code) {
    case DUP:
      tmp = object_stack[sp];
      object_stack[--sp] = tmp;
      break;
      
    case SWAP:
      tmp = object_stack[sp++];
      tmp2 = object_stack[sp++];
      object_stack[--sp] = tmp2;
      object_stack[--sp] = tmp;
      break;

    case JMP:
      tmp_addr = pc-1;
      pc = tmp_addr + prog[pc];
      break;
      
    case CALL:
      // push return address on stack
      return_stack[--rsp] = pc+1;
      // jump to location
      pc = object_stack[sp++]->addr;
      break;

    case RET:
      pc = return_stack[rsp++];
      break;

    case ENV_LOOKUP:
      // grab environment offset
      tmp_addr = prog[pc++];
      tmp_env = env;
      while(tmp_addr--) {
        tmp_env = tmp_env->next;
      }
      object_stack[--sp] = tmp_env->value;
      break;
      
    case PUSH_ENV:
      env_stack[--esp] = env;
      env = object_stack[sp++]->env;
      break;
      
    case POP_ENV:
      env = env_stack[esp++];
      break;

    case EXTEND_ENV:
      tmp_env = malloc(sizeof(struct renv));
      tmp_env->value = object_stack[sp++];
      tmp_env->next = env;
      env = tmp_env;
      break;

    case GET_ENV:
      tmp = malloc(sizeof(struct val));
      tmp->typ = ENVIRONMENT;
      tmp->env = env;
      object_stack[--sp] = tmp;
      break;

    case MK_CLOSURE:
      tmp = malloc(sizeof(struct val));
      tmp->typ = CLOSURE;
      tmp->cls = malloc(sizeof(struct closure));
      tmp->cls->bound_env = object_stack[sp++]->env;
      tmp->cls->body_addr = object_stack[sp++]->addr;
      object_stack[--sp] = tmp;
      break;

    case GET_CLOSURE_ENV:
      tmp = malloc(sizeof(struct val));
      tmp->typ = ENVIRONMENT;
      tmp->env = object_stack[sp++]->cls->bound_env;
      object_stack[--sp] = tmp;
      break;

    case GET_CLOSURE_CODE:
      tmp = malloc(sizeof(struct val));
      tmp->typ = ADDRESS;
      tmp->addr = object_stack[sp++]->cls->body_addr;
      object_stack[--sp] = tmp;
      break;
      
    case GET_REL_ADDR:
      tmp_addr = pc-1;
      tmp_addr += prog[pc++];
      
      tmp = malloc(sizeof(struct val));
      tmp->typ = ADDRESS;
      tmp->addr = tmp_addr;
      object_stack[--sp] = tmp;
      break;
      
    }
  }
  if(sp != 1023) {
    printf("Execution ended with more than one item on the stack!\n");
    exit(1);
  }
  return object_stack[sp];
}

int main() {
  while(1) {
    // jump back to this point if we have an error
    if(setjmp(error) == 1) {
      while(getchar() != '\n');  // skip past unread input
    }
    
    printf("Enter expression> ");
    struct expr* exp = parse_expression();
    struct buffer* compiled_code = compile_expression(exp, NULL);
    
    struct val* result = execute_program(compiled_code);
    printf("Result> ");
    print_value(result);
    printf("\n");
  }
  return 0;
}
