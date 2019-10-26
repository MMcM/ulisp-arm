/* uLisp ARM 2.9b - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 25th September 2019

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// Lisp Library
const char LispLibrary[] PROGMEM = "(defun load-file (filename) (princ \"Loading \") (princ filename) (terpri) (with-sd-card (s filename) (loop (let ((expr (read s))) (if (null expr) (return) (eval expr))))))"
  "(dolist (fname (with-sd-card (s \"filelist.lsp\") (read s))) (load-file fname))";

// Compile options

// #define resetautorun
#define printfreespace
#define serialmonitor
// #define printgcs
#define sdcardsupport
#define lisplibrary
#define lis3dhsupport

// Includes

//#include "LispLibrary.h"
#include <setjmp.h>
#include <SPI.h>
#include <Wire.h>
#include <limits.h>
#include <Bounce2.h>

#if defined(sdcardsupport)
#include <SD.h>
#define SDSIZE 172
#else
#define SDSIZE 0
#endif

#if defined(lis3dhsupport)
#include <Adafruit_LIS3DH.h>
#endif

// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           (((object *) (x))->car)
#define second(x)          (car(cdr(x)))
#define cddr(x)            (cdr(cdr(x)))
#define third(x)           (car(cdr(cdr(x))))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

#define integerp(x)        ((x) != NULL && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && (x)->type == FLOAT)
#define symbolp(x)         ((x) != NULL && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && (x)->type == CHARACTER)
#define streamp(x)         ((x) != NULL && (x)->type == STREAM)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

// Constants

const int TRACEMAX = 3; // Number of traced functions
enum type { ZERO=0, SYMBOL=2, NUMBER=4, STREAM=6, CHARACTER=8, FLOAT=10, STRING=12, PAIR=14 };  // STRING and PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT, BACKTICK, COMMA, COMMAAT};
enum stream { SERIALSTREAM, I2CSTREAM, SPISTREAM, SDSTREAM };

enum function { NIL, TEE, NOTHING, OPTIONAL, AMPREST, LAMBDA, LET, LETSTAR, CLOSURE, SPECIAL_FORMS, QUOTE,
DEFUN, DEFVAR, SETQ, LOOP, RETURN, PUSH, POP, INCF, DECF, SETF, DOLIST, DOTIMES, TRACE, UNTRACE,
FORMILLIS, WITHSERIAL, WITHI2C, WITHSPI, WITHSDCARD,
MACRO, DEFMACRO, QUASIQUOTE, UNQUOTE, UNQUOTESPLICING, EXPAND,
TAIL_FORMS, PROGN, IF, COND, WHEN, UNLESS, CASE, AND,
OR, FUNCTIONS, NOT, NULLFN, CONS, ATOM, LISTP, CONSP, SYMBOLP, STREAMP, EQ, EQUAL, CAR, FIRST, CDR, REST, CAAR,
CADR, SECOND, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, THIRD, CDAAR, CDADR, CDDAR, CDDDR, LENGTH, LIST,
REVERSE, NTH, ASSOC, MEMBER, APPLY, FUNCALL, APPEND, MAPC, MAPCAR, MAPCAN, ADD, SUBTRACT, MULTIPLY,
DIVIDE, MOD, ONEPLUS, ONEMINUS, ABS, RANDOM, MAXFN, MINFN, NOTEQ, NUMEQ, LESS, LESSEQ, GREATER, GREATEREQ,
PLUSP, MINUSP, ZEROP, ODDP, EVENP, INTEGERP, NUMBERP, FLOATFN, FLOATP, SIN, COS, TAN, ASIN, ACOS, ATAN,
SINH, COSH, TANH, EXP, SQRT, LOG, EXPT, CEILING, FLOOR, TRUNCATE, ROUND, CHAR, CHARCODE, CODECHAR,
CHARACTERP, STRINGP, STRINGEQ, STRINGLESS, STRINGGREATER, SORT, STRINGFN, CONCATENATE, SUBSEQ,
READFROMSTRING, PRINCTOSTRING, PRIN1TOSTRING, LOGAND, LOGIOR, LOGXOR, LOGNOT, ASH, LOGBITP, EVAL, GLOBALS,
LOCALS, MAKUNBOUND, BREAK, READ, PRIN1, PRINT, PRINC, TERPRI, READBYTE, READLINE, WRITEBYTE, WRITESTRING,
WRITELINE, RESTARTI2C, GC, ROOM, SAVEIMAGE, LOADIMAGE, CLS, PINMODE, DIGITALREAD, DIGITALWRITE,
ANALOGREAD, ANALOGWRITE, DELAY, MILLIS, SLEEP, NOTE, EDIT, PPRINT, PPRINTALL, REQUIRE, LISTLIBRARY,
MAKEDEBOUNCER, UPDATEDEBOUNCERS, DEBOUNCERVALUE, DEBOUNCERROSE, DEBOUNCERFELL,
INITLIS3DH, LIS3DHBUMP, LIS3DHACCELERATION,
ENDFUNCTIONS };

// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t min;
  uint8_t max;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;

// Workspace
#define PERSIST __attribute__((section(".text")))
#define WORDALIGNED __attribute__((aligned (4)))
#define BUFFERSIZE 34  // Number of bits+2

#if defined(ARDUINO_ITSYBITSY_M0)
  #define WORKSPACESIZE 3072-SDSIZE       /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_GEMMA_M0)
  #define WORKSPACESIZE 3072-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_FEATHER_M0_EXPRESS)
  #define WORKSPACESIZE 3072-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define SDCARD_SS_PIN 4
  uint8_t _end;

#elif defined(ARDUINO_METRO_M4)
  #define WORKSPACESIZE 20480-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_ITSYBITSY_M4)
  #define WORKSPACESIZE 20480-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_FEATHER_M4)
  #define WORKSPACESIZE 20480-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 2048000           /* 2 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define SDCARD_SS_PIN 15
  uint8_t _end;

#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  #define WORKSPACESIZE 30720-SDSIZE      /* Objects (8*bytes) */
  #define DATAFLASHSIZE 8192000           /* 8 MBytes */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_SAM_DUE)
  #define WORKSPACESIZE 10240-SDSIZE      /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  #define SDCARD_SS_PIN 10
  extern uint8_t _end;

#elif defined(ARDUINO_SAMD_MKRZERO)
  #define WORKSPACESIZE 3072-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  uint8_t _end;

#elif defined(ARDUINO_SAMD_ZERO)          /* Put this last, otherwise overrides the Adafruit boards */
  #define WORKSPACESIZE 3072-SDSIZE       /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  #define SDCARD_SS_PIN 10
  uint8_t _end;

#elif defined(_VARIANT_BBC_MICROBIT_)
  #define WORKSPACESIZE 1280              /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 512             /* Bytes */
  uint8_t _end;

#elif defined(MAX32620)
  #define WORKSPACESIZE 24576-SDSIZE      /* Objects (8*bytes) */
  #define SYMBOLTABLESIZE 1024            /* Bytes */
  uint8_t _end;

#endif

object Workspace[WORKSPACESIZE] WORDALIGNED;
char SymbolTable[SYMBOLTABLESIZE];

// Global variables

jmp_buf exception;
unsigned int Freespace = 0;
object *Freelist;
char *SymbolTop = SymbolTable;
unsigned int I2CCount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
int GlobalStringIndex = 0;
char BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;

// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED };
volatile char Flags;

// Forward references
object *tee;
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read ();
void repl(object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);
void error (symbol_t fname, PGM_P string, object *symbol);
void error2 (symbol_t fname, PGM_P string);

// Set up workspace

void initworkspace () {
  Freelist = NULL;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
  }
}

object *myalloc () {
  if (Freespace == 0) error2(0, PSTR("no room"));
  object *temp = Freelist;
  Freelist = cdr(Freelist);
  Freespace--;
  return temp;
}

inline void myfree (object *obj) {
  car(obj) = NULL;
  cdr(obj) = Freelist;
  Freelist = obj;
  Freespace++;
}

// Make each type of object

object *number (int n) {
  object *ptr = myalloc();
  ptr->type = NUMBER;
  ptr->integer = n;
  return ptr;
}

object *makefloat (float f) {
  object *ptr = myalloc();
  ptr->type = FLOAT;
  ptr->single_float = f;
  return ptr;
}

object *character (char c) {
  object *ptr = myalloc();
  ptr->type = CHARACTER;
  ptr->integer = c;
  return ptr;
}

object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}

object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}

object *newsymbol (symbol_t name) {
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && obj->name == name) return obj;
  }
  return symbol(name);
}

object *stream (unsigned char streamtype, unsigned char address) {
  object *ptr = myalloc();
  ptr->type = STREAM;
  ptr->integer = streamtype<<8 | address;
  return ptr;
}

// Garbage collection

void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);

  if (type >= PAIR || type == ZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if (type == STRING) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}

void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

void gc (object *form, object *env) {
  #if defined(printgcs)
  int start = Freespace;
  #endif
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  sweep();
  #if defined(printgcs)
  pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
  #endif
}

// Compact image

void movepointer (object *from, object *to) {
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    unsigned int type = (obj->type) & ~MARKBIT;
    if (marked(obj) && (type >= STRING || type==ZERO)) {
      if (car(obj) == (object *)((uintptr_t)from | MARKBIT))
        car(obj) = (object *)((uintptr_t)to | MARKBIT);
      if (cdr(obj) == from) cdr(obj) = to;
    }
  }
  // Fix strings
  for (int i=0; i<WORKSPACESIZE; i++) {
    object *obj = &Workspace[i];
    if (marked(obj) && ((obj->type) & ~MARKBIT) == STRING) {
      obj = cdr(obj);
      while (obj != NULL) {
        if (cdr(obj) == to) cdr(obj) = from;
        obj = (object *)((uintptr_t)(car(obj)) & ~MARKBIT);
      }
    }
  }
}

int compactimage (object **arg) {
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  object *firstfree = Workspace;
  while (marked(firstfree)) firstfree++;
  object *obj = &Workspace[WORKSPACESIZE-1];
  while (firstfree < obj) {
    if (marked(obj)) {
      car(firstfree) = car(obj);
      cdr(firstfree) = cdr(obj);
      unmark(obj);
      movepointer(obj, firstfree);
      if (GlobalEnv == obj) GlobalEnv = firstfree;
      if (GCStack == obj) GCStack = firstfree;
      if (*arg == obj) *arg = firstfree;
      while (marked(firstfree)) firstfree++;
    }
    obj--;
  }
  sweep();
  return firstfree - Workspace;
}

// Make SD card filename

char *MakeFilename (object *arg) {
  char *buffer = SymbolTop;
  int max = maxbuffer(buffer);
  int i = 0;
  do {
    char c = nthchar(arg, i);
    if (c == '\0') break;
    buffer[i++] = c;
  } while (i<max);
  buffer[i] = '\0';
  return buffer;
}

// Save-image and load-image

#if defined(sdcardsupport)
void SDWriteInt (File file, int data) {
  file.write(data & 0xFF); file.write(data>>8 & 0xFF);
  file.write(data>>16 & 0xFF); file.write(data>>24 & 0xFF);
}
#elif defined(DATAFLASHSIZE)
// Winbond DataFlash support for Adafruit M4 Express boards
#define PAGEPROG      0x02
#define READSTATUS    0x05
#define READDATA      0x03
#define WRITEENABLE   0x06
#define BLOCK64K      0xD8
#define READID        0x90

// Arduino pins used for dataflash
#if defined(ARDUINO_ITSYBITSY_M0)
const int sck = 38, ssel = 39, mosi = 37, miso = 36;
#elif defined(ARDUINO_ITSYBITSY_M4)
const int sck = 32, ssel = 33, mosi = 34, miso = 35;
#elif defined(ARDUINO_METRO_M4)
const int sck = 41, ssel = 42, mosi = 43, miso = 44;
#elif defined(ARDUINO_FEATHER_M4)
const int sck = 34, ssel = 35, mosi = 36, miso = 37;
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
const int sck = 89, ssel = 90, mosi = 91, miso = 92;
#endif

boolean FlashSetup () {
  uint8_t manID, devID;
  digitalWrite(ssel, HIGH); pinMode(ssel, OUTPUT);
  pinMode(sck, OUTPUT);
  pinMode(mosi, OUTPUT);
  pinMode(miso, INPUT);
  digitalWrite(sck, LOW); digitalWrite(mosi, HIGH);
  digitalWrite(ssel, LOW);
  FlashWrite(READID);
  for(uint8_t i=0; i<4; i++) manID = FlashRead();
  devID = FlashRead();
  digitalWrite(ssel, HIGH);
  return (devID == 0x14 || devID == 0x16); // Found correct device
}

inline void FlashWrite (uint8_t data) {
  shiftOut(mosi, sck, MSBFIRST, data);
}

void FlashBusy () {
  digitalWrite(ssel, 0);
  FlashWrite(READSTATUS);
  while (FlashRead() & 1 != 0);
  digitalWrite(ssel, 1);
}

void FlashWriteEnable () {
  digitalWrite(ssel, 0);
  FlashWrite(WRITEENABLE);
  digitalWrite(ssel, 1);
}

void FlashBeginRead () {
  FlashBusy();
  digitalWrite(ssel, 0);
  FlashWrite(READDATA);
  FlashWrite(0); FlashWrite(0); FlashWrite(0);
}

inline uint8_t FlashRead () {
  int data;
  return shiftIn(miso, sck, MSBFIRST);
}

inline void FlashEndRead(void) {
  digitalWrite(ssel, 1);
}

void FlashBeginWrite (int blocks) {
  FlashBusy();
  // Erase 64K
  FlashWriteEnable();
  for (int b=0; b<blocks; b++) {
    digitalWrite(ssel, 0);
    FlashWrite(BLOCK64K);
    FlashWrite(0); FlashWrite(b); FlashWrite(0);
    digitalWrite(ssel, 1);
    FlashBusy();
  }
}

inline uint8_t FlashReadByte () {
  return FlashRead();
}

void FlashWriteByte (unsigned long *addr, uint8_t data) {
  // New page
  if (((*addr) & 0xFF) == 0) {
    digitalWrite(ssel, 1);
    FlashBusy();
    FlashWriteEnable();
    digitalWrite(ssel, 0);
    FlashWrite(PAGEPROG);
    FlashWrite((*addr)>>16);
    FlashWrite((*addr)>>8);
    FlashWrite(0);
  }
  FlashWrite(data);
  (*addr)++;
}

inline void FlashEndWrite (void) {
  digitalWrite(ssel, 1);
}

void FlashWriteInt (unsigned long *addr, int data) {
  FlashWriteByte(addr, data & 0xFF); FlashWriteByte(addr, data>>8 & 0xFF);
  FlashWriteByte(addr, data>>16 & 0xFF); FlashWriteByte(addr, data>>24 & 0xFF);
}
#endif

int saveimage (object *arg) {
  unsigned int imagesize = compactimage(&arg);
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) {
    file = SD.open(MakeFilename(arg), O_RDWR | O_CREAT | O_TRUNC);
    arg = NULL;
  } else if (arg == NULL || listp(arg)) file = SD.open("ULISP.IMG", O_RDWR | O_CREAT | O_TRUNC);
  else error(SAVEIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(SAVEIMAGE, PSTR("problem saving to SD card"));
  SDWriteInt(file, (uintptr_t)arg);
  SDWriteInt(file, imagesize);
  SDWriteInt(file, (uintptr_t)GlobalEnv);
  SDWriteInt(file, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SDWriteInt(file, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) file.write(SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    SDWriteInt(file, (uintptr_t)car(obj));
    SDWriteInt(file, (uintptr_t)cdr(obj));
  }
  file.close();
  return imagesize;
#elif defined(DATAFLASHSIZE)
  if (!(arg == NULL || listp(arg))) error(SAVEIMAGE, PSTR("illegal argument"), arg);
  if (!FlashSetup()) error2(SAVEIMAGE, PSTR("no DataFlash found."));
  // Save to DataFlash
  int bytesneeded = imagesize*8 + SYMBOLTABLESIZE + 20;
  if (bytesneeded > DATAFLASHSIZE) error(SAVEIMAGE, PSTR("image size too large"), number(imagesize));
  unsigned long addr = 0;
  FlashBeginWrite((bytesneeded+65535)/65536);
  FlashWriteInt(&addr, (uintptr_t)arg);
  FlashWriteInt(&addr, imagesize);
  FlashWriteInt(&addr, (uintptr_t)GlobalEnv);
  FlashWriteInt(&addr, (uintptr_t)GCStack);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  FlashWriteInt(&addr, (uintptr_t)SymbolTop);
  for (int i=0; i<SYMBOLTABLESIZE; i++) FlashWriteByte(&addr, SymbolTable[i]);
  #endif
  for (unsigned int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    FlashWriteInt(&addr, (uintptr_t)car(obj));
    FlashWriteInt(&addr, (uintptr_t)cdr(obj));
  }
  FlashEndWrite();
  return imagesize;
#else
  (void) arg;
  error2(SAVEIMAGE, PSTR("not available"));
  return 0;
#endif
}

#if defined(sdcardsupport)
int SDReadInt (File file) {
  uintptr_t b0 = file.read(); uintptr_t b1 = file.read();
  uintptr_t b2 = file.read(); uintptr_t b3 = file.read();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#elif defined(DATAFLASHSIZE)
int FlashReadInt () {
  uint8_t b0 = FlashReadByte(); uint8_t b1 = FlashReadByte();
  uint8_t b2 = FlashReadByte(); uint8_t b3 = FlashReadByte();
  return b0 | b1<<8 | b2<<16 | b3<<24;
}
#endif

int loadimage (object *arg) {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file;
  if (stringp(arg)) file = SD.open(MakeFilename(arg));
  else if (arg == NULL) file = SD.open("/ULISP.IMG");
  else error(LOADIMAGE, PSTR("illegal argument"), arg);
  if (!file) error2(LOADIMAGE, PSTR("problem loading from SD card"));
  SDReadInt(file);
  int imagesize = SDReadInt(file);
  GlobalEnv = (object *)SDReadInt(file);
  GCStack = (object *)SDReadInt(file);
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)SDReadInt(file);
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = file.read();
  #endif
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)SDReadInt(file);
    cdr(obj) = (object *)SDReadInt(file);
  }
  file.close();
  gc(NULL, NULL);
  return imagesize;
#elif defined(DATAFLASHSIZE)
  if (!FlashSetup()) error2(LOADIMAGE, PSTR("no DataFlash found."));
  FlashBeginRead();
  FlashReadInt(); // Skip eval address
  int imagesize = FlashReadInt();
  if (imagesize == 0 || imagesize == 0xFFFF) error2(LOADIMAGE, PSTR("no saved image"));
  GlobalEnv = (object *)FlashReadInt();
  GCStack = (object *)FlashReadInt();
  #if SYMBOLTABLESIZE > BUFFERSIZE
  SymbolTop = (char *)FlashReadInt();
  for (int i=0; i<SYMBOLTABLESIZE; i++) SymbolTable[i] = FlashReadByte();
  #endif
  for (int i=0; i<imagesize; i++) {
    object *obj = &Workspace[i];
    car(obj) = (object *)FlashReadInt();
    cdr(obj) = (object *)FlashReadInt();
  }
  gc(NULL, NULL);
  FlashEndRead();
  return imagesize;
#else
  (void) arg;
  error2(LOADIMAGE, PSTR("not available"));
  return 0;
#endif
}

void autorunimage () {
#if defined(sdcardsupport)
  SD.begin(SDCARD_SS_PIN);
  File file = SD.open("ULISP.IMG");
  if (!file) error2(0, PSTR("problem autorunning from SD card"));
  object *autorun = (object *)SDReadInt(file);
  file.close();
  if (autorun != NULL) {
    loadimage(NULL);
    apply(0, autorun, NULL, NULL);
  }
#elif defined(DATAFLASHSIZE)
  if (!FlashSetup()) error2(0, PSTR("no DataFlash found."));
  FlashBeginRead();
  object *autorun = (object *)FlashReadInt();
  FlashEndRead();
  if (autorun != NULL && (unsigned int)autorun != 0xFFFF) {
    loadimage(nil);
    apply(0, autorun, NULL, NULL);
  }
#else
  error2(0, PSTR("autorun not available"));
#endif
}

// Error handling

void errorsub (symbol_t fname, PGM_P string) {
  pfl(pserial); pfstring(PSTR("Error: "), pserial);
  if (fname) {
    pserial('\'');
    pstring(symbolname(fname), pserial);
    pfstring(PSTR("' "), pserial);
  }
  pfstring(string, pserial);
}

void error (symbol_t fname, PGM_P string, object *symbol) {
  errorsub(fname, string);
  pfstring(PSTR(": "), pserial); printobject(symbol, pserial);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

void error2 (symbol_t fname, PGM_P string) {
  errorsub(fname, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

// Save space as these are used multiple times
const char notanumber[] PROGMEM = "argument is not a number";
const char notastring[] PROGMEM = "argument is not a string";
const char notalist[] PROGMEM = "argument is not a list";
const char notproper[] PROGMEM = "argument is not a proper list";
const char noargument[] PROGMEM = "missing argument";
const char nostream[] PROGMEM = "missing stream argument";
const char overflow[] PROGMEM = "arithmetic overflow";
const char invalidpin[] PROGMEM = "invalid pin";
const char resultproper[] PROGMEM = "result is not a proper list";

// Tracing

boolean tracing (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) return i+1;
    i++;
  }
  return 0;
}

void trace (symbol_t name) {
  if (tracing(name)) error(TRACE, PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(TRACE, PSTR("already tracing 3 functions"));
}

void untrace (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) { TraceFn[i] = 0; return; }
    i++;
  }
  error(UNTRACE, PSTR("not tracing"), symbol(name));
}

// Helper functions

boolean consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZERO;
}

boolean atom (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type < PAIR && type != ZERO;
}

boolean listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZERO;
}

boolean improperp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type < PAIR && type != ZERO;
}

int toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+30;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+1;
  return -1; // Invalid
}

int fromradix40 (int n) {
  if (n >= 1 && n <= 26) return 'a'+n-1;
  if (n >= 30 && n <= 39) return '0'+n-30;
  return 0;
}

int pack40 (char *buffer) {
  return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}

boolean valid40 (char *buffer) {
 return (toradix40(buffer[0]) >= 0 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}

int digitvalue (char d) {
  if (d>='0' && d<='9') return d-'0';
  d = d | 0x20;
  if (d>='a' && d<='f') return d-'a'+10;
  return 16;
}

char *symbolname (symbol_t x) {
  if (x < ENDFUNCTIONS) return lookupbuiltin(x);
  else if (x >= 64000) return lookupsymbol(x);
  char *buffer = SymbolTop;
  buffer[3] = '\0';
  for (int n=2; n>=0; n--) {
    buffer[n] = fromradix40(x % 40);
    x = x / 40;
  }
  return buffer;
}

int checkinteger (symbol_t name, object *obj) {
  if (!integerp(obj)) error(name, PSTR("argument is not an integer"), obj);
  return obj->integer;
}

float checkintfloat (symbol_t name, object *obj){
  if (integerp(obj)) return obj->integer;
  if (floatp(obj)) return obj->single_float;
  error(name, notanumber, obj);
}

int checkchar (symbol_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return obj->integer;
}

int isstream (object *obj){
  if (!streamp(obj)) error(0, PSTR("not a stream"), obj);
  return obj->integer;
}

int issymbol (object *obj, symbol_t n) {
  return symbolp(obj) && obj->name == n;
}

void checkargs (symbol_t name, object *args) {
  int nargs = listlength(name, args);
  if (name >= ENDFUNCTIONS) error(0, PSTR("not valid here"), symbol(name));
  if (nargs<lookupmin(name)) error2(name, PSTR("has too few arguments"));
  if (nargs>lookupmax(name)) error2(name, PSTR("has too many arguments"));
}

int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}

int equal (object *arg1, object *arg2) {
  if (eq(arg1, arg2)) return true;
  if (stringp(arg1) && stringp(arg2)) {
    object *args = cons(arg1, cons(arg2, nil));
    return stringcompare(EQUAL, args, false, false, true);
  }
  if (consp(arg1) && consp(arg2)) {
    while (arg1 != nil && arg2 != nil) {
      if (!equal(car(arg1), car(arg2))) return false;
      arg1 = cdr(arg1);
      arg2 = cdr(arg2);
    }
    return arg1 == nil && arg2 == nil;
  }
  return false;
}

int listlength (symbol_t name, object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(name, notproper);
    list = cdr(list);
    length++;
  }
  return length;
}

// Association lists

object *assoc (object *key, object *list) {
  while (list != NULL) {
    if (improperp(list)) error(ASSOC, notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(ASSOC, PSTR("element is not a list"), pair);
    if (pair != NULL && eq(key,car(pair))) return pair;
    list = cdr(list);
  }
  return nil;
}

object *delassoc (object *key, object **alist) {
  object *list = *alist;
  object *prev = NULL;
  while (list != NULL) {
    object *pair = first(list);
    if (eq(key,car(pair))) {
      if (prev == NULL) *alist = cdr(list);
      else cdr(prev) = cdr(list);
      return key;
    }
    prev = list;
    list = cdr(list);
  }
  return nil;
}

// String utilities

void indent (int spaces, pfun_t pfun) {
  for (int i=0; i<spaces; i++) pfun(' ');
}

void buildstring (char ch, int *chars, object **head) {
  static object* tail;
  static uint8_t shift;
  if (*chars == 0) {
    shift = (sizeof(int)-1)*8;
    *chars = ch<<shift;
    object *cell = myalloc();
    if (*head == NULL) *head = cell; else tail->car = cell;
    cell->car = NULL;
    cell->integer = *chars;
    tail = cell;
  } else {
    shift = shift - 8;
    *chars = *chars | ch<<shift;
    tail->integer = *chars;
    if (shift == 0) *chars = 0;
  }
}

object *readstring (char delim, gfun_t gfun) {
  object *obj = myalloc();
  obj->type = STRING;
  int ch = gfun();
  if (ch == -1) return nil;
  object *head = NULL;
  int chars = 0;
  while ((ch != delim) && (ch != -1)) {
    if (ch == '\\') ch = gfun();
    buildstring(ch, &chars, &head);
    ch = gfun();
  }
  obj->cdr = head;
  return obj;
}

int stringlength (object *form) {
  int length = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      if (chars>>i & 0xFF) length++;
    }
    form = car(form);
  }
  return length;
}

char nthchar (object *string, int n) {
  object *arg = cdr(string);
  int top;
  if (sizeof(int) == 4) { top = n>>2; n = 3 - (n&3); }
  else { top = n>>1; n = 1 - (n&1); }
  for (int i=0; i<top; i++) {
    if (arg == NULL) return 0;
    arg = car(arg);
  }
  if (arg == NULL) return 0;
  return (arg->integer)>>(n*8) & 0xFF;
}

// Lookup variable in environment

object *value (symbol_t n, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair)->name == n) return pair;
    env = cdr(env);
  }
  return nil;
}

object *findvalue (object *var, object *env) {
  symbol_t varname = var->name;
  object *pair = value(varname, env);
  if (pair == NULL) pair = value(varname, GlobalEnv);
  if (pair == NULL) error(0, PSTR("unknown variable"), var);
  return pair;
}

// Handling closures

object *closure (int tc, symbol_t name, object *state, object *function, object *args, object **env) {
  int trace = 0;
  if (name) trace = tracing(name);
  if (trace) {
    indent(TraceDepth[trace-1]<<1, pserial);
    pint(TraceDepth[trace-1]++, pserial);
    pserial(':'); pserial(' '); pserial('('); pstring(symbolname(name), pserial);
  }
  object *params = first(function);
  function = cdr(function);
  // Dropframe
  if (tc) {
    if (*env != NULL && car(*env) == NULL) {
      pop(*env);
      while (*env != NULL && car(*env) != NULL) pop(*env);
    } else push(nil, *env);
  }
  // Push state
  while (state != NULL) {
    object *pair = first(state);
    push(pair, *env);
    state = cdr(state);
  }
  // Add arguments to environment
  boolean optional = false;
  while (params != NULL) {
    object *value;
    object *var = first(params);
    if (symbolp(var) && var->name == OPTIONAL) optional = true;
    else {
      if (consp(var)) {
        if (!optional) error(name, PSTR("invalid default value"), var);
        if (args == NULL) value = eval(second(var), *env);
        else { value = first(args); args = cdr(args); }
        var = first(var);
        if (!symbolp(var)) error(name, PSTR("illegal optional parameter"), var);
      } else if (!symbolp(var)) {
        error2(name, PSTR("illegal parameter"));
      } else if (var->name == AMPREST) {
        params = cdr(params);
        var = first(params);
        value = args;
        args = NULL;
      } else {
        if (args == NULL) {
          if (optional) value = nil;
          else {
            if (name) error2(name, PSTR("has too few arguments"));
            else error2(0, PSTR("function has too few arguments"));
          }
        } else { value = first(args); args = cdr(args); }
      }
      push(cons(var,value), *env);
      if (trace) { pserial(' '); printobject(value, pserial); }
    }
    params = cdr(params);
  }
  if (args != NULL) {
    if (name) error2(name, PSTR("has too many arguments"));
    else error2(0, PSTR("function has too many arguments"));
  }
  if (trace) { pserial(')'); pln(pserial); }
  // Do an implicit progn
  if (tc) push(nil, *env);
  return tf_progn(function, *env);
}

object *apply (symbol_t name, object *function, object *args, object *env) {
  if (symbolp(function)) {
    symbol_t fname = function->name;
    checkargs(fname, args);
    return ((fn_ptr_type)lookupfn(fname))(args, env);
  }
  if (consp(function) && issymbol(car(function), LAMBDA)) {
    function = cdr(function);
    object *result = closure(0, 0, NULL, function, args, &env);
    return eval(result, env);
  }
  if (consp(function) && issymbol(car(function), CLOSURE)) {
    function = cdr(function);
    object *result = closure(0, 0, car(function), cdr(function), args, &env);
    return eval(result, env);
  }
  error(name, PSTR("illegal function"), function);
  return NULL;
}

// In-place operations

object **place (symbol_t name, object *args, object *env) {
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (issymbol(function, CAR) || issymbol(function, FIRST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take car"), value);
    return &car(value);
  }
  if (issymbol(function, CDR) || issymbol(function, REST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take cdr"), value);
    return &cdr(value);
  }
  if (issymbol(function, NTH)) {
    int index = checkinteger(NTH, eval(second(args), env));
    object *list = eval(third(args), env);
    if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
    while (index > 0) {
      list = cdr(list);
      if (list == NULL) error2(name, PSTR("index to nth is out of range"));
      index--;
    }
    return &car(list);
  }
  error2(name, PSTR("illegal place"));
  return nil;
}

// Checked car and cdr

inline object *carx (object *arg) {
  if (!listp(arg)) error(0, PSTR("Can't take car"), arg);
  if (arg == nil) return nil;
  return car(arg);
}

inline object *cdrx (object *arg) {
  if (!listp(arg)) error(0, PSTR("Can't take cdr"), arg);
  if (arg == nil) return nil;
  return cdr(arg);
}

// I2C interface

void I2Cinit (bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

inline uint8_t I2Cread () {
  return Wire.read();
}

inline bool I2Cwrite (uint8_t data) {
  return Wire.write(data);
}

bool I2Cstart (uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   Wire.beginTransmission(address);
   ok = (Wire.endTransmission(true) == 0);
   Wire.beginTransmission(address);
 }
 else Wire.requestFrom(address, I2CCount);
 return ok;
}

bool I2Crestart (uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(false) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return error ? false : true;
}

void I2Cstop (uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}

// Streams

inline int spiread () { return SPI.transfer(0); }
#if defined(ARDUINO_SAM_DUE)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
inline int serial2read () { while (!Serial2.available()) testescape(); return Serial2.read(); }
inline int serial3read () { while (!Serial3.available()) testescape(); return Serial3.read(); }
#elif !defined(_VARIANT_BBC_MICROBIT_)
inline int serial1read () { while (!Serial1.available()) testescape(); return Serial1.read(); }
#endif
#if defined(sdcardsupport)
File SDpfile, SDgfile;
inline int SDread () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  return SDgfile.read();
}
#endif

void serialbegin (int address, int baud) {
  #if defined(ARDUINO_SAM_DUE)
  if (address == 1) Serial1.begin((long)baud*100);
  else if (address == 2) Serial2.begin((long)baud*100);
  else if (address == 3) Serial3.begin((long)baud*100);
  #elif !defined(_VARIANT_BBC_MICROBIT_)
  if (address == 1) Serial1.begin((long)baud*100);
  #endif
  else error(WITHSERIAL, PSTR("port not supported"), number(address));
}

void serialend (int address) {
  #if defined(ARDUINO_SAM_DUE)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  else if (address == 2) {Serial2.flush(); Serial2.end(); }
  else if (address == 3) {Serial3.flush(); Serial3.end(); }
  #elif !defined(_VARIANT_BBC_MICROBIT_)
  if (address == 1) {Serial1.flush(); Serial1.end(); }
  #endif
}

gfun_t gstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  gfun_t gfun = gserial;
  if (args != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) gfun = (gfun_t)I2Cread;
  else if (streamtype == SPISTREAM) gfun = spiread;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) gfun = gserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) gfun = serial1read;
    else if (address == 2) gfun = serial2read;
    else if (address == 3) gfun = serial3read;
    #elif !defined(_VARIANT_BBC_MICROBIT_)
    else if (address == 1) gfun = serial1read;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) gfun = (gfun_t)SDread;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return gfun;
}

inline void spiwrite (char c) { SPI.transfer(c); }
#if defined(ARDUINO_SAM_DUE)
inline void serial1write (char c) { Serial1.write(c); }
inline void serial2write (char c) { Serial2.write(c); }
inline void serial3write (char c) { Serial3.write(c); }
#elif !defined(_VARIANT_BBC_MICROBIT_)
inline void serial1write (char c) { Serial1.write(c); }
#endif
#if defined(sdcardsupport)
inline void SDwrite (char c) { SDpfile.write(c); }
#endif

pfun_t pstreamfun (object *args) {
  int streamtype = SERIALSTREAM;
  int address = 0;
  pfun_t pfun = pserial;
  if (args != NULL && first(args) != NULL) {
    int stream = isstream(first(args));
    streamtype = stream>>8; address = stream & 0xFF;
  }
  if (streamtype == I2CSTREAM) pfun = (pfun_t)I2Cwrite;
  else if (streamtype == SPISTREAM) pfun = spiwrite;
  else if (streamtype == SERIALSTREAM) {
    if (address == 0) pfun = pserial;
    #if defined(ARDUINO_SAM_DUE)
    else if (address == 1) pfun = serial1write;
    else if (address == 2) pfun = serial2write;
    else if (address == 3) pfun = serial3write;
    #elif !defined(_VARIANT_BBC_MICROBIT_)
    else if (address == 1) pfun = serial1write;
    #endif
  }
  #if defined(sdcardsupport)
  else if (streamtype == SDSTREAM) pfun = (pfun_t)SDwrite;
  #endif
  else error2(0, PSTR("unknown stream type"));
  return pfun;
}

// Check pins

void checkanalogread (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!(pin>=54 && pin<=65)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!(pin>=15 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!(pin>=14 && pin<=25)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin>=8 && pin<=10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=14 && pin<=21)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin>=14 && pin<=19)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=67 && pin<=74) || (pin>=54 && pin<=61))) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(_VARIANT_BBC_MICROBIT_)
  if (!((pin>=0 && pin<=4) || pin==10)) error(ANALOGREAD, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!(pin>=49 && pin<=52)) error(ANALOGREAD, invalidpin, number(pin));
#endif
}

void checkanalogwrite (int pin) {
#if defined(ARDUINO_SAM_DUE)
  if (!((pin>=2 && pin<=13) || pin==66 || pin==67)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_ZERO)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || pin==14)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_SAMD_MKRZERO)
  if (!((pin>=0 && pin<=8) || pin==10 || pin==18 || pin==19)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M0)
  if (!((pin>=3 && pin<=6) || (pin>=8 && pin<=13) || (pin>=15 && pin<=16) || (pin>=22 && pin<=25))) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GEMMA_M0)
  if (!(pin==0 || pin==2 || pin==9 || pin==10)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_METRO_M4)
  if (!(pin>=0 && pin<=15)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_ITSYBITSY_M4)
  if (!(pin==0 || pin==1 || pin==4 || pin==5 || pin==7 || (pin>=9 && pin<=15) || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_FEATHER_M4)
  if (!(pin==0 || pin==1 || (pin>=4 && pin<=6) || (pin>=9 && pin<=13) || pin==14 || pin==15 || pin==17 || pin==21 || pin==22)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(ARDUINO_GRAND_CENTRAL_M4)
  if (!((pin>=2 && pin<=9) || pin==11 || (pin>=13 && pin<=45) || pin==48 || (pin>=50 && pin<=53) || pin==58 || pin==61 || pin==68 || pin==69)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(_VARIANT_BBC_MICROBIT_)
  if (!(pin>=0 && pin<=2)) error(ANALOGWRITE, invalidpin, number(pin));
#elif defined(MAX32620)
  if (!((pin>=20 && pin<=29) || pin==32 || (pin>=40 && pin<=48))) error(ANALOGWRITE, invalidpin, number(pin));
#endif
}

// Note

void tone (int pin, int note) {
  (void) pin, (void) note;
}

void noTone (int pin) {
  (void) pin;
}

const int scale[] PROGMEM = {4186,4435,4699,4978,5274,5588,5920,6272,6645,7040,7459,7902};

void playnote (int pin, int note, int octave) {
  int prescaler = 8 - octave - note/12;
  if (prescaler<0 || prescaler>8) error(NOTE, PSTR("octave out of range"), number(prescaler));
  tone(pin, scale[note%12]>>prescaler);
}

void nonote (int pin) {
  noTone(pin);
}

// Sleep

#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
void WDT_Handler(void) {
  // ISR for watchdog early warning
  WDT->CTRL.bit.ENABLE = 0;        // Disable watchdog
  while(WDT->STATUS.bit.SYNCBUSY); // Sync CTRL write
  WDT->INTFLAG.bit.EW  = 1;        // Clear interrupt flag
}
#endif

void initsleep () {
#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
 // One-time initialization of watchdog timer.

  // Generic clock generator 2, divisor = 32 (2^(DIV+1))
  GCLK->GENDIV.reg = GCLK_GENDIV_ID(2) | GCLK_GENDIV_DIV(4);
  // Enable clock generator 2 using low-power 32KHz oscillator.
  // With /32 divisor above, this yields 1024Hz clock.
  GCLK->GENCTRL.reg = GCLK_GENCTRL_ID(2) |
                      GCLK_GENCTRL_GENEN |
                      GCLK_GENCTRL_SRC_OSCULP32K |
                      GCLK_GENCTRL_DIVSEL;
  while(GCLK->STATUS.bit.SYNCBUSY);
  // WDT clock = clock gen 2
  GCLK->CLKCTRL.reg = GCLK_CLKCTRL_ID_WDT |
                      GCLK_CLKCTRL_CLKEN |
                      GCLK_CLKCTRL_GEN_GCLK2;

  // Enable WDT early-warning interrupt
  NVIC_DisableIRQ(WDT_IRQn);
  NVIC_ClearPendingIRQ(WDT_IRQn);
  NVIC_SetPriority(WDT_IRQn, 0);         // Top priority
  NVIC_EnableIRQ(WDT_IRQn);
#endif
}

void sleep (int secs) {
#if defined(ARDUINO_SAMD_ZERO) || defined(ARDUINO_SAMD_MKRZERO)
  WDT->CTRL.reg = 0;                     // Disable watchdog for config
  while(WDT->STATUS.bit.SYNCBUSY);
  WDT->INTENSET.bit.EW   = 1;            // Enable early warning interrupt
  WDT->CONFIG.bit.PER    = 0xB;          // Period = max
  WDT->CONFIG.bit.WINDOW = 0x7;          // Set time of interrupt = 1024 cycles = 1 sec
  WDT->CTRL.bit.WEN      = 1;            // Enable window mode
  while(WDT->STATUS.bit.SYNCBUSY);       // Sync CTRL write

  SysTick->CTRL = 0;                     // Stop SysTick interrupts

  while (secs > 0) {
    WDT->CLEAR.reg = WDT_CLEAR_CLEAR_KEY;// Clear watchdog interval
    while(WDT->STATUS.bit.SYNCBUSY);
    WDT->CTRL.bit.ENABLE = 1;            // Start watchdog now!
    while(WDT->STATUS.bit.SYNCBUSY);
    SCB->SCR |= SCB_SCR_SLEEPDEEP_Msk;   // Deepest sleep
    __DSB();
    __WFI();                             // Wait for interrupt
    secs--;
  }
  SysTick->CTRL = 7;                     // Restart SysTick interrupts
#else
  delay(1000*secs);
#endif
}

// Special forms

object *sp_quote (object *args, object *env) {
  (void) env;
  checkargs(QUOTE, args);
  return first(args);
}

object *sp_defun (object *args, object *env) {
  (void) env;
  checkargs(DEFUN, args);
  object *var = first(args);
  if (var->type != SYMBOL) error(DEFUN, PSTR("not a symbol"), var);
  object *val = cons(symbol(LAMBDA), cdr(args));
  object *pair = value(var->name,GlobalEnv);
  if (pair != NULL) { cdr(pair) = val; return var; }
  push(cons(var, val), GlobalEnv);
  return var;
}

object *sp_defvar (object *args, object *env) {
  checkargs(DEFVAR, args);
  object *var = first(args);
  if (var->type != SYMBOL) error(DEFVAR, PSTR("not a symbol"), var);
  object *val = NULL;
  val = eval(second(args), env);
  object *pair = value(var->name, GlobalEnv);
  if (pair != NULL) { cdr(pair) = val; return var; }
  push(cons(var, val), GlobalEnv);
  return var;
}

object *sp_setq (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETQ, PSTR("odd number of parameters"));
    object *pair = findvalue(first(args), env);
    arg = eval(second(args), env);
    cdr(pair) = arg;
    args = cddr(args);
  }
  return arg;
}

object *sp_loop (object *args, object *env) {
  object *start = args;
  for (;;) {
    args = start;
    while (args != NULL) {
      object *result = eval(car(args),env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      args = cdr(args);
    }
  }
}

object *sp_return (object *args, object *env) {
  object *result = eval(tf_progn(args,env), env);
  setflag(RETURNFLAG);
  return result;
}

object *sp_push (object *args, object *env) {
  checkargs(PUSH, args);
  object *item = eval(first(args), env);
  object **loc = place(PUSH, second(args), env);
  push(item, *loc);
  return *loc;
}

object *sp_pop (object *args, object *env) {
  checkargs(POP, args);
  object **loc = place(POP, first(args), env);
  object *result = car(*loc);
  pop(*loc);
  return result;
}

// Special forms incf/decf

object *sp_incf (object *args, object *env) {
  checkargs(INCF, args);
  object **loc = place(INCF, first(args), env);
  args = cdr(args);

  object *x = *loc;
  object *inc = (args != NULL) ? eval(first(args), env) : NULL;

  if (floatp(x) || floatp(inc)) {
    float increment;
    float value = checkintfloat(INCF, x);

    if (inc == NULL) increment = 1.0;
    else increment = checkintfloat(INCF, inc);

    *loc = makefloat(value + increment);
  } else if (integerp(x) && (integerp(inc) || inc == NULL)) {
    int increment;
    int value = x->integer;

    if (inc == NULL) increment = 1;
    else increment = inc->integer;

    if (increment < 1) {
      if (INT_MIN - increment > value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    } else {
      if (INT_MAX - increment < value) *loc = makefloat((float)value + (float)increment);
      else *loc = number(value + increment);
    }
  } else error2(INCF, notanumber);
  return *loc;
}

object *sp_decf (object *args, object *env) {
  checkargs(DECF, args);
  object **loc = place(DECF, first(args), env);
  args = cdr(args);

  object *x = *loc;
  object *dec = (args != NULL) ? eval(first(args), env) : NULL;

  if (floatp(x) || floatp(dec)) {
    float decrement;
    float value = checkintfloat(DECF, x);

    if (dec == NULL) decrement = 1.0;
    else decrement = checkintfloat(DECF, dec);

    *loc = makefloat(value - decrement);
  } if (integerp(x) && (integerp(dec) || dec == NULL)) {
    int decrement;
    int value = x->integer;

    if (dec == NULL) decrement = 1;
    else decrement = dec->integer;

    if (decrement < 1) {
      if (INT_MAX + decrement < value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    } else {
      if (INT_MIN + decrement > value) *loc = makefloat((float)value - (float)decrement);
      else *loc = number(value - decrement);
    }
  } else error2(DECF, notanumber);
  return *loc;
}

object *sp_setf (object *args, object *env) {
  object *arg = nil;
  while (args != NULL) {
    if (cdr(args) == NULL) error2(SETF, PSTR("odd number of parameters"));
    object **loc = place(SETF, first(args), env);
    arg = eval(second(args), env);
    *loc = arg;
    args = cddr(args);
  }
  return arg;
}

object *sp_dolist (object *args, object *env) {
  if (args == NULL) error2(DOLIST, noargument);
  object *params = first(args);
  object *var = first(params);
  object *list = eval(second(params), env);
  push(list, GCStack); // Don't GC the list
  object *pair = cons(var,nil);
  push(pair,env);
  params = cdr(cdr(params));
  args = cdr(args);
  while (list != NULL) {
    if (improperp(list)) error(DOLIST, notproper, list);
    cdr(pair) = first(list);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        pop(GCStack);
        return result;
      }
      forms = cdr(forms);
    }
    list = cdr(list);
  }
  cdr(pair) = nil;
  pop(GCStack);
  if (params == NULL) return nil;
  return eval(car(params), env);
}

object *sp_dotimes (object *args, object *env) {
  if (args == NULL) error2(DOTIMES, noargument);
  object *params = first(args);
  object *var = first(params);
  int count = checkinteger(DOTIMES, eval(second(params), env));
  int index = 0;
  params = cdr(cdr(params));
  object *pair = cons(var,number(0));
  push(pair,env);
  args = cdr(args);
  while (index < count) {
    cdr(pair) = number(index);
    object *forms = args;
    while (forms != NULL) {
      object *result = eval(car(forms), env);
      if (tstflag(RETURNFLAG)) {
        clrflag(RETURNFLAG);
        return result;
      }
      forms = cdr(forms);
    }
    index++;
  }
  cdr(pair) = number(index);
  if (params == NULL) return nil;
  return eval(car(params), env);
}

object *sp_trace (object *args, object *env) {
  (void) env;
  while (args != NULL) {
      trace(first(args)->name);
      args = cdr(args);
  }
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
    i++;
  }
  return args;
}

object *sp_untrace (object *args, object *env) {
  (void) env;
  if (args == NULL) {
    int i = 0;
    while (i < TRACEMAX) {
      if (TraceFn[i] != 0) args = cons(symbol(TraceFn[i]), args);
      TraceFn[i] = 0;
      i++;
    }
  } else {
    while (args != NULL) {
      untrace(first(args)->name);
      args = cdr(args);
    }
  }
  return args;
}

object *sp_formillis (object *args, object *env) {
  object *param = first(args);
  unsigned long start = millis();
  unsigned long now, total = 0;
  if (param != NULL) total = checkinteger(FORMILLIS, eval(first(param), env));
  eval(tf_progn(cdr(args),env), env);
  do {
    now = millis() - start;
    testescape();
  } while (now < total);
  if (now <= INT_MAX) return number(now);
  return nil;
}

object *sp_withserial (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSERIAL, nostream);
  object *var = first(params);
  int address = checkinteger(WITHSERIAL, eval(second(params), env));
  params = cddr(params);
  int baud = 96;
  if (params != NULL) baud = checkinteger(WITHSERIAL, eval(first(params), env));
  object *pair = cons(var, stream(SERIALSTREAM, address));
  push(pair,env);
  serialbegin(address, baud);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  serialend(address);
  return result;
}

object *sp_withi2c (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHI2C, nostream);
  object *var = first(params);
  int address = checkinteger(WITHI2C, eval(second(params), env));
  params = cddr(params);
  int read = 0; // Write
  I2CCount = 0;
  if (params != NULL) {
    object *rw = eval(first(params), env);
    if (integerp(rw)) I2CCount = rw->integer;
    read = (rw != NULL);
  }
  I2Cinit(1); // Pullups
  object *pair = cons(var, (I2Cstart(address, read)) ? stream(I2CSTREAM, address) : nil);
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  I2Cstop(read);
  return result;
}

object *sp_withspi (object *args, object *env) {
  object *params = first(args);
  if (params == NULL) error2(WITHSPI, nostream);
  object *var = first(params);
  params = cdr(params);
  if (params == NULL) error2(WITHSPI, nostream);
  int pin = checkinteger(WITHSPI, eval(car(params), env));
  pinMode(pin, OUTPUT);
  digitalWrite(pin, HIGH);
  params = cdr(params);
  int clock = 4000, mode = SPI_MODE0; // Defaults
  BitOrder bitorder = MSBFIRST;
  if (params != NULL) {
    clock = checkinteger(WITHSPI, eval(car(params), env));
    params = cdr(params);
    if (params != NULL) {
      bitorder = (checkinteger(WITHSPI, eval(car(params), env)) == 0) ? LSBFIRST : MSBFIRST;
      params = cdr(params);
      if (params != NULL) {
        int modeval = checkinteger(WITHSPI, eval(car(params), env));
        mode = (modeval == 3) ? SPI_MODE3 : (modeval == 2) ? SPI_MODE2 : (modeval == 1) ? SPI_MODE1 : SPI_MODE0;
      }
    }
  }
  object *pair = cons(var, stream(SPISTREAM, pin));
  push(pair,env);
  SPI.begin();
  SPI.beginTransaction(SPISettings(((unsigned long)clock * 1000), bitorder, mode));
  digitalWrite(pin, LOW);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  digitalWrite(pin, HIGH);
  SPI.endTransaction();
  return result;
}

object *sp_withsdcard (object *args, object *env) {
#if defined(sdcardsupport)
  object *params = first(args);
  if (params == NULL) error2(WITHSDCARD, nostream);
  object *var = first(params);
  object *filename = eval(second(params), env);
  params = cddr(params);
  SD.begin(SDCARD_SS_PIN);
  int mode = 0;
  if (params != NULL && first(params) != NULL) mode = checkinteger(WITHSDCARD, first(params));
  int oflag = O_READ;
  if (mode == 1) oflag = O_RDWR | O_CREAT | O_APPEND; else if (mode == 2) oflag = O_RDWR | O_CREAT | O_TRUNC;
  if (mode >= 1) {
    SDpfile = SD.open(MakeFilename(filename), oflag);
    if (!SDpfile) error2(WITHSDCARD, PSTR("problem writing to SD card"));
  } else {
    SDgfile = SD.open(MakeFilename(filename), oflag);
    if (!SDgfile) error2(WITHSDCARD, PSTR("problem reading from SD card"));
  }
  object *pair = cons(var, stream(SDSTREAM, 1));
  push(pair,env);
  object *forms = cdr(args);
  object *result = eval(tf_progn(forms,env), env);
  if (mode >= 1) SDpfile.close(); else SDgfile.close();
  return result;
#else
  (void) args, (void) env;
  error2(WITHSDCARD, PSTR("not supported"));
  return nil;
#endif
}

// Tail-recursive forms

object *tf_progn (object *args, object *env) {
  if (args == NULL) return nil;
  object *more = cdr(args);
  while (more != NULL) {
    object *result = eval(car(args),env);
    if (tstflag(RETURNFLAG)) return result;
    args = more;
    more = cdr(args);
  }
  return car(args);
}

object *tf_if (object *args, object *env) {
  if (args == NULL || cdr(args) == NULL) error2(IF, PSTR("missing argument(s)"));
  if (eval(first(args), env) != nil) return second(args);
  args = cddr(args);
  return (args != NULL) ? first(args) : nil;
}

object *tf_cond (object *args, object *env) {
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(COND, PSTR("illegal clause"), clause);
    object *test = eval(first(clause), env);
    object *forms = cdr(clause);
    if (test != nil) {
      if (forms == NULL) return test; else return tf_progn(forms, env);
    }
    args = cdr(args);
  }
  return nil;
}

object *tf_when (object *args, object *env) {
  if (args == NULL) error2(WHEN, noargument);
  if (eval(first(args), env) != nil) return tf_progn(cdr(args),env);
  else return nil;
}

object *tf_unless (object *args, object *env) {
  if (args == NULL) error2(UNLESS, noargument);
  if (eval(first(args), env) != nil) return nil;
  else return tf_progn(cdr(args),env);
}

object *tf_case (object *args, object *env) {
  object *test = eval(first(args), env);
  args = cdr(args);
  while (args != NULL) {
    object *clause = first(args);
    if (!consp(clause)) error(CASE, PSTR("illegal clause"), clause);
    object *key = car(clause);
    object *forms = cdr(clause);
    if (consp(key)) {
      while (key != NULL) {
        if (eq(test,car(key))) return tf_progn(forms, env);
        key = cdr(key);
      }
    } else if (eq(test,key) || eq(key,tee)) return tf_progn(forms, env);
    args = cdr(args);
  }
  return nil;
}

object *tf_and (object *args, object *env) {
  if (args == NULL) return tee;
  object *more = cdr(args);
  while (more != NULL) {
    if (eval(car(args), env) == NULL) return nil;
    args = more;
    more = cdr(args);
  }
  return car(args);
}

object *tf_or (object *args, object *env) {
  while (args != NULL) {
    if (eval(car(args), env) != NULL) return car(args);
    args = cdr(args);
  }
  return nil;
}

// Core functions

object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}

object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args), second(args));
}

object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}

object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}

object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}

object *fn_symbolp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return symbolp(arg) ? tee : nil;
}

object *fn_streamp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return streamp(arg) ? tee : nil;
}

object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}

object *fn_equal (object *args, object *env) {
  (void) env;
  return equal(first(args), second(args)) ? tee : nil;
}

// List functions

object *fn_car (object *args, object *env) {
  (void) env;
  return carx(first(args));
}

object *fn_cdr (object *args, object *env) {
  (void) env;
  return cdrx(first(args));
}

object *fn_caar (object *args, object *env) {
  (void) env;
  return carx(carx(first(args)));
}

object *fn_cadr (object *args, object *env) {
  (void) env;
  return carx(cdrx(first(args)));
}

object *fn_cdar (object *args, object *env) {
  (void) env;
  return cdrx(carx(first(args)));
}

object *fn_cddr (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(first(args)));
}

object *fn_caaar (object *args, object *env) {
  (void) env;
  return carx(carx(carx(first(args))));
}

object *fn_caadr (object *args, object *env) {
  (void) env;
  return carx(carx(cdrx(first(args))));
}

object *fn_cadar (object *args, object *env) {
  (void) env;
  return carx(cdrx(carx(first(args))));
}

object *fn_caddr (object *args, object *env) {
  (void) env;
  return carx(cdrx(cdrx(first(args))));
}

object *fn_cdaar (object *args, object *env) {
  (void) env;
  return cdrx(carx(carx(first(args))));
}

object *fn_cdadr (object *args, object *env) {
  (void) env;
  return cdrx(carx(cdrx(first(args))));
}

object *fn_cddar (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(carx(first(args))));
}

object *fn_cdddr (object *args, object *env) {
  (void) env;
  return cdrx(cdrx(cdrx(first(args))));
}

object *fn_length (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (listp(arg)) return number(listlength(LENGTH, arg));
  if (!stringp(arg)) error(LENGTH, PSTR("argument is not a list or string"), arg);
  return number(stringlength(arg));
}

object *fn_list (object *args, object *env) {
  (void) env;
  return args;
}

object *fn_reverse (object *args, object *env) {
  (void) env;
  object *list = first(args);
  object *result = NULL;
  while (list != NULL) {
    if (improperp(list)) error(REVERSE, notproper, list);
    push(first(list),result);
    list = cdr(list);
  }
  return result;
}

object *fn_nth (object *args, object *env) {
  (void) env;
  int n = checkinteger(NTH, first(args));
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(NTH, notproper, list);
    if (n == 0) return car(list);
    list = cdr(list);
    n--;
  }
  return nil;
}

object *fn_assoc (object *args, object *env) {
  (void) env;
  object *key = first(args);
  object *list = second(args);
  return assoc(key,list);
}

object *fn_member (object *args, object *env) {
  (void) env;
  object *item = first(args);
  object *list = second(args);
  while (list != NULL) {
    if (improperp(list)) error(MEMBER, notproper, list);
    if (eq(item,car(list))) return list;
    list = cdr(list);
  }
  return nil;
}

object *fn_apply (object *args, object *env) {
  object *previous = NULL;
  object *last = args;
  while (cdr(last) != NULL) {
    previous = last;
    last = cdr(last);
  }
  object *arg = car(last);
  if (!listp(arg)) error(APPLY, PSTR("last argument is not a list"), arg);
  cdr(previous) = arg;
  return apply(APPLY, first(args), cdr(args), env);
}

object *fn_funcall (object *args, object *env) {
  return apply(FUNCALL, first(args), cdr(args), env);
}

object *fn_append (object *args, object *env) {
  (void) env;
  object *head = NULL;
  object *tail;
  while (args != NULL) {
    object *list = first(args);
    if (!listp(list)) error(APPEND, notalist, list);
    while (consp(list)) {
      object *obj = cons(car(list), cdr(list));
      if (head == NULL) head = obj;
      else cdr(tail) = obj;
      tail = obj;
      list = cdr(list);
      if (cdr(args) != NULL && improperp(list)) error(APPEND, notproper, first(args));
    }
    args = cdr(args);
  }
  return head;
}

object *fn_mapc (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *result = first(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         return result;
      }
      if (improperp(list)) error(MAPC, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    apply(MAPC, function, cdr(params), env);
  }
}

object *fn_mapcar (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL);
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(MAPCAR, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(MAPCAR, function, cdr(params), env);
    object *obj = cons(result,NULL);
    cdr(tail) = obj; tail = obj;
  }
}

object *fn_mapcan (object *args, object *env) {
  object *function = first(args);
  args = cdr(args);
  object *params = cons(NULL, NULL);
  push(params,GCStack);
  object *head = cons(NULL, NULL);
  push(head,GCStack);
  object *tail = head;
  // Make parameters
  while (true) {
    object *tailp = params;
    object *lists = args;
    while (lists != NULL) {
      object *list = car(lists);
      if (list == NULL) {
         pop(GCStack);
         pop(GCStack);
         return cdr(head);
      }
      if (improperp(list)) error(MAPCAN, notproper, list);
      object *obj = cons(first(list),NULL);
      car(lists) = cdr(list);
      cdr(tailp) = obj; tailp = obj;
      lists = cdr(lists);
    }
    object *result = apply(MAPCAN, function, cdr(params), env);
    while (consp(result)) {
      cdr(tail) = result; tail = result;
      result = cdr(result);
    }
    if (result != NULL) error(MAPCAN, resultproper, result);
  }
}

// Arithmetic functions

object *add_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult + checkintfloat(ADD, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_add (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    object *arg = car(args);
    if (floatp(arg)) return add_floats(args, (float)result);
    else if (integerp(arg)) {
      int val = arg->integer;
      if (val < 1) { if (INT_MIN - val > result) return add_floats(args, (float)result); }
      else { if (INT_MAX - val < result) return add_floats(args, (float)result); }
      result = result + val;
    } else error(ADD, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}

object *subtract_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    fresult = fresult - checkintfloat(SUBTRACT, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *negate (object *arg) {
  if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(-result);
    else return number(-result);
  } else if (floatp(arg)) return makefloat(-(arg->single_float));
  else error(SUBTRACT, notanumber, arg);
}

object *fn_subtract (object *args, object *env) {
  (void) env;
  object *arg = car(args);
  args = cdr(args);
  if (args == NULL) return negate(arg);
  else if (floatp(arg)) return subtract_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) return subtract_floats(args, result);
      else if (integerp(arg)) {
        int val = (car(args))->integer;
        if (val < 1) { if (INT_MAX + val < result) return subtract_floats(args, result); }
        else { if (INT_MIN + val > result) return subtract_floats(args, result); }
        result = result - val;
      } else error(SUBTRACT, notanumber, arg);
      args = cdr(args);
    }
    return number(result);
  } else error(SUBTRACT, notanumber, arg);
}

object *multiply_floats (object *args, float fresult) {
  while (args != NULL) {
   object *arg = car(args);
    fresult = fresult * checkintfloat(MULTIPLY, arg);
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_multiply (object *args, object *env) {
  (void) env;
  int result = 1;
  while (args != NULL){
    object *arg = car(args);
    if (floatp(arg)) return multiply_floats(args, result);
    else if (integerp(arg)) {
      int64_t val = result * (int64_t)(arg->integer);
      if ((val > INT_MAX) || (val < INT_MIN)) return multiply_floats(args, result);
      result = val;
    } else error(MULTIPLY, notanumber, arg);
    args = cdr(args);
  }
  return number(result);
}

object *divide_floats (object *args, float fresult) {
  while (args != NULL) {
    object *arg = car(args);
    float f = checkintfloat(DIVIDE, arg);
    if (f == 0.0) error2(DIVIDE, PSTR("division by zero"));
    fresult = fresult / f;
    args = cdr(args);
  }
  return makefloat(fresult);
}

object *fn_divide (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  args = cdr(args);
  // One argument
  if (args == NULL) {
    if (floatp(arg)) {
      float f = arg->single_float;
      if (f == 0.0) error2(DIVIDE, PSTR("division by zero"));
      return makefloat(1.0 / f);
    } else if (integerp(arg)) {
      int i = arg->integer;
      if (i == 0) error2(DIVIDE, PSTR("division by zero"));
      else if (i == 1) return number(1);
      else return makefloat(1.0 / i);
    } else error(DIVIDE, notanumber, arg);
  }
  // Multiple arguments
  if (floatp(arg)) return divide_floats(args, arg->single_float);
  else if (integerp(arg)) {
    int result = arg->integer;
    while (args != NULL) {
      arg = car(args);
      if (floatp(arg)) {
        return divide_floats(args, result);
      } else if (integerp(arg)) {
        int i = arg->integer;
        if (i == 0) error2(DIVIDE, PSTR("division by zero"));
        if ((result % i) != 0) return divide_floats(args, result);
        if ((result == INT_MIN) && (i == -1)) return divide_floats(args, result);
        result = result / i;
        args = cdr(args);
      } else error(DIVIDE, notanumber, arg);
    }
    return number(result);
  } else error(DIVIDE, notanumber, arg);
}

object *fn_mod (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  object *arg2 = second(args);
  if (integerp(arg1) && integerp(arg2)) {
    int divisor = arg2->integer;
    if (divisor == 0) error2(MOD, PSTR("division by zero"));
    int dividend = arg1->integer;
    int remainder = dividend % divisor;
    if ((dividend<0) != (divisor<0)) remainder = remainder + divisor;
    return number(remainder);
  } else {
    float fdivisor = checkintfloat(MOD, arg2);
    if (fdivisor == 0.0) error2(MOD, PSTR("division by zero"));
    float fdividend = checkintfloat(MOD, arg1);
    float fremainder = fmod(fdividend , fdivisor);
    if ((fdividend<0) != (fdivisor<0)) fremainder = fremainder + fdivisor;
    return makefloat(fremainder);
  }
}

object *fn_oneplus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) + 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MAX) return makefloat((arg->integer) + 1.0);
    else return number(result + 1);
  } else error(ONEPLUS, notanumber, arg);
}

object *fn_oneminus (object *args, object *env) {
  (void) env;
  object* arg = first(args);
  if (floatp(arg)) return makefloat((arg->single_float) - 1.0);
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat((arg->integer) - 1.0);
    else return number(result - 1);
  } else error(ONEMINUS, notanumber, arg);
}

object *fn_abs (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return makefloat(abs(arg->single_float));
  else if (integerp(arg)) {
    int result = arg->integer;
    if (result == INT_MIN) return makefloat(abs((float)result));
    else return number(abs(result));
  } else error(ABS, notanumber, arg);
}

object *fn_random (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (integerp(arg)) return number(random(arg->integer));
  else if (floatp(arg)) return makefloat((float)rand()/(float)(RAND_MAX/(arg->single_float)));
  else error(RANDOM, notanumber, arg);
}

object *fn_maxfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) > (result->integer)) result = arg;
    } else if ((checkintfloat(MAXFN, arg) > checkintfloat(MAXFN, result))) result = arg;
    args = cdr(args);
  }
  return result;
}

object *fn_minfn (object *args, object *env) {
  (void) env;
  object* result = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg = car(args);
    if (integerp(result) && integerp(arg)) {
      if ((arg->integer) < (result->integer)) result = arg;
    } else if ((checkintfloat(MINFN, arg) < checkintfloat(MINFN, result))) result = arg;
    args = cdr(args);
  }
  return result;
}

// Arithmetic comparisons

object *fn_noteq (object *args, object *env) {
  (void) env;
  while (args != NULL) {
    object *nargs = args;
    object *arg1 = first(nargs);
    nargs = cdr(nargs);
    while (nargs != NULL) {
      object *arg2 = first(nargs);
      if (integerp(arg1) && integerp(arg2)) {
        if ((arg1->integer) == (arg2->integer)) return nil;
      } else if ((checkintfloat(NOTEQ, arg1) == checkintfloat(NOTEQ, arg2))) return nil;
      nargs = cdr(nargs);
    }
    args = cdr(args);
  }
  return tee;
}

object *fn_numeq (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) == (arg2->integer))) return nil;
    } else if (!(checkintfloat(NUMEQ, arg1) == checkintfloat(NUMEQ, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

object *fn_less (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) < (arg2->integer))) return nil;
    } else if (!(checkintfloat(LESS, arg1) < checkintfloat(LESS, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

object *fn_lesseq (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) <= (arg2->integer))) return nil;
    } else if (!(checkintfloat(LESSEQ, arg1) <= checkintfloat(LESSEQ, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

object *fn_greater (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) > (arg2->integer))) return nil;
    } else if (!(checkintfloat(GREATER, arg1) > checkintfloat(GREATER, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

object *fn_greatereq (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) >= (arg2->integer))) return nil;
    } else if (!(checkintfloat(GREATEREQ, arg1) >= checkintfloat(GREATEREQ, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}

object *fn_plusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) > 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) > 0) ? tee : nil;
  else error(PLUSP, notanumber, arg);
}

object *fn_minusp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) < 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) < 0) ? tee : nil;
  else error(MINUSP, notanumber, arg);
}

object *fn_zerop (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (floatp(arg)) return ((arg->single_float) == 0.0) ? tee : nil;
  else if (integerp(arg)) return ((arg->integer) == 0) ? tee : nil;
  else error(ZEROP, notanumber, arg);
}

object *fn_oddp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(ODDP, first(args));
  return ((arg & 1) == 1) ? tee : nil;
}

object *fn_evenp (object *args, object *env) {
  (void) env;
  int arg = checkinteger(EVENP, first(args));
  return ((arg & 1) == 0) ? tee : nil;
}

// Number functions

object *fn_integerp (object *args, object *env) {
  (void) env;
  return integerp(first(args)) ? tee : nil;
}

object *fn_numberp (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (integerp(arg) || floatp(arg)) ? tee : nil;
}

// Floating-point functions

object *fn_floatfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  return (floatp(arg)) ? arg : makefloat((float)(arg->integer));
}

object *fn_floatp (object *args, object *env) {
  (void) env;
  return floatp(first(args)) ? tee : nil;
}

object *fn_sin (object *args, object *env) {
  (void) env;
  return makefloat(sin(checkintfloat(SIN, first(args))));
}

object *fn_cos (object *args, object *env) {
  (void) env;
  return makefloat(cos(checkintfloat(COS, first(args))));
}

object *fn_tan (object *args, object *env) {
  (void) env;
  return makefloat(tan(checkintfloat(TAN, first(args))));
}

object *fn_asin (object *args, object *env) {
  (void) env;
  return makefloat(asin(checkintfloat(ASIN, first(args))));
}

object *fn_acos (object *args, object *env) {
  (void) env;
  return makefloat(acos(checkintfloat(ACOS, first(args))));
}

object *fn_atan (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float div = 1.0;
  args = cdr(args);
  if (args != NULL) div = checkintfloat(ATAN, first(args));
  return makefloat(atan2(checkintfloat(ATAN, arg), div));
}

object *fn_sinh (object *args, object *env) {
  (void) env;
  return makefloat(sinh(checkintfloat(SINH, first(args))));
}

object *fn_cosh (object *args, object *env) {
  (void) env;
  return makefloat(cosh(checkintfloat(COSH, first(args))));
}

object *fn_tanh (object *args, object *env) {
  (void) env;
  return makefloat(tanh(checkintfloat(TANH, first(args))));
}

object *fn_exp (object *args, object *env) {
  (void) env;
  return makefloat(exp(checkintfloat(EXP, first(args))));
}

object *fn_sqrt (object *args, object *env) {
  (void) env;
  return makefloat(sqrt(checkintfloat(SQRT, first(args))));
}

object *fn_log (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  float fresult = log(checkintfloat(LOG, arg));
  args = cdr(args);
  if (args == NULL) return makefloat(fresult);
  else return makefloat(fresult / log(checkintfloat(LOG, first(args))));
}

int intpower (int base, int exp) {
  int result = 1;
  while (exp) {
    if (exp & 1) result = result * base;
    exp = exp / 2;
    base = base * base;
  }
  return result;
}

object *fn_expt (object *args, object *env) {
  (void) env;
  object *arg1 = first(args); object *arg2 = second(args);
  float float1 = checkintfloat(EXPT, arg1);
  float value = log(abs(float1)) * checkintfloat(EXPT, arg2);
  if (integerp(arg1) && integerp(arg2) && ((arg2->integer) > 0) && (abs(value) < 21.4875))
    return number(intpower(arg1->integer, arg2->integer));
  if (float1 < 0) error2(EXPT, PSTR("invalid result"));
  return makefloat(exp(value));
}

object *fn_ceiling (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(ceil(checkintfloat(CEILING, arg) / checkintfloat(CEILING, first(args))));
  else return number(ceil(checkintfloat(CEILING, arg)));
}

object *fn_floor (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(floor(checkintfloat(FLOOR, arg) / checkintfloat(FLOOR, first(args))));
  else return number(floor(checkintfloat(FLOOR, arg)));
}

object *fn_truncate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number((int)(checkintfloat(TRUNCATE, arg) / checkintfloat(TRUNCATE, first(args))));
  else return number((int)(checkintfloat(TRUNCATE, arg)));
}

int myround (float number) {
  return (number >= 0) ? (int)(number + 0.5) : (int)(number - 0.5);
}

object *fn_round (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(myround(checkintfloat(ROUND, arg) / checkintfloat(ROUND, first(args))));
  else return number(myround(checkintfloat(ROUND, arg)));
}

// Characters

object *fn_char (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(CHAR, notastring, arg);
  char c = nthchar(arg, checkinteger(CHAR, second(args)));
  if (c == 0) error2(CHAR, PSTR("index out of range"));
  return character(c);
}

object *fn_charcode (object *args, object *env) {
  (void) env;
  return number(checkchar(CHARCODE, first(args)));
}

object *fn_codechar (object *args, object *env) {
  (void) env;
  return character(checkinteger(CODECHAR, first(args)));
}

object *fn_characterp (object *args, object *env) {
  (void) env;
  return characterp(first(args)) ? tee : nil;
}

// Strings

object *fn_stringp (object *args, object *env) {
  (void) env;
  return stringp(first(args)) ? tee : nil;
}

bool stringcompare (symbol_t name, object *args, bool lt, bool gt, bool eq) {
  object *arg1 = first(args); if (!stringp(arg1)) error(name, notastring, arg1);
  object *arg2 = second(args); if (!stringp(arg2)) error(name, notastring, arg2);
  arg1 = cdr(arg1);
  arg2 = cdr(arg2);
  while ((arg1 != NULL) || (arg2 != NULL)) {
    if (arg1 == NULL) return lt;
    if (arg2 == NULL) return gt;
    if (arg1->integer < arg2->integer) return lt;
    if (arg1->integer > arg2->integer) return gt;
    arg1 = car(arg1);
    arg2 = car(arg2);
  }
  return eq;
}

object *fn_stringeq (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGEQ, args, false, false, true) ? tee : nil;
}

object *fn_stringless (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGLESS, args, true, false, false) ? tee : nil;
}

object *fn_stringgreater (object *args, object *env) {
  (void) env;
  return stringcompare(STRINGGREATER, args, false, true, false) ? tee : nil;
}

object *fn_sort (object *args, object *env) {
  if (first(args) == NULL) return nil;
  object *list = cons(nil,first(args));
  push(list,GCStack);
  object *predicate = second(args);
  object *compare = cons(NULL, cons(NULL, NULL));
  object *ptr = cdr(list);
  while (cdr(ptr) != NULL) {
    object *go = list;
    while (go != ptr) {
      car(compare) = car(cdr(ptr));
      car(cdr(compare)) = car(cdr(go));
      if (apply(SORT, predicate, compare, env)) break;
      go = cdr(go);
    }
    if (go != ptr) {
      object *obj = cdr(ptr);
      cdr(ptr) = cdr(obj);
      cdr(obj) = cdr(go);
      cdr(go) = obj;
    } else ptr = cdr(ptr);
  }
  pop(GCStack);
  return cdr(list);
}

object *fn_stringfn (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  int type = arg->type;
  if (type == STRING) return arg;
  object *obj = myalloc();
  obj->type = STRING;
  if (type == CHARACTER) {
    object *cell = myalloc();
    cell->car = NULL;
    uint8_t shift = (sizeof(int)-1)*8;
    cell->integer = (arg->integer)<<shift;
    obj->cdr = cell;
  } else if (type == SYMBOL) {
    char *s = symbolname(arg->name);
    char ch = *s++;
    object *head = NULL;
    int chars = 0;
    while (ch) {
      if (ch == '\\') ch = *s++;
      buildstring(ch, &chars, &head);
      ch = *s++;
    }
    obj->cdr = head;
  } else error(STRINGFN, PSTR("can't convert to string"), arg);
  return obj;
}

object *fn_concatenate (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  symbol_t name = arg->name;
  if (name != STRINGFN) error2(CONCATENATE, PSTR("only supports strings"));
  args = cdr(args);
  object *result = myalloc();
  result->type = STRING;
  object *head = NULL;
  int chars = 0;
  while (args != NULL) {
    object *obj = first(args);
    if (obj->type != STRING) error(CONCATENATE, notastring, obj);
    obj = cdr(obj);
    while (obj != NULL) {
      int quad = obj->integer;
      while (quad != 0) {
         char ch = quad>>((sizeof(int)-1)*8) & 0xFF;
         buildstring(ch, &chars, &head);
         quad = quad<<8;
      }
      obj = car(obj);
    }
    args = cdr(args);
  }
  result->cdr = head;
  return result;
}

object *fn_subseq (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(SUBSEQ, notastring, arg);
  int start = checkinteger(SUBSEQ, second(args));
  int end;
  args = cddr(args);
  if (args != NULL) end = checkinteger(SUBSEQ, car(args)); else end = stringlength(arg);
  object *result = myalloc();
  result->type = STRING;
  object *head = NULL;
  int chars = 0;
  for (int i=start; i<end; i++) {
    char ch = nthchar(arg, i);
    if (ch == 0) error2(SUBSEQ, PSTR("index out of range"));
    buildstring(ch, &chars, &head);
  }
  result->cdr = head;
  return result;
}

int gstr () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = nthchar(GlobalString, GlobalStringIndex++);
  return (c != 0) ? c : '\n'; // -1?
}

object *fn_readfromstring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  if (!stringp(arg)) error(READFROMSTRING, notastring, arg);
  GlobalString = arg;
  GlobalStringIndex = 0;
  return read(gstr);
}

void pstr (char c) {
  buildstring(c, &GlobalStringIndex, &GlobalString);
}

object *fn_princtostring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  object *obj = myalloc();
  obj->type = STRING;
  GlobalString = NULL;
  GlobalStringIndex = 0;
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(arg, pstr);
  Flags = temp;
  obj->cdr = GlobalString;
  return obj;
}

object *fn_prin1tostring (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  object *obj = myalloc();
  obj->type = STRING;
  GlobalString = NULL;
  GlobalStringIndex = 0;
  printobject(arg, pstr);
  obj->cdr = GlobalString;
  return obj;
}

// Bitwise operators

object *fn_logand (object *args, object *env) {
  (void) env;
  int result = -1;
  while (args != NULL) {
    result = result & checkinteger(LOGAND, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_logior (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    result = result | checkinteger(LOGIOR, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_logxor (object *args, object *env) {
  (void) env;
  int result = 0;
  while (args != NULL) {
    result = result ^ checkinteger(LOGXOR, first(args));
    args = cdr(args);
  }
  return number(result);
}

object *fn_lognot (object *args, object *env) {
  (void) env;
  int result = checkinteger(LOGNOT, car(args));
  return number(~result);
}

object *fn_ash (object *args, object *env) {
  (void) env;
  int value = checkinteger(ASH, first(args));
  int count = checkinteger(ASH, second(args));
  if (count >= 0) return number(value << count);
  else return number(value >> abs(count));
}

object *fn_logbitp (object *args, object *env) {
  (void) env;
  int index = checkinteger(LOGBITP, first(args));
  int value = checkinteger(LOGBITP, second(args));
  return (bitRead(value, index) == 1) ? tee : nil;
}

// System functions

object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}

object *fn_globals (object *args, object *env) {
  (void) args;
  if (GlobalEnv == NULL) return nil;
  return fn_mapcar(cons(symbol(CAR),cons(GlobalEnv,nil)), env);
}

object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}

object *fn_makunbound (object *args, object *env) {
  (void) env;
  object *key = first(args);
  deletesymbol(key->name);
  return (delassoc(key, &GlobalEnv) != NULL) ? tee : nil;
}

object *fn_break (object *args, object *env) {
  (void) args;
  pfstring(PSTR("\rBreak!\r"), pserial);
  BreakLevel++;
  repl(env);
  BreakLevel--;
  return nil;
}

object *fn_read (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return read(gfun);
}

object *fn_prin1 (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  printobject(obj, pfun);
  return obj;
}

object *fn_print (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  printobject(obj, pfun);
  (pfun)(' ');
  return obj;
}

object *fn_princ (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printobject(obj, pfun);
  Flags = temp;
  return obj;
}

object *fn_terpri (object *args, object *env) {
  (void) env;
  pfun_t pfun = pstreamfun(args);
  pln(pfun);
  return nil;
}

object *fn_readbyte (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  int c = gfun();
  return (c == -1) ? nil : number(c);
}

object *fn_readline (object *args, object *env) {
  (void) env;
  gfun_t gfun = gstreamfun(args);
  return readstring('\n', gfun);
}

object *fn_writebyte (object *args, object *env) {
  (void) env;
  int value = checkinteger(WRITEBYTE, first(args));
  pfun_t pfun = pstreamfun(cdr(args));
  (pfun)(value);
  return nil;
}

object *fn_writestring (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  Flags = temp;
  return nil;
}

object *fn_writeline (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  char temp = Flags;
  clrflag(PRINTREADABLY);
  printstring(obj, pfun);
  pln(pfun);
  Flags = temp;
  return nil;
}

object *fn_restarti2c (object *args, object *env) {
  (void) env;
  int stream = first(args)->integer;
  args = cdr(args);
  int read = 0; // Write
  I2CCount = 0;
  if (args != NULL) {
    object *rw = first(args);
    if (integerp(rw)) I2CCount = rw->integer;
    read = (rw != NULL);
  }
  int address = stream & 0xFF;
  if (stream>>8 != I2CSTREAM) error2(RESTARTI2C, PSTR("not an i2c stream"));
  return I2Crestart(address, read) ? tee : nil;
}

object *fn_gc (object *obj, object *env) {
  int initial = Freespace;
  unsigned long start = micros();
  gc(obj, env);
  unsigned long elapsed = micros() - start;
  pfstring(PSTR("Space: "), pserial);
  pint(Freespace - initial, pserial);
  pfstring(PSTR(" bytes, Time: "), pserial);
  pint(elapsed, pserial);
  pfstring(PSTR(" us\r"), pserial);
  return nil;
}

object *fn_room (object *args, object *env) {
  (void) args, (void) env;
  return number(Freespace);
}

object *fn_saveimage (object *args, object *env) {
  if (args != NULL) args = eval(first(args), env);
  return number(saveimage(args));
}

object *fn_loadimage (object *args, object *env) {
  (void) env;
  if (args != NULL) args = first(args);
  return number(loadimage(args));
}

object *fn_cls (object *args, object *env) {
  (void) args, (void) env;
  pserial(12);
  return nil;
}

// Arduino procedures

object *fn_pinmode (object *args, object *env) {
  (void) env;
  int pin = checkinteger(PINMODE, first(args));
  PinMode pm = INPUT;
  object *mode = second(args);
  if (integerp(mode)) {
    int nmode = mode->integer;
    if (nmode == 1) pm = OUTPUT; else if (nmode == 2) pm = INPUT_PULLUP;
    #if defined(INPUT_PULLDOWN)
    else if (nmode == 4) pm = INPUT_PULLDOWN;
    #endif
  } else if (mode != nil) pm = OUTPUT;
  pinMode(pin, pm);
  return nil;
}

object *fn_digitalread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALREAD, first(args));
  if (digitalRead(pin) != 0) return tee; else return nil;
}

object *fn_digitalwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(DIGITALWRITE, first(args));
  object *mode = second(args);
  if (integerp(mode)) digitalWrite(pin, mode->integer ? HIGH : LOW);
  else digitalWrite(pin, (mode != nil) ? HIGH : LOW);
  return mode;
}

object *fn_analogread (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGREAD, first(args));
  checkanalogread(pin);
  return number(analogRead(pin));
}

object *fn_analogwrite (object *args, object *env) {
  (void) env;
  int pin = checkinteger(ANALOGWRITE, first(args));
  checkanalogwrite(pin);
  object *value = second(args);
  analogWrite(pin, checkinteger(ANALOGWRITE, value));
  return value;
}

object *fn_delay (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  delay(checkinteger(DELAY, arg1));
  return arg1;
}

object *fn_millis (object *args, object *env) {
  (void) args, (void) env;
  return number(millis());
}

object *fn_sleep (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  sleep(checkinteger(SLEEP, arg1));
  return arg1;
}

object *fn_note (object *args, object *env) {
  (void) env;
  static int pin = 255;
  if (args != NULL) {
    pin = checkinteger(NOTE, first(args));
    int note = 0;
    if (cddr(args) != NULL) note = checkinteger(NOTE, second(args));
    int octave = 0;
    if (cddr(args) != NULL) octave = checkinteger(NOTE, third(args));
    playnote(pin, note, octave);
  } else nonote(pin);
  return nil;
}

// Tree Editor

object *fn_edit (object *args, object *env) {
  object *fun = first(args);
  object *pair = findvalue(fun, env);
  clrflag(EXITEDITOR);
  object *arg = edit(eval(fun, env));
  cdr(pair) = arg;
  return arg;
}

object *edit (object *fun) {
  while (1) {
    if (tstflag(EXITEDITOR)) return fun;
    char c = gserial();
    if (c == 'q') setflag(EXITEDITOR);
    else if (c == 'b') return fun;
    else if (c == 'r') fun = read(gserial);
    else if (c == '\n') { pfl(pserial); superprint(fun, 0, pserial); pln(pserial); }
    else if (c == 'c') fun = cons(read(gserial), fun);
    else if (atom(fun)) pserial('!');
    else if (c == 'd') fun = cons(car(fun), edit(cdr(fun)));
    else if (c == 'a') fun = cons(edit(car(fun)), cdr(fun));
    else if (c == 'x') fun = cdr(fun);
    else pserial('?');
  }
}

// Pretty printer

const int PPINDENT = 2;
const int PPWIDTH = 80;

void pcount (char c) {
  LastPrint = c;
  if (c == '\n') GlobalStringIndex++;
  GlobalStringIndex++;
}

int atomwidth (object *obj) {
  GlobalStringIndex = 0;
  printobject(obj, pcount);
  return GlobalStringIndex;
}

boolean quoted (object *obj) {
  return (consp(obj) && car(obj) != NULL && car(obj)->name == QUOTE && consp(cdr(obj)) && cddr(obj) == NULL);
}

boolean quasiquoted (object *obj) {
  return (consp(obj) && car(obj) != NULL && car(obj)->name == QUASIQUOTE && consp(cdr(obj)) && cddr(obj) == NULL);
}

boolean macrop(object *obj) {
  return (consp(obj) && car(obj) != NULL && car(obj)->name == MACRO);
}

int subwidth (object *obj, int w) {
  if (atom(obj)) return w - atomwidth(obj);
  if (quoted(obj)) return subwidthlist(car(cdr(obj)), w - 1);
  return subwidthlist(obj, w - 1);
}

int subwidthlist (object *form, int w) {
  while (form != NULL && w >= 0) {
    if (atom(form)) return w - (2 + atomwidth(form));
    w = subwidth(car(form), w - 1);
    form = cdr(form);
  }
  return w;
}

void superprint (object *form, int lm, pfun_t pfun) {
  if (atom(form)) {
    if (symbolp(form) && form->name == NOTHING) pstring(symbolname(form->name), pfun);
    else printobject(form, pfun);
  }
  else if (quoted(form)) { pfun('\''); superprint(car(cdr(form)), lm + 1, pfun); }
  else if (quasiquoted(form)) { pfun('`'); superprint(car(cdr(form)), lm + 1, pfun); }
  else if (subwidth(form, PPWIDTH - lm) >= 0) supersub(form, lm + PPINDENT, 0, pfun);
  else supersub(form, lm + PPINDENT, 1, pfun);
}

const int ppspecials = 15;
const char ppspecial[ppspecials] PROGMEM =
  { DOTIMES, DOLIST, IF, SETQ, TEE, LET, LETSTAR, LAMBDA, WHEN, UNLESS, WITHI2C, WITHSERIAL, WITHSPI, WITHSDCARD, FORMILLIS };

void supersub (object *form, int lm, int super, pfun_t pfun) {
  int special = 0, separate = 1;
  object *arg = car(form);
  if (symbolp(arg)) {
    int name = arg->name;
    if (name == DEFUN) special = 2;
    else for (int i=0; i<ppspecials; i++) {
      if (name == ppspecial[i]) { special = 1; break; }
    }
  }
  while (form != NULL) {
    if (atom(form)) { pfstring(PSTR(" . "), pfun); printobject(form, pfun); pfun(')'); return; }
    else if (separate) { pfun('('); separate = 0; }
    else if (special) { pfun(' '); special--; }
    else if (!super) pfun(' ');
    else { pln(pfun); indent(lm, pfun); }
    superprint(car(form), lm, pfun);
    form = cdr(form);
  }
  pfun(')'); return;
}

object *fn_pprint (object *args, object *env) {
  (void) env;
  object *obj = first(args);
  pfun_t pfun = pstreamfun(cdr(args));
  pln(pfun);
  superprint(obj, 0, pfun);
  return symbol(NOTHING);
}

object *fn_pprintall (object *args, object *env) {
  (void) args, (void) env;
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    object *val = cdr(pair);
    pln(pserial);
    if (consp(val) && symbolp(car(val)) && car(val)->name == LAMBDA) {
      superprint(cons(symbol(DEFUN), cons(var, cdr(val))), 0, pserial);
    } else {
      superprint(cons(symbol(DEFVAR),cons(var,cons(cons(symbol(QUOTE),cons(val,NULL))
      ,NULL))), 0, pserial);
    }
    pln(pserial);
    globals = cdr(globals);
  }
  return symbol(NOTHING);
}

// LispLibrary

object *fn_require (object *args, object *env) {
  object *arg = first(args);
  object *globals = GlobalEnv;
  if (!symbolp(arg)) error(REQUIRE, PSTR("argument is not a symbol"), arg);
  while (globals != NULL) {
    object *pair = first(globals);
    object *var = car(pair);
    if (symbolp(var) && var == arg) return nil;
    globals = cdr(globals);
  }
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    // Is this the definition we want
    int fname = first(line)->name;
    if ((fname == DEFUN || fname == DEFVAR) && symbolp(second(line)) && second(line)->name == arg->name) {
      eval(line, env);
      return tee;
    }
    line = read(glibrary);
  }
  return nil;
}

object *fn_listlibrary (object *args, object *env) {
  (void) args, (void) env;
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    int fname = first(line)->name;
    if (fname == DEFUN || fname == DEFVAR) {
      pstring(symbolname(second(line)->name), pserial); pserial(' ');
    }
    line = read(glibrary);
  }
  return symbol(NOTHING);
}

// Insert your own function definitions here

// LIS3DH support

#if defined(lis3dhsupport)
Adafruit_LIS3DH lis = Adafruit_LIS3DH();
#define CLICK_THRESHOLD 40
#endif

object *fn_init_lis3dh(object *args, object *env) {
#if defined(lis3dhsupport)
  if (!lis.begin(0x18)) {
    error2(INITLIS3DH, PSTR("couldn't start LIS3DH"));
  }
  lis.setRange(LIS3DH_RANGE_4_G);
  lis.setClick(1, CLICK_THRESHOLD);
  return nil;
#else
  (void) args, (void) env;
  error2(INITLIS3DH, PSTR("not supported"));
  return nil;
#endif
}

object *fn_lis3dh_bump(object *args, object *env) {
#if defined(lis3dhsupport)
  uint8_t bump = lis.getClick();
  return (bump & 0x10) ? tee : nil;
#else
  (void) args, (void) env;
  error2(LIS3DHBUMP, PSTR("not supported"));
  return nil;
#endif
}

object *fn_lis3dh_acceleration(object *args, object *env) {
#if defined(lis3dhsupport)
  sensors_event_t event;
  lis.getEvent(&event);
  return cons(number(event.acceleration.x), cons(number(event.acceleration.y), number(event.acceleration.z)));
#else
  (void) args, (void) env;
  error2(LIS3DHACCELERATION, PSTR("not supported"));
  return nil;
#endif
}


// Debouncer support

Bounce *debouncers[16];
int number_of_debouncers = 0;

object *fn_make_debouncer(object *args, object *env) {
  if (number_of_debouncers == 16) {
    error2(MAKEDEBOUNCER, PSTR("too many debouncers requested"));
  }
  int pin = checkinteger(MAKEDEBOUNCER, first(args));
  Bounce *debouncer = new Bounce();
  debouncer->attach(pin, INPUT_PULLUP);
  debouncers[number_of_debouncers] = debouncer;
  return number(number_of_debouncers++);
}

object *fn_update_debouncers(object *args, object *env) {
  for (int i = 0; i < number_of_debouncers; i++) {
    debouncers[i]->update();
  }
  return nil;
}

object *fn_debouncer_value(object *args, object *env) {
  int debouncer_number = checkinteger(DEBOUNCERVALUE, first(args));
  if (debouncer_number >= number_of_debouncers) {
    error(DEBOUNCERVALUE, PSTR("debouncer ID out of range"), first(args));
  }
  return debouncers[debouncer_number]->read() ? tee : nil;
}

object *fn_debouncer_rose(object *args, object *env) {
  int debouncer_number = checkinteger(DEBOUNCERROSE, first(args));
  if (debouncer_number >= number_of_debouncers) {
    error(DEBOUNCERROSE, PSTR("debouncer ID out of range"), first(args));
  }
  return debouncers[debouncer_number]->rose() ? tee : nil;
}

object *fn_debouncer_fell(object *args, object *env) {
  int debouncer_number = checkinteger(DEBOUNCERFELL, first(args));
  if (debouncer_number >= number_of_debouncers) {
    error(DEBOUNCERFELL, PSTR("debouncer ID out of range"), first(args));
  }
  return debouncers[debouncer_number]->fell() ? tee : nil;
}

// Macro support

object *sp_defmacro (object *args, object *env) {
  (void) env;
  checkargs(DEFMACRO, args);
  object *var = first(args);
  if (var->type != SYMBOL) error(DEFMACRO, PSTR("not a symbol"), var);
  object *val = cons(symbol(MACRO), cdr(args));
  object *pair = value(var->name,GlobalEnv);
  if (pair != NULL) { cdr(pair) = val; return var; }
  push(cons(var, val), GlobalEnv);
  return var;
}

object *reverse_it(object *l) {
  object *reversed = NULL;
  while (l != NULL) {
    push(first(l),reversed);
    l = cdr(l);
  }
  return reversed;
}

object *reverse_and_flatten(object *expr) {
  if (!consp(expr))
    return expr;

  object *reversed = reverse_it(expr);

  object *result = NULL;
  for (object *cell = reversed; cell != NULL; cell = cdr(cell)) {
    if (!consp(car(cell))) {
      push(car(cell), result);
    } else {
      for (object *subcell = car(cell); subcell != NULL; subcell = cdr(subcell)) {
        push(car(subcell), result);
      }
    }
  }
  return reverse_it(result);
}

object *process_quasiquoted(object *expr, int level, object *env) {
  Serial.print("**** Processing quasiquote of : ");
  printobject(expr, pserial);
  Serial.println("");
  Serial.print("**** at level ");
  Serial.println(level);
  if (!consp(expr)) {
    return cons(expr, NULL);
  }

  if (issymbol(car(expr), QUASIQUOTE)) {
    Serial.println("Nested quasiquote");
    object *processed = process_quasiquoted(second(expr), level + 1, env);
    return cons(cons(symbol(QUASIQUOTE), processed), NULL);
  } else if (issymbol(car(expr), UNQUOTE)) {
    Serial.println("**** Processing UNQUOTE");
    Serial.print("**** At level ");
    Serial.println(level);
    if (level == 1) {
      object *processed = process_quasiquoted(second(expr), level, env);
      object *result = eval(car(processed), env);
      Serial.print("**** Result: ");
      printobject(result, pserial);
      Serial.println("");
      return cons(result, NULL);
    } else {
      object *processed = process_quasiquoted(second(expr), level - 1, env);
      return cons(cons(symbol(UNQUOTE), processed), NULL);
    }
  } else if (issymbol(car(expr), UNQUOTESPLICING)) {
    Serial.println("**** Processing UNQUOTESPLICING");
    Serial.print("**** At level ");
    Serial.println(level);
    if (level == 1) {
      object *processed = process_quasiquoted(second(expr), level, env);
      Serial.print("**** Processed: ");
      printobject(car(processed), pserial);
      Serial.println("");
      object *result = eval(car(processed), env);
      Serial.print("**** Result: ");
      printobject(result, pserial);
      Serial.println("");
      return result;
    } else {
      object *processed = process_quasiquoted(second(expr), level - 1, env);
      return cons(cons(symbol(UNQUOTESPLICING), processed), NULL);
    }
  } else {
    Serial.println("Processing something else");
    Serial.print("**** At level ");
    Serial.println(level);
    object *parts = NULL;
    for (object *cell = expr; cell != NULL; cell = cdr(cell)) {
      object *processed = process_quasiquoted(car(cell), level, env);
      push(processed, parts);
    }
    Serial.print("**** parts: ");
    printobject(parts, pserial);
    Serial.println("");

    object *result = reverse_and_flatten(parts);
    Serial.print("**** Result: ");
    printobject(result, pserial);
    Serial.println("");
    return cons(result, NULL);
  }


}

object *sp_quasiquote (object *args, object *env) {
  checkargs(QUASIQUOTE, args);
  object *result = process_quasiquoted(car(args), 1, env);
  return car(result);
}

object *sp_unquote (object *args, object *env) {
  (void)args;
  (void)env;
  error2(UNQUOTE, PSTR("not supported outside a quasiquote form"));
}

object *sp_unquote_splicing (object *args, object *env) {
  (void)args;
  (void)env;
  error2(UNQUOTESPLICING, PSTR("not supported outside a quasiquote form"));
}


object *expand (object *body, object *env) {
  object *expanded = eval(body, env);
  return expanded;
}

//MOVE arg evaluation to sp_expand
object *sp_expand (object *args, object *env) {
  object *macro = eval(car(args), env);
  if (!macrop(macro)) {
    error(EXPAND, PSTR("needs a macro"), macro);
  }
  object *params = car(cdr(macro));
  object *body = car(cdr(cdr(macro)));
  if (!consp(params) || !consp(cdr(args)) || listlength(EXPAND, params) != listlength(EXPAND, cdr(args))) {
    error2(EXPAND, PSTR("params/args mismatch"));
  }
  // add params->args to newenv
  object *newenv = env;
  push(newenv, GCStack);
  object *p;
  object *a;
  for (p = params, a = cdr(args); p != NULL; p = cdr(p), a = cdr(a)) {
     push(cons(car(p), eval(car(a), env)), newenv);
     // push(cons(car(p), car(a)), newenv);
     car(GCStack) = newenv;
  }
  object *expanded = expand(body, newenv);
  pop(GCStack);
  return expanded;
}

// Built-in procedure names - stored in PROGMEM

const char string0[] PROGMEM = "nil";
const char string1[] PROGMEM = "t";
const char string2[] PROGMEM = "nothing";
const char string3[] PROGMEM = "&optional";
const char string4[] PROGMEM = "&rest";
const char string5[] PROGMEM = "lambda";
const char string6[] PROGMEM = "let";
const char string7[] PROGMEM = "let*";
const char string8[] PROGMEM = "closure";
const char string9[] PROGMEM = "special_forms";
const char string10[] PROGMEM = "quote";
const char string11[] PROGMEM = "defun";
const char string12[] PROGMEM = "defvar";
const char string13[] PROGMEM = "setq";
const char string14[] PROGMEM = "loop";
const char string15[] PROGMEM = "return";
const char string16[] PROGMEM = "push";
const char string17[] PROGMEM = "pop";
const char string18[] PROGMEM = "incf";
const char string19[] PROGMEM = "decf";
const char string20[] PROGMEM = "setf";
const char string21[] PROGMEM = "dolist";
const char string22[] PROGMEM = "dotimes";
const char string23[] PROGMEM = "trace";
const char string24[] PROGMEM = "untrace";
const char string25[] PROGMEM = "for-millis";
const char string26[] PROGMEM = "with-serial";
const char string27[] PROGMEM = "with-i2c";
const char string28[] PROGMEM = "with-spi";
const char string29[] PROGMEM = "with-sd-card";
const char string30[] PROGMEM = "macro";
const char string31[] PROGMEM = "defmacro";
const char string32[] PROGMEM = "quasiquote";
const char string33[] PROGMEM = "unquote";
const char string34[] PROGMEM = "unquote-splicing";
const char string35[] PROGMEM = "expand";
const char string36[] PROGMEM = "tail_forms";
const char string37[] PROGMEM = "progn";
const char string38[] PROGMEM = "if";
const char string39[] PROGMEM = "cond";
const char string40[] PROGMEM = "when";
const char string41[] PROGMEM = "unless";
const char string42[] PROGMEM = "case";
const char string43[] PROGMEM = "and";
const char string44[] PROGMEM = "or";
const char string45[] PROGMEM = "functions";
const char string46[] PROGMEM = "not";
const char string47[] PROGMEM = "null";
const char string48[] PROGMEM = "cons";
const char string49[] PROGMEM = "atom";
const char string50[] PROGMEM = "listp";
const char string51[] PROGMEM = "consp";
const char string52[] PROGMEM = "symbolp";
const char string53[] PROGMEM = "streamp";
const char string54[] PROGMEM = "eq";
const char string55[] PROGMEM = "equal";
const char string56[] PROGMEM = "car";
const char string57[] PROGMEM = "first";
const char string58[] PROGMEM = "cdr";
const char string59[] PROGMEM = "rest";
const char string60[] PROGMEM = "caar";
const char string61[] PROGMEM = "cadr";
const char string62[] PROGMEM = "second";
const char string63[] PROGMEM = "cdar";
const char string64[] PROGMEM = "cddr";
const char string65[] PROGMEM = "caaar";
const char string66[] PROGMEM = "caadr";
const char string67[] PROGMEM = "cadar";
const char string68[] PROGMEM = "caddr";
const char string69[] PROGMEM = "third";
const char string70[] PROGMEM = "cdaar";
const char string71[] PROGMEM = "cdadr";
const char string72[] PROGMEM = "cddar";
const char string73[] PROGMEM = "cdddr";
const char string74[] PROGMEM = "length";
const char string75[] PROGMEM = "list";
const char string76[] PROGMEM = "reverse";
const char string77[] PROGMEM = "nth";
const char string78[] PROGMEM = "assoc";
const char string79[] PROGMEM = "member";
const char string80[] PROGMEM = "apply";
const char string81[] PROGMEM = "funcall";
const char string82[] PROGMEM = "append";
const char string83[] PROGMEM = "mapc";
const char string84[] PROGMEM = "mapcar";
const char string85[] PROGMEM = "mapcan";
const char string86[] PROGMEM = "+";
const char string87[] PROGMEM = "-";
const char string88[] PROGMEM = "*";
const char string89[] PROGMEM = "/";
const char string90[] PROGMEM = "mod";
const char string91[] PROGMEM = "1+";
const char string92[] PROGMEM = "1-";
const char string93[] PROGMEM = "abs";
const char string94[] PROGMEM = "random";
const char string95[] PROGMEM = "max";
const char string96[] PROGMEM = "min";
const char string97[] PROGMEM = "/=";
const char string98[] PROGMEM = "=";
const char string99[] PROGMEM = "<";
const char string100[] PROGMEM = "<=";
const char string101[] PROGMEM = ">";
const char string102[] PROGMEM = ">=";
const char string103[] PROGMEM = "plusp";
const char string104[] PROGMEM = "minusp";
const char string105[] PROGMEM = "zerop";
const char string106[] PROGMEM = "oddp";
const char string107[] PROGMEM = "evenp";
const char string108[] PROGMEM = "integerp";
const char string109[] PROGMEM = "numberp";
const char string110[] PROGMEM = "float";
const char string111[] PROGMEM = "floatp";
const char string112[] PROGMEM = "sin";
const char string113[] PROGMEM = "cos";
const char string114[] PROGMEM = "tan";
const char string115[] PROGMEM = "asin";
const char string116[] PROGMEM = "acos";
const char string117[] PROGMEM = "atan";
const char string118[] PROGMEM = "sinh";
const char string119[] PROGMEM = "cosh";
const char string120[] PROGMEM = "tanh";
const char string121[] PROGMEM = "exp";
const char string122[] PROGMEM = "sqrt";
const char string123[] PROGMEM = "log";
const char string124[] PROGMEM = "expt";
const char string125[] PROGMEM = "ceiling";
const char string126[] PROGMEM = "floor";
const char string127[] PROGMEM = "truncate";
const char string128[] PROGMEM = "round";
const char string129[] PROGMEM = "char";
const char string130[] PROGMEM = "char-code";
const char string131[] PROGMEM = "code-char";
const char string132[] PROGMEM = "characterp";
const char string133[] PROGMEM = "stringp";
const char string134[] PROGMEM = "string=";
const char string135[] PROGMEM = "string<";
const char string136[] PROGMEM = "string>";
const char string137[] PROGMEM = "sort";
const char string138[] PROGMEM = "string";
const char string139[] PROGMEM = "concatenate";
const char string140[] PROGMEM = "subseq";
const char string141[] PROGMEM = "read-from-string";
const char string142[] PROGMEM = "princ-to-string";
const char string143[] PROGMEM = "prin1-to-string";
const char string144[] PROGMEM = "logand";
const char string145[] PROGMEM = "logior";
const char string146[] PROGMEM = "logxor";
const char string147[] PROGMEM = "lognot";
const char string148[] PROGMEM = "ash";
const char string149[] PROGMEM = "logbitp";
const char string150[] PROGMEM = "eval";
const char string151[] PROGMEM = "globals";
const char string152[] PROGMEM = "locals";
const char string153[] PROGMEM = "makunbound";
const char string154[] PROGMEM = "break";
const char string155[] PROGMEM = "read";
const char string156[] PROGMEM = "prin1";
const char string157[] PROGMEM = "print";
const char string158[] PROGMEM = "princ";
const char string159[] PROGMEM = "terpri";
const char string160[] PROGMEM = "read-byte";
const char string161[] PROGMEM = "read-line";
const char string162[] PROGMEM = "write-byte";
const char string163[] PROGMEM = "write-string";
const char string164[] PROGMEM = "write-line";
const char string165[] PROGMEM = "restart-i2c";
const char string166[] PROGMEM = "gc";
const char string167[] PROGMEM = "room";
const char string168[] PROGMEM = "save-image";
const char string169[] PROGMEM = "load-image";
const char string170[] PROGMEM = "cls";
const char string171[] PROGMEM = "pinmode";
const char string172[] PROGMEM = "digitalread";
const char string173[] PROGMEM = "digitalwrite";
const char string174[] PROGMEM = "analogread";
const char string175[] PROGMEM = "analogwrite";
const char string176[] PROGMEM = "delay";
const char string177[] PROGMEM = "millis";
const char string178[] PROGMEM = "sleep";
const char string179[] PROGMEM = "note";
const char string180[] PROGMEM = "edit";
const char string181[] PROGMEM = "pprint";
const char string182[] PROGMEM = "pprintall";
const char string183[] PROGMEM = "require";
const char string184[] PROGMEM = "list-library";
const char string185[] PROGMEM = "make-debouncer";
const char string186[] PROGMEM = "update-debouncers";
const char string187[] PROGMEM = "debouncer-value?";
const char string188[] PROGMEM = "debouncer-rose?";
const char string189[] PROGMEM = "debouncer-fell?";
const char string190[] PROGMEM = "init-lis3dh";
const char string191[] PROGMEM = "lis3dh-bump?";
const char string192[] PROGMEM = "lis3dh-acceleration";


const tbl_entry_t lookup_table[] PROGMEM = {
  { string0, NULL, 0, 0 },
  { string1, NULL, 0, 0 },
  { string2, NULL, 0, 0 },
  { string3, NULL, 0, 0 },
  { string4, NULL, 0, 0 },
  { string5, NULL, 0, 127 },
  { string6, NULL, 0, 127 },
  { string7, NULL, 0, 127 },
  { string8, NULL, 0, 127 },
  { string9, NULL, NIL, NIL },
  { string10, sp_quote, 1, 1 },
  { string11, sp_defun, 0, 127 },
  { string12, sp_defvar, 2, 2 },
  { string13, sp_setq, 2, 126 },
  { string14, sp_loop, 0, 127 },
  { string15, sp_return, 0, 127 },
  { string16, sp_push, 2, 2 },
  { string17, sp_pop, 1, 1 },
  { string18, sp_incf, 1, 2 },
  { string19, sp_decf, 1, 2 },
  { string20, sp_setf, 2, 126 },
  { string21, sp_dolist, 1, 127 },
  { string22, sp_dotimes, 1, 127 },
  { string23, sp_trace, 0, 1 },
  { string24, sp_untrace, 0, 1 },
  { string25, sp_formillis, 1, 127 },
  { string26, sp_withserial, 1, 127 },
  { string27, sp_withi2c, 1, 127 },
  { string28, sp_withspi, 1, 127 },
  { string29, sp_withsdcard, 2, 127 },
  { string30, NULL, 0, 0 },
  { string31, sp_defmacro, 0, 127 },
  { string32, sp_quasiquote, 1, 1 },
  { string33, sp_unquote, 1, 1 },
  { string34, sp_unquote_splicing, 1, 1 },
  { string35, sp_expand, 1, 127 },
  { string36, NULL, NIL, NIL },
  { string37, tf_progn, 0, 127 },
  { string38, tf_if, 2, 3 },
  { string39, tf_cond, 0, 127 },
  { string40, tf_when, 1, 127 },
  { string41, tf_unless, 1, 127 },
  { string42, tf_case, 1, 127 },
  { string43, tf_and, 0, 127 },
  { string44, tf_or, 0, 127 },
  { string45, NULL, NIL, NIL },
  { string46, fn_not, 1, 1 },
  { string47, fn_not, 1, 1 },
  { string48, fn_cons, 2, 2 },
  { string49, fn_atom, 1, 1 },
  { string50, fn_listp, 1, 1 },
  { string51, fn_consp, 1, 1 },
  { string52, fn_symbolp, 1, 1 },
  { string53, fn_streamp, 1, 1 },
  { string54, fn_eq, 2, 2 },
  { string55, fn_equal, 2, 2 },
  { string56, fn_car, 1, 1 },
  { string57, fn_car, 1, 1 },
  { string58, fn_cdr, 1, 1 },
  { string59, fn_cdr, 1, 1 },
  { string60, fn_caar, 1, 1 },
  { string61, fn_cadr, 1, 1 },
  { string62, fn_cadr, 1, 1 },
  { string63, fn_cdar, 1, 1 },
  { string64, fn_cddr, 1, 1 },
  { string65, fn_caaar, 1, 1 },
  { string66, fn_caadr, 1, 1 },
  { string67, fn_cadar, 1, 1 },
  { string68, fn_caddr, 1, 1 },
  { string69, fn_caddr, 1, 1 },
  { string70, fn_cdaar, 1, 1 },
  { string71, fn_cdadr, 1, 1 },
  { string72, fn_cddar, 1, 1 },
  { string73, fn_cdddr, 1, 1 },
  { string74, fn_length, 1, 1 },
  { string75, fn_list, 0, 127 },
  { string76, fn_reverse, 1, 1 },
  { string77, fn_nth, 2, 2 },
  { string78, fn_assoc, 2, 2 },
  { string79, fn_member, 2, 2 },
  { string80, fn_apply, 2, 127 },
  { string81, fn_funcall, 1, 127 },
  { string82, fn_append, 0, 127 },
  { string83, fn_mapc, 2, 127 },
  { string84, fn_mapcar, 2, 127 },
  { string85, fn_mapcan, 2, 127 },
  { string86, fn_add, 0, 127 },
  { string87, fn_subtract, 1, 127 },
  { string88, fn_multiply, 0, 127 },
  { string89, fn_divide, 1, 127 },
  { string90, fn_mod, 2, 2 },
  { string91, fn_oneplus, 1, 1 },
  { string92, fn_oneminus, 1, 1 },
  { string93, fn_abs, 1, 1 },
  { string94, fn_random, 1, 1 },
  { string95, fn_maxfn, 1, 127 },
  { string96, fn_minfn, 1, 127 },
  { string97, fn_noteq, 1, 127 },
  { string98, fn_numeq, 1, 127 },
  { string99, fn_less, 1, 127 },
  { string100, fn_lesseq, 1, 127 },
  { string101, fn_greater, 1, 127 },
  { string102, fn_greatereq, 1, 127 },
  { string103, fn_plusp, 1, 1 },
  { string104, fn_minusp, 1, 1 },
  { string105, fn_zerop, 1, 1 },
  { string106, fn_oddp, 1, 1 },
  { string107, fn_evenp, 1, 1 },
  { string108, fn_integerp, 1, 1 },
  { string109, fn_numberp, 1, 1 },
  { string110, fn_floatfn, 1, 1 },
  { string111, fn_floatp, 1, 1 },
  { string112, fn_sin, 1, 1 },
  { string113, fn_cos, 1, 1 },
  { string114, fn_tan, 1, 1 },
  { string115, fn_asin, 1, 1 },
  { string116, fn_acos, 1, 1 },
  { string117, fn_atan, 1, 2 },
  { string118, fn_sinh, 1, 1 },
  { string119, fn_cosh, 1, 1 },
  { string120, fn_tanh, 1, 1 },
  { string121, fn_exp, 1, 1 },
  { string122, fn_sqrt, 1, 1 },
  { string123, fn_log, 1, 2 },
  { string124, fn_expt, 2, 2 },
  { string125, fn_ceiling, 1, 2 },
  { string126, fn_floor, 1, 2 },
  { string127, fn_truncate, 1, 2 },
  { string128, fn_round, 1, 2 },
  { string129, fn_char, 2, 2 },
  { string130, fn_charcode, 1, 1 },
  { string131, fn_codechar, 1, 1 },
  { string132, fn_characterp, 1, 1 },
  { string133, fn_stringp, 1, 1 },
  { string134, fn_stringeq, 2, 2 },
  { string135, fn_stringless, 2, 2 },
  { string136, fn_stringgreater, 2, 2 },
  { string137, fn_sort, 2, 2 },
  { string138, fn_stringfn, 1, 1 },
  { string139, fn_concatenate, 1, 127 },
  { string140, fn_subseq, 2, 3 },
  { string141, fn_readfromstring, 1, 1 },
  { string142, fn_princtostring, 1, 1 },
  { string143, fn_prin1tostring, 1, 1 },
  { string144, fn_logand, 0, 127 },
  { string145, fn_logior, 0, 127 },
  { string146, fn_logxor, 0, 127 },
  { string147, fn_lognot, 1, 1 },
  { string148, fn_ash, 2, 2 },
  { string149, fn_logbitp, 2, 2 },
  { string150, fn_eval, 1, 1 },
  { string151, fn_globals, 0, 0 },
  { string152, fn_locals, 0, 0 },
  { string153, fn_makunbound, 1, 1 },
  { string154, fn_break, 0, 0 },
  { string155, fn_read, 0, 1 },
  { string156, fn_prin1, 1, 2 },
  { string157, fn_print, 1, 2 },
  { string158, fn_princ, 1, 2 },
  { string159, fn_terpri, 0, 1 },
  { string160, fn_readbyte, 0, 2 },
  { string161, fn_readline, 0, 1 },
  { string162, fn_writebyte, 1, 2 },
  { string163, fn_writestring, 1, 2 },
  { string164, fn_writeline, 1, 2 },
  { string165, fn_restarti2c, 1, 2 },
  { string166, fn_gc, 0, 0 },
  { string167, fn_room, 0, 0 },
  { string168, fn_saveimage, 0, 1 },
  { string169, fn_loadimage, 0, 1 },
  { string170, fn_cls, 0, 0 },
  { string171, fn_pinmode, 2, 2 },
  { string172, fn_digitalread, 1, 1 },
  { string173, fn_digitalwrite, 2, 2 },
  { string174, fn_analogread, 1, 1 },
  { string175, fn_analogwrite, 2, 2 },
  { string176, fn_delay, 1, 1 },
  { string177, fn_millis, 0, 0 },
  { string178, fn_sleep, 1, 1 },
  { string179, fn_note, 0, 3 },
  { string180, fn_edit, 1, 1 },
  { string181, fn_pprint, 1, 2 },
  { string182, fn_pprintall, 0, 0 },
  { string183, fn_require, 1, 1 },
  { string184, fn_listlibrary, 0, 0 },
  { string185, fn_make_debouncer, 1, 1 },
  { string186, fn_update_debouncers, 0, 0 },
  { string187, fn_debouncer_value, 1, 1 },
  { string188, fn_debouncer_rose, 1, 1 },
  { string189, fn_debouncer_fell, 1, 1 },
  { string190, fn_init_lis3dh, 0, 0 },
  { string191, fn_lis3dh_bump, 0, 0 },
  { string192, fn_lis3dh_acceleration, 0, 0 },
};

// Table lookup functions

int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
    if (strcasecmp(n, (char*)lookup_table[entry].string) == 0)
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}

int longsymbol (char *buffer) {
  char *p = SymbolTable;
  int i = 0;
  while (strcasecmp(p, buffer) != 0) {p = p + strlen(p) + 1; i++; }
  if (p == buffer) {
    // Add to symbol table?
    char *newtop = SymbolTop + strlen(p) + 1;
    if (SYMBOLTABLESIZE - (newtop - SymbolTable) < BUFFERSIZE) error2(0, PSTR("no room for long symbols"));
    SymbolTop = newtop;
  }
  if (i > 1535) error2(0, PSTR("Too many long symbols"));
  return i + 64000; // First number unused by radix40
}

intptr_t lookupfn (symbol_t name) {
  return (intptr_t)lookup_table[name].fptr;
}

uint8_t lookupmin (symbol_t name) {
  return lookup_table[name].min;
}

uint8_t lookupmax (symbol_t name) {
  return lookup_table[name].max;
}

char *lookupbuiltin (symbol_t name) {
  char *buffer = SymbolTop;
  strcpy(buffer, (char *)lookup_table[name].string);
  return buffer;
}

char *lookupsymbol (symbol_t name) {
  char *p = SymbolTable;
  int i = name - 64000;
  while (i > 0 && p < SymbolTop) {p = p + strlen(p) + 1; i--; }
  if (p == SymbolTop) return NULL; else return p;
}

void deletesymbol (symbol_t name) {
  char *p = lookupsymbol(name);
  if (p == NULL) return;
  char *q = p + strlen(p) + 1;
  *p = '\0'; p++;
  while (q < SymbolTop) *(p++) = *(q++);
  SymbolTop = p;
}

void testescape () {
  if (Serial.read() == '~') error2(0, PSTR("escape!"));
}

// Main evaluator

uint8_t End;

object *eval (object *form, object *env) {
  int TC=0;
  EVAL:
  yield(); // Needed on ESP8266 to avoid Soft WDT Reset
  // Enough space?
  if (End != 0xA5) error2(0, PSTR("Stack overflow"));
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("Escape!"));}
  #if defined (serialmonitor)
  testescape();
  #endif

  if (form == NULL) return nil;

  if (integerp(form) || floatp(form) || characterp(form) || stringp(form)) return form;

  if (symbolp(form)) {
    symbol_t name = form->name;
    if (name == NIL) return nil;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (name <= ENDFUNCTIONS) return form;
    error(0, PSTR("undefined"), form);
  }

  // It's a list
  object *function = car(form);
  object *args = cdr(form);
  object *unevaled_args = cdr(form);

  if (function == NULL) error(0, PSTR("illegal function"), nil);
  if (!listp(args)) error(0, PSTR("can't evaluate a dotted pair"), args);

  // List starts with a symbol?
  if (symbolp(function)) {
    symbol_t name = function->name;

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      object *assigns = first(args);
      object *forms = cdr(args);
      object *newenv = env;
      push(newenv, GCStack);
      while (assigns != NULL) {
        object *assign = car(assigns);
        if (!consp(assign)) push(cons(assign,nil), newenv);
        else if (cdr(assign) == NULL) push(cons(first(assign),nil), newenv);
        else push(cons(first(assign),eval(second(assign),env)), newenv);
        car(GCStack) = newenv;
        if (name == LETSTAR) env = newenv;
        assigns = cdr(assigns);
      }
      env = newenv;
      pop(GCStack);
      form = tf_progn(forms,env);
      TC = TCstart;
      goto EVAL;
    }

    if (name == LAMBDA) {
      if (env == NULL) return form;
      object *envcopy = NULL;
      while (env != NULL) {
        object *pair = first(env);
        if (pair != NULL) push(pair, envcopy);
        env = cdr(env);
      }
      return cons(symbol(CLOSURE), cons(envcopy,args));
    }

    if (name < SPECIAL_FORMS) error2((int)function, PSTR("can't be used as a function"));

    if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }
  }

  // Evaluate the parameters - result in head
  object *fname = car(form);
  int TCstart = TC;
  object *head = cons(eval(car(form), env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL){
    object *obj = cons(eval(car(form),env),NULL);
    cdr(tail) = obj;
    tail = obj;
    form = cdr(form);
    nargs++;
  }

  function = car(head);
  args = cdr(head);

  if (symbolp(function)) {
    symbol_t name = function->name;
    if (name >= ENDFUNCTIONS) error(0, PSTR("not valid here"), fname);
    if (nargs<lookupmin(name)) error2(name, PSTR("has too few arguments"));
    if (nargs>lookupmax(name)) error2(name, PSTR("has too many arguments"));
    object *result = ((fn_ptr_type)lookupfn(name))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function) && issymbol(car(function), LAMBDA)) {
    form = closure(TCstart, fname->name, NULL, cdr(function), args, &env);
    pop(GCStack);
    int trace = tracing(fname->name);
    if (trace) {
      object *result = eval(form, env);
      indent((--(TraceDepth[trace-1]))<<1, pserial);
      pint(TraceDepth[trace-1], pserial);
      pserial(':'); pserial(' ');
      printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
      printobject(result, pserial); pln(pserial);
      return result;
    } else {
      TC = 1;
      goto EVAL;
    }
  }

  if (consp(function) && issymbol(car(function), CLOSURE)) {
    function = cdr(function);
    form = closure(TCstart, fname->name, car(function), cdr(function), args, &env);
    pop(GCStack);
    TC = 1;
    goto EVAL;
  }

  if (consp(function) && (issymbol(car(function), MACRO))) {
    // has the form (MACRO (param...) expr...)
    object *definition = cdr(function);
    object *params = car(definition);
    object *body = car(cdr(definition));
    Serial.print("Macro: params: ");
    printobject(params, pserial);
    Serial.print(" - args: ");
    printobject(args, pserial);
    Serial.print(" - body: ");
    printobject(body, pserial);
    Serial.println("");

    object *newenv = env;
    push(newenv, GCStack);
    object *p;
    object *a;
    for (p = params, a = args; p != NULL; p = cdr(p), a = cdr(a)) {
      //     push(cons(car(p), eval(car(a), env)), newenv);
     push(cons(car(p), car(a)), newenv);
     printobject(car(car(newenv)), pserial);
     Serial.print(" => ");
     printobject(cdr(car(newenv)), pserial);
     Serial.println("");
     car(GCStack) = newenv;
    }
    form = expand(body, newenv);
    pop(GCStack);
    goto EVAL;
  }

  error(0, PSTR("illegal function"), fname); return nil;
}

// Print functions

inline int maxbuffer (char *buffer) {
  return SYMBOLTABLESIZE-(buffer-SymbolTable)-1;
}

void pserial (char c) {
  LastPrint = c;
  if (c == '\n') Serial.write('\r');
  Serial.write(c);
}

const char ControlCodes[] PROGMEM = "Null\0SOH\0STX\0ETX\0EOT\0ENQ\0ACK\0Bell\0Backspace\0Tab\0Newline\0VT\0"
"Page\0Return\0SO\0SI\0DLE\0DC1\0DC2\0DC3\0DC4\0NAK\0SYN\0ETB\0CAN\0EM\0SUB\0Escape\0FS\0GS\0RS\0US\0Space\0";

void pcharacter (char c, pfun_t pfun) {
  if (!tstflag(PRINTREADABLY)) pfun(c);
  else {
    pfun('#'); pfun('\\');
    if (c > 32) pfun(c);
    else {
      const char *p = ControlCodes;
      while (c > 0) {p = p + strlen(p) + 1; c--; }
      pfstring(p, pfun);
    }
  }
}

void pstring (char *s, pfun_t pfun) {
  while (*s) pfun(*s++);
}

void printstring (object *form, pfun_t pfun) {
  if (tstflag(PRINTREADABLY)) pfun('"');
  form = cdr(form);
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (tstflag(PRINTREADABLY) && (ch == '"' || ch == '\\')) pfun('\\');
      if (ch) pfun(ch);
    }
    form = car(form);
  }
  if (tstflag(PRINTREADABLY)) pfun('"');
}

void pfstring (const char *s, pfun_t pfun) {
  int p = 0;
  while (1) {
    char c = s[p++];
    if (c == 0) return;
    pfun(c);
  }
}

void pint (int i, pfun_t pfun) {
  int lead = 0;
  #if INT_MAX == 32767
  int p = 10000;
  #else
  int p = 1000000000;
  #endif
  if (i<0) pfun('-');
  for (int d=p; d>0; d=d/10) {
    int j = i/d;
    if (j!=0 || lead || d==1) { pfun(abs(j)+'0'); lead=1;}
    i = i - j*d;
  }
}

void pmantissa (float f, pfun_t pfun) {
  int sig = floor(log10(f));
  int mul = pow(10, 5 - sig);
  int i = round(f * mul);
  boolean point = false;
  if (i == 1000000) { i = 100000; sig++; }
  if (sig < 0) {
    pfun('0'); pfun('.'); point = true;
    for (int j=0; j < - sig - 1; j++) pfun('0');
  }
  mul = 100000;
  for (int j=0; j<7; j++) {
    int d = (int)(i / mul);
    pfun(d + '0');
    i = i - d * mul;
    if (i == 0) {
      if (!point) {
        for (int k=j; k<sig; k++) pfun('0');
        pfun('.'); pfun('0');
      }
      return;
    }
    if (j == sig && sig >= 0) { pfun('.'); point = true; }
    mul = mul / 10;
  }
}

void pfloat (float f, pfun_t pfun) {
  if (isnan(f)) { pfstring(PSTR("NaN"), pfun); return; }
  if (f == 0.0) { pfun('0'); return; }
  if (isinf(f)) { pfstring(PSTR("Inf"), pfun); return; }
  if (f < 0) { pfun('-'); f = -f; }
  // Calculate exponent
  int e = 0;
  if (f < 1e-3 || f >= 1e5) {
    e = floor(log(f) / 2.302585); // log10 gives wrong result
    f = f / pow(10, e);
  }

  pmantissa (f, pfun);

  // Exponent
  if (e != 0) {
    pfun('e');
    pint(e, pfun);
  }
}

inline void pln (pfun_t pfun) {
  pfun('\n');
}

void pfl (pfun_t pfun) {
  if (LastPrint != '\n') pfun('\n');
}

void printobject (object *form, pfun_t pfun){
  if (form == NULL) pfstring(PSTR("nil"), pfun);
  else if (listp(form) && issymbol(car(form), CLOSURE)) pfstring(PSTR("<closure>"), pfun);
  else if (listp(form)) {
    pfun('(');
    printobject(car(form), pfun);
    form = cdr(form);
    while (form != NULL && listp(form)) {
      pfun(' ');
      printobject(car(form), pfun);
      form = cdr(form);
    }
    if (form != NULL) {
      pfstring(PSTR(" . "), pfun);
      printobject(form, pfun);
    }
    pfun(')');
  } else if (integerp(form)) pint(form->integer, pfun);
  else if (floatp(form)) pfloat(form->single_float, pfun);
  else if (symbolp(form)) { if (form->name != NOTHING) pstring(symbolname(form->name), pfun); }
  else if (characterp(form)) pcharacter(form->integer, pfun);
  else if (stringp(form)) printstring(form, pfun);
  else if (streamp(form)) {
    pfstring(PSTR("<"), pfun);
    if ((form->integer)>>8 == SPISTREAM) pfstring(PSTR("spi"), pfun);
    else if ((form->integer)>>8 == I2CSTREAM) pfstring(PSTR("i2c"), pfun);
    else if ((form->integer)>>8 == SDSTREAM) pfstring(PSTR("sd"), pfun);
    else pfstring(PSTR("serial"), pfun);
    pfstring(PSTR("-stream "), pfun);
    pint(form->integer & 0xFF, pfun);
    pfun('>');
  } else
    error2(0, PSTR("Error in print"));
}

// Read functions

int glibrary () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = LispLibrary[GlobalStringIndex++];
  return (c != 0) ? c : -1; // -1?
}

void loadfromlibrary (object *env) {
  GlobalStringIndex = 0;
  object *line = read(glibrary);
  while (line != NULL) {
    eval(line, env);
    line = read(glibrary);
  }
}

int gserial () {
  if (LastChar) {
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  while (!Serial.available());
  char temp = Serial.read();
  if (temp != '\n') pserial(temp);
  return temp;
}

object *nextitem (gfun_t gfun) {
  int ch = gfun();
  while(isspace(ch)) ch = gfun();

  if (ch == ';') {
    while(ch != '(') ch = gfun();
    ch = '(';
  }
  if (ch == '\n') ch = gfun();
  if (ch == -1) return nil;
  if (ch == ')') return (object *)KET;
  if (ch == '(') return (object *)BRA;
  if (ch == '\'') return (object *)QUO;
  if (ch == '`') return (object *)BACKTICK;
  if (ch == ',') return (object *)COMMA;
  if (ch == '@') return (object *)COMMAAT;

  // Parse string
  if (ch == '"') return readstring('"', gfun);

  // Parse symbol, character, or number
  int index = 0, base = 10, sign = 1;
  char *buffer = SymbolTop;
  int bufmax = maxbuffer(buffer); // Max index
  unsigned int result = 0;
  boolean isfloat = false;
  float fresult = 0.0;

  if (ch == '+') {
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '-') {
    sign = -1;
    buffer[index++] = ch;
    ch = gfun();
  } else if (ch == '.') {
    buffer[index++] = ch;
    ch = gfun();
    if (ch == ' ') return (object *)DOT;
    isfloat = true;
  } else if (ch == '#') {
    ch = gfun() & ~0x20;
    if (ch == '\\') base = 0; // character
    else if (ch == 'B') base = 2;
    else if (ch == 'O') base = 8;
    else if (ch == 'X') base = 16;
    else if (ch == 0x07) return nextitem(gfun);
    else error2(0, PSTR("Illegal character after #"));
    ch = gfun();
  /* } else if (ch == ',') { */
  /*   ch = gfun(); */
  /*   if (ch == '@') { */
  /*     return (object *)COMMAAT; */
  /*   } else { */
  /*     return (object *)COMMA; */
  /*   } */
  }
  int valid; // 0=undecided, -1=invalid, +1=valid
  if (ch == '.') valid = 0; else if (digitvalue(ch)<base) valid = 1; else valid = -1;
  boolean isexponent = false;
  int exponent = 0, esign = 1;
  buffer[2] = '\0'; // In case symbol is one letter
  float divisor = 10.0;

  while(!isspace(ch) && ch != ')' && ch != '(' && index < bufmax) {
    buffer[index++] = ch;
    if (base == 10 && ch == '.' && !isexponent) {
      isfloat = true;
      fresult = result;
    } else if (base == 10 && (ch == 'e' || ch == 'E')) {
      if (!isfloat) { isfloat = true; fresult = result; }
      isexponent = true;
      if (valid == 1) valid = 0; else valid = -1;
    } else if (isexponent && ch == '-') {
      esign = -esign;
    } else if (isexponent && ch == '+') {
    } else {
      int digit = digitvalue(ch);
      if (digitvalue(ch)<base && valid != -1) valid = 1; else valid = -1;
      if (isexponent) {
        exponent = exponent * 10 + digit;
      } else if (isfloat) {
        fresult = fresult + digit / divisor;
        divisor = divisor * 10.0;
      } else {
        result = result * base + digit;
      }
    }
    ch = gfun();
  }

  buffer[index] = '\0';
  if (ch == ')' || ch == '(') LastChar = ch;
  if (isfloat && valid == 1) return makefloat(fresult * sign * pow(10, exponent * esign));
  else if (valid == 1) {
    if (base == 10 && result > ((unsigned int)INT_MAX+(1-sign)/2))
      return makefloat((float)result*sign);
    return number(result*sign);
  } else if (base == 0) {
    if (index == 1) return character(buffer[0]);
    const char* p = ControlCodes; char c = 0;
    while (c < 33) {
      if (strcasecmp(buffer, p) == 0) return character(c);
      p = p + strlen(p) + 1; c++;
    }
    error2(0, PSTR("Unknown character"));
  }

  int x = builtin(buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return newsymbol(x);
  else if (index < 4 && valid40(buffer)) return newsymbol(pack40(buffer));
  else return newsymbol(longsymbol(buffer));
}

object *readrest (gfun_t gfun) {
  object *item = nextitem(gfun);
  object *head = NULL;
  object *tail = NULL;

  while (item != (object *)KET) {
    if (item == (object *)BRA) {
      item = readrest(gfun);
    } else if (item == (object *)QUO) {
      item = cons(symbol(QUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)BACKTICK) {
      item = cons(symbol(QUASIQUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)COMMA) {
      item = cons(symbol(UNQUOTE), cons(read(gfun), NULL));
    } else if (item == (object *)COMMAAT) {
      item = cons(symbol(UNQUOTESPLICING), cons(read(gfun), NULL));
    } else if (item == (object *)DOT) {
      tail->cdr = read(gfun);
      if (readrest(gfun) != NULL) error2(0, PSTR("malformed list"));
      return head;
    } else {
      object *cell = cons(item, NULL);
      if (head == NULL) head = cell;
      else tail->cdr = cell;
      tail = cell;
      item = nextitem(gfun);
    }
  }
  return head;
}

object *read (gfun_t gfun) {
  object *item = nextitem(gfun);
  if (item == (object *)KET) error2(0, PSTR("incomplete list"));
  if (item == (object *)BRA) return readrest(gfun);
  if (item == (object *)DOT) return read(gfun);
  if (item == (object *)QUO) return cons(symbol(QUOTE), cons(read(gfun), NULL));
  if (item == (object *)BACKTICK) return cons(symbol(QUASIQUOTE), cons(read(gfun), NULL));
  if (item == (object *)COMMA) return cons(symbol(UNQUOTE), cons(read(gfun), NULL));
  if (item == (object *)COMMAAT) return cons(symbol(UNQUOTESPLICING), cons(read(gfun), NULL));
  return item;
}

// Setup

void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  pfstring(PSTR("uLisp 2.9 "), pserial); pln(pserial);
}

// Read/Evaluate/Print loop

void repl (object *env) {
  for (;;) {
    randomSeed(micros());
    gc(NULL, env);
    #if defined (printfreespace)
    pint(Freespace, pserial);
    #endif
    if (BreakLevel) {
      pfstring(PSTR(" : "), pserial);
      pint(BreakLevel, pserial);
    }
    pfstring(PSTR("> "), pserial);
    object *line = read(gserial);
    if (BreakLevel && line == nil) { pln(pserial); return; }
    if (line == (object *)KET) error2(0, PSTR("unmatched right bracket"));
    push(line, GCStack);
    pfl(pserial);
    line = eval(line, env);
    pfl(pserial);
    printobject(line, pserial);
    pop(GCStack);
    pfl(pserial);
    pln(pserial);
  }
}

void loop () {
  End = 0xA5;      // Canary to check stack
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    volatile int autorun = 12; // Fudge to keep code size the same
    #else
    volatile int autorun = 13;
    #endif
    if (autorun == 12) autorunimage();
  }
  // Come here after error
  delay(100); while (Serial.available()) Serial.read();
  for (int i=0; i<TRACEMAX; i++) TraceDepth[i] = 0;
  #if defined(sdcardsupport)
  SDpfile.close(); SDgfile.close();
  #endif
  #if defined(lisplibrary)
  if (!tstflag(LIBRARYLOADED)) { setflag(LIBRARYLOADED); loadfromlibrary(NULL); }
  #endif
  repl(NULL);
}
