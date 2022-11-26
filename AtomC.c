#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define SIZE 1000
#define err(s) { printf("Eroare la linia %d : %s", atomConsumat->linie, s); exit(EXIT_FAILURE); }

typedef enum coduriAtom {
	ID, VAR, FUNCTION, IF, ELSE, WHILE, END, RETURN,
	TYPE_INT, TYPE_REAL, TYPE_STR, INT, REAL, STR,
	COMMA, COLON, SEMICOLON, LPAR, RPAR, FINISH,
	ADD, SUB, MUL, DIV, AND, OR, NOT, ASSIGN, EQUAL,
	NOTEQ, LESS
}codAtom;

const char* coduri_Atom[] = { "ID", "VAR", "FUNCTION", "IF", "ELSE", "WHILE", "END", "RETURN",
	"TYPE_INT", "TYPE_REAL", "TYPE_STR", "INT", "REAL", "STR",
	"COMMA", "COLON", "SEMICOLON", "LPAR", "RPAR", "FINISH",
	"ADD", "SUB", "MUL", "DIV", "AND", "OR", "NOT", "ASSIGN", "EQUAL",
	"NOTEQ", "LESS" };

typedef struct atom {
	codAtom cod;
	int linie;
	union {
		int i;
		double r;
		char s[100];
	};
}A;

A atomi[SIZE];
A* atomCrt = &atomi;
A* atomConsumat = NULL;

int nrLinie = 1;
int nrAtomi = 0;
char oldCh = -1; //folosit pt a memora caractere care nu sunt cuvinte speciale/id/str/int/real

void addAtom(codAtom cod, char* s) {
	A atom;
	atom.linie = nrLinie; atom.cod = cod;
	if (cod == ID || cod == STR)
		strcpy(atom.s, s);
	if (cod == INT)
		atom.i = atoi(s);
	if (cod == REAL)
		atom.r = atof(s);
	atomi[nrAtomi++] = atom;
}

void afisareAtomi() {
	int i, j;
	printf("\nLista de atomi:\n\n");

	for (i = 1; i <= nrLinie; i++) {
		printf("Linia %d :", i);
		for (j = 0; j < nrAtomi; j++)
			if (atomi[j].linie == i) {
				printf(" %s", coduri_Atom[atomi[j].cod]);
				if (atomi[j].cod == ID || atomi[j].cod == STR)
					printf(":%s", atomi[j].s);
				else if (atomi[j].cod == INT)
					printf(":%d", atomi[j].i);
				else if (atomi[j].cod == REAL)
					printf(":%g", atomi[j].r);
			}
		printf("\n");
	}
}

int consume(codAtom cod) {
	if (atomCrt->cod == cod) {
		atomConsumat = atomCrt;
		atomCrt++;
		return 1;
	}
	return 0;
}

int baseType() {
	if (consume(TYPE_INT))
		return 1;
	if (consume(TYPE_REAL))
		return 1;
	if (consume(TYPE_STR))
		return 1;

	return 0;
}

int defVar() {
	if (consume(VAR)) {
		if (consume(ID)) {
			if (consume(COLON)) {
				if (baseType()) {
					if (consume(SEMICOLON)) {
						return 1;
					}
					else {
						err("lipseste ; la finalul declaratiei de variabila.\n");
						return 0;
					}
				}
				else {
					err("lipseste tipul variabilei.\n");
					return 0;
				}
			}
			else {
				err("lipseste : dupa numele variabilei.\n");
				return 0;
			}
		}
		else {
			err("lipseste numele variabilei.\n");
			return 0;
		}
	}
	else
		return 0;
}

int funcParams() {
	if (funcParam())
	{
		for (;;) {
			if (consume(COMMA)) {
				if (funcParam()) {}
				else err("lipsesc parametrii de dupa ,\n");
			}
			break;
		}
	}
	return 1;
}

int funcParam() {
	if (consume(ID)) {
		if (consume(COLON)) {
			if (baseType())
				return 1;
			else {
				err("lipseste tipul variabilei transmisa ca si parametru.\n");
				return 0;
			}
		}
		else {
			err("lipseste : din declaratia parametrilor.\n");
			return 0;
		}
	}
	return 0;
}

int defFunc() {
	if (consume(FUNCTION)) {
		if (consume(ID)) {
			if (consume(LPAR)) {
				if (funcParams()) {
					if (consume(RPAR)) {
						if (consume(COLON)) {
							if (baseType()) {
								for (;;) {
									if (defVar()) {}
									else break;
								}
								if (block()) {
									if (consume(END))
										return 1;
									else {
										err("lipseste END pentru a marca finalul functiei.\n");
										return 0;
									}
								}
								else {
									err("lipseste blocul de instructiuni din cadrul functiei.\n");
									return 0;
								}
							}
							else {
								err("lipseste tipul returnat de functie\n");
								return 0;
							}
						}
						else {
							err("lipseste : dupa inchiderea declararii parametrilor functiei\n");
							return 0;
						}
					}
					else {
						err("lipseste ) de la declararea parametrilor functiei.\n");
						return 0;
					}
				}
				else {
					err("lipsesc parametrii functiei.\n");
					return 0;
				}
			}
			else {
				err("lipseste ( de la declararea parametrilor functiei.\n");
				return 0;
			}
		}
		else {
			err("lipseste numele functiei.\n");
			return 0;
		}
	}									
	return 0;
}

int block() {
	if (instr()) {
		for (;;) {
			if (instr()) {}
			else break;
		}
		return 1;
	}
	return 0;
}

int expr() {
	if (exprLogic())
		return 1;
	return 0;
}

int factor() {
	if (consume(INT))
		return 1;

	if (consume(REAL))
		return 1;

	if (consume(STR))
		return 1;

	if (consume(LPAR)) {
		if (expr()) {
			if (consume(RPAR))
				return 1;
			else {
				err("lipseste ) la finalul expresiei.\n");
				return 0;
			}
		}
		else {
			err("lipseste expresia din paranteza.\n");
			return 0;
		}
	}
			

	if (consume(ID))
	{
		if (consume(LPAR))
		{
			if (expr())
			{
				for (;;)
				{
					if (consume(COMMA))
					{
						if (expr()) {}
						else {
							err("lipseste parametrul de dupa virgula.\n");
							return 0;
						}
					}
					break;
				}
				if (consume(RPAR)) {}
				else {
					err("lipseste ) la sfarsitul apelului functiei.\n");
					return 0;
				}
			}
			else {
				err("lipseste expresia de dupa (.\n");
				return 0;
			}
		}
		return 1;
	}

	return 0;
}

int exprPrefix() {
	if (consume(SUB)) {}
	if (consume(NOT)) {}

	if (factor())
		return 1;

	return 0;
}

int exprMul() {

	if (exprPrefix()) {
		if (consume(MUL)) {
			if (exprPrefix())
				return 1;
			else {
				err("lipseste valoarea din cadrul inmultirii.\n");
				return 0;
			}
		}
		if (consume(DIV)) {
			if (exprPrefix())
				return 1;
			else {
				err("lipseste valoarea din cadrul impartirii.\n");
				return 0;
			}
		}
		return 1;
	}
	return 0;
}

int exprAdd() {
	if (exprMul()) {
		if (consume(ADD)) {
			if (exprMul())
				return 1;
			else {
				err("lipseste valoarea de dupa +.\n");
				return 0;
			}
		}
		if (consume(SUB)) {
			if (exprMul())
				return 1;
			else {
				err("lipseste valoarea de dupa -.\n");
				return 0;
			}
		}
		return 1;
	}
	return 0;
}

int exprComp() {
	if (exprAdd()) {
		if (consume(LESS)) {
			if (exprAdd())
				return 1;
			else {
				err("lipseste valoarea cu care se compara, dupa <.\n");
				return 0;
			}
		}
		if (consume(EQUAL)) {
			if (exprAdd())
				return 1;
			else {
				err("lipseste valoarea de dupa egal.\n");
				return 0;
			}
		}
		return 1;
	}
	return 0;
}

int exprAssign() {
	if (consume(ID)) {
		if (consume(ASSIGN)) {
			if (exprComp())
				return 1;
			else {
				err("lipseste valoarea asignata.\n");
				return 0;
			}
		}
		else
			atomCrt = atomConsumat;
	}

	if (exprComp())
		return 1;
	return 0;
}

int exprLogic() {
	if (exprAssign()) {
		for (;;) {
			if (consume(AND) | consume(OR)) {
				if (exprAssign()) {}
				else {
					err("lipseste operandul din expresia logica.\n");
					return 0;
				}
			}
			else break;
		}
		return 1;
	}
}

int instr() {
	if (expr()) {}

	if (consume(SEMICOLON))
		return 1;

	if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr()) {
				if (consume(RPAR)) {
					if (block()) {
						if (consume(ELSE)) {
							if (block()) {
								if (consume(END)) {
									return 1;
								}
								else {
									err("lipseste END pentru a marca sfarsitul IF-ului.\n");
									return 0;
								}
							}
							else {
								err("lipseste blocul de instructiuni de dupa ELSE.\n");
								return 0;
							}
						}
						if (consume(END))
							return 1;
						else {
							err("lipseste END pentru a marca sfarsitul IF-ului.\n");
							return 0;
						}
					}
					else {
						err("lipseste blocul de instructiuni din cadrul IF-ului.\n");
						return 0;
					}
				}
				else {
					err("lipseste ) de la IF.\n");
					return 0;
				}
			}
			else {
				err("lipseste expresia din IF.\n");
				return 0;
			}
		}
		else {
			err("lipseste ( de la IF.\n");
			return 0;
		}
	}

	else if (consume(RETURN)) {
		if (expr()) {
			if (consume(SEMICOLON))
				return 1;
			else err("lipseste ; dupa numele variabilei returnate.\n");
		}
		else err("lipseste expresia/variabila returnata.\n");
	}

	else if (consume(WHILE)) {
		if (consume(LPAR)) {
			if (expr()) {
				if (consume(RPAR)) {
					if (block()) {
						if (consume(END))
							return 1;
						else {
							err("lipseste END pentru a marca sfarsitul blocului WHILE.\n");
							return 0;
						}
					}
					else {
						err("lipseste blocul de instructiuni din WHILE.\n");
						return 0;
					}
				}
				else {
					err("lipseste ) de la WHILE.\n");
					return 0;
				}
			}
			else {
				err("lipseste expresia care se verifica in WHILE.\n");
				return 0;
			}
		}
		else {
			err("lipseste ( de la WHILE.\n");
			return 0;
		}
	}
	return 0;
}

int program() {
	for (;;) {
		if (defVar()) {}
		else if (defFunc()) {}
		else if (block()) {}
		else break;
	}
	if (consume(FINISH))
		return 1;
}

codAtom getNextTk(FILE* f) {
	int state = 0;
	int n = 0;
	char buffer[100] = { '\0' };
	char ch;

	for (;;) {
		if (oldCh == -1)
			ch = fgetc(f);
		else {
			ch = oldCh;
			oldCh = -1;
		}
		switch (state) {
		case 0:
			if (isalpha(ch) || ch == '_') { buffer[n++] = ch; state = 1; } //ID
			else if (isdigit(ch)) { buffer[n++] = ch; state = 3; } //INT & REAL
			else if (ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t') {
				state = 0;
				if (ch == '\n') {
					nrLinie++;
				}
			}
			else if (ch == '\"') { buffer[n++] = ch; state = 8; } //STR
			else if (ch == ':') { state = 10; oldCh = ch; }
			else if (ch == ';') { state = 11; oldCh = ch; }
			else if (ch == ',') { state = 12; oldCh = ch; }
			else if (ch == ')') { state = 13; oldCh = ch; }
			else if (ch == '(') { state = 14; oldCh = ch; }
			else if (ch == EOF) { state = 15; oldCh = ch; }
			else if (ch == '+') { state = 16; oldCh = ch; }
			else if (ch == '-') { state = 17; oldCh = ch; }
			else if (ch == '*') { state = 18; oldCh = ch; }
			else if (ch == '/') { state = 19; oldCh = ch; }
			else if (ch == '&') { state = 20; }
			else if (ch == '|') { state = 22; }
			else if (ch == '!') { state = 24; }
			else if (ch == '=') { state = 27; }
			else if (ch == '<') { state = 30; }
			else if (ch == '#') { buffer[n++] = ch; state = 31; }
			break;

		case 1:
			if (isalpha(ch) || isdigit(ch) || ch == '_') {
				buffer[n++] = ch;
				state = 1;
			}
			else { state = 2; oldCh = ch; }
			break;

		case 2:
			oldCh = ch;
			buffer[n] = '\0';
			if (!strcmp(buffer, "function")) { addAtom(FUNCTION, buffer); return FUNCTION; }
			if (!strcmp(buffer, "var")) { addAtom(VAR, buffer); return VAR; }
			if (!strcmp(buffer, "if")) { addAtom(IF, buffer); return IF; }
			if (!strcmp(buffer, "else")) { addAtom(ELSE, buffer); return ELSE; }
			if (!strcmp(buffer, "while")) { addAtom(WHILE, buffer); return WHILE; }
			if (!strcmp(buffer, "end")) { addAtom(END, buffer); return END; }
			if (!strcmp(buffer, "return")) { addAtom(RETURN, buffer); return RETURN; }
			if (!strcmp(buffer, "int")) { addAtom(TYPE_INT, buffer); return TYPE_INT; }
			if (!strcmp(buffer, "real")) { addAtom(TYPE_REAL, buffer); return TYPE_REAL; }
			if (!strcmp(buffer, "str")) { addAtom(TYPE_STR, buffer); return TYPE_STR; }
			else {
				addAtom(ID, buffer);
				return ID;
			}
			break;
		case 3:
			if (isdigit(ch)) {
				buffer[n++] = ch;
				state = 3;
			}
			else if (ch == '.') {
				buffer[n++] = ch;
				state = 4;
			}
			else {
				oldCh = ch;
				state = 7;
			}
			break;

		case 4:
			if (isdigit(ch)) {
				buffer[n++] = ch;
				state = 5;
			}
			else {
				printf("Dupa punct trebuie sa urmeze o cifra.\n");
				exit(EXIT_FAILURE);
			}
			break;

		case 5:
			if (isdigit(ch)) {
				buffer[n++] = ch;
				state = 5;
			}
			else {
				oldCh = ch;
				state = 6;
			}
			break;

		case 6:
			oldCh = ch;
			buffer[n] = '\0';
			addAtom(REAL, buffer);
			return REAL;
			break;

		case 7:
			oldCh = ch;
			buffer[n] = '\0';
			addAtom(INT, buffer);
			return INT;
			break;

		case 8:
			if ((isalnum(ch) || ispunct(ch) || isspace(ch)) && ch != '\"') {
				buffer[n++] = ch;
				state = 8;
			}
			else if (ch == '\"') {
				buffer[n++] = ch;
				state = 9;
			}
			else {
				printf("Eroare: sir invalid.\n");
				exit(EXIT_FAILURE);
			}
			break;

		case 9:
			oldCh = ch;
			buffer[n] = '\0';
			addAtom(STR, buffer);
			return STR;
			break;

		case 10:
			addAtom(COLON, buffer);
			return COLON;
			break;

		case 11:
			addAtom(SEMICOLON, buffer);
			return SEMICOLON;
			break;

		case 12:
			addAtom(COMMA, buffer);
			return COMMA;
			break;

		case 13:
			addAtom(RPAR, buffer);
			return RPAR;
			break;

		case 14:
			addAtom(LPAR, buffer);
			return LPAR;
			break;

		case 15:
			addAtom(FINISH, buffer);
			return FINISH;
			break;

		case 16:
			addAtom(ADD, buffer);
			return ADD;
			break;

		case 17:
			addAtom(SUB, buffer);
			return SUB;
			break;

		case 18:
			addAtom(MUL, buffer);
			return MUL;
			break;

		case 19:
			addAtom(DIV, buffer);
			return DIV;
			break;

		case 20:
			if (ch == '&') {
				buffer[n++] = ch;
				state = 21;
			}
			else {
				printf("Eroare: lipseste al doilea &.\n");
				exit(EXIT_FAILURE);
			}
			break;

		case 21:
			addAtom(AND, buffer);
			return AND;
			break;

		case 22:
			if (ch == '|') {
				buffer[n++] = ch;
				state = 23;
			}
			else {
				printf("Eroare: lipseste al doilea |.\n");
				exit(EXIT_FAILURE);
			}
			break;

		case 23:
			addAtom(OR, buffer);
			return OR;
			break;

		case 24:
			if (ch == '=') {
				state = 26;
				oldCh = ch;
			}
			else {
				state = 25;
				oldCh = ch;
			}
			break;

		case 25:
			addAtom(NOT, buffer);
			return NOT;
			break;

		case 26:
			addAtom(NOTEQ, buffer);
			return NOTEQ;
			break;

		case 27:
			if (ch == '=') {
				state = 29;
				oldCh = ch;
			}
			else {
				state = 28;
				oldCh = ch;
			}
			break;

		case 28:
			oldCh = ch;
			addAtom(ASSIGN, buffer);
			return ASSIGN;
			break;

		case 29:
			oldCh = ch;
			addAtom(EQUAL, buffer);
			return EQUAL;
			break;

		case 30:
			oldCh = ch;
			addAtom(LESS, buffer);
			return LESS;
			break;

		case 31:
			if (ch != '\n' && ch != '\r') {
				buffer[n++] = ch; 
				state = 31;
			}
			else {
				return 50;
			}
			break;
		}
	}
}

int main() {
	FILE* f = fopen("1.q", "rb");
	char ch;
	if (!f) {
		printf("Nu se poate deschide fisierul.\n");
		exit(EXIT_FAILURE);
	}

	printf("Codul Quick:\n");

	while ((ch = fgetc(f)) != EOF)
		putchar(ch);
	printf("\n");

	rewind(f);

	while (getNextTk(f) != FINISH) {};

	fclose(f);

	afisareAtomi();

	if (program())
		printf("\nSintaxa OK.");

	return 0;
}