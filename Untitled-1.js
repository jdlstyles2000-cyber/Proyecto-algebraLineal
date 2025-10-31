/*
 * RESOLVEDOR MATEMÁTICO EN C++ (C++17)
 *
 * Funcionalidad:
 *  1) Sistemas de ecuaciones LINEALES ingresando ecuaciones como texto (ej: 2*x + 3*y = 5)
 *     - Si m=n: eliminación gaussiana con pivoteo parcial.
 *     - Si m!=n: solución de mínimos cuadrados (ecuaciones normales).
 *  2) Ecuación CUADRÁTICA: a*x^2 + b*x + c = 0 (con raíces reales o complejas).
 *  3) Sistemas GENERALES (no lineales) con Newton-Raphson y Jacobiano numérico.
 *
 * Entrada de ecuaciones: usa punto decimal (3.5), operadores + - * / ^, funciones sin, cos, tan,
 * exp, log/ln, sqrt, abs, asin, acos, atan, sinh, cosh, tanh y constantes 'pi', 'e'.
 * NO hay multiplicación implícita: escribe 2*x, sin(x), etc.
 *
 * Compilar (Linux/macOS):
 *   g++ -std=c++17 -O2 -o resuelve resuelve_matematica.cpp
 *
 * Compilar (Windows - MSVC):
 *   cl /std:c++17 /O2 resuelve_matematica.cpp /Fe:resuelve.exe
 */

#include <bits/stdc++.h>
using namespace std;

// ===================== Utilidades de cadena =====================
static inline string trim(const string& s) {
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a == string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b - a + 1);
}
static inline bool iequals(const string& a, const string& b) {
    if (a.size() != b.size()) return false;
    for (size_t i=0;i<a.size();++i)
        if (tolower(a[i]) != tolower(b[i])) return false;
    return true;
}

// ===================== Parser de expresiones =====================
// Soporta: números, variables (identificadores), funciones, + - * / ^, paréntesis y coma.
// Maneja unario '-' (negación). Shunting-yard -> RPN.

enum class TokType { Number, Ident, Op, LParen, RParen, Comma };

struct Token {
    TokType type;
    string text;   // tal cual
    double value;  // si es número
    int arity;     // si es función: aridad (1 o 2). Para operadores se usa precedencia aparte.
};

struct Lexer {
    string s; size_t i=0, n=0;
    explicit Lexer(string str): s(move(str)), n(s.size()) {}
    static bool isIdentStart(char c){ return isalpha((unsigned char)c) || c=='_'; }
    static bool isIdentChar(char c){ return isalnum((unsigned char)c) || c=='_' ; }

    vector<Token> lex() {
        vector<Token> out;
        while (i<n) {
            char c = s[i];
            if (isspace((unsigned char)c)) { ++i; continue; }
            if (isdigit((unsigned char)c) || (c=='.')) {
                size_t j=i; bool sawDot=(c=='.');
                ++j;
                while (j<n) {
                    char d=s[j];
                    if (isdigit((unsigned char)d)) { ++j; }
                    else if (d=='.' && !sawDot) { sawDot=true; ++j; }
                    else break;
                }
                // notación científica
                if (j<n && (s[j]=='e' || s[j]=='E')) {
                    size_t k=j+1;
                    if (k<n && (s[k]=='+' || s[k]=='-')) ++k;
                    bool hasExpDigit=false;
                    while (k<n && isdigit((unsigned char)s[k])) { hasExpDigit=true; ++k; }
                    if (hasExpDigit) j=k;
                }
                string num = s.substr(i, j-i);
                Token t{TokType::Number, num, stod(num), 0};
                out.push_back(t);
                i=j; continue;
            }
            if (isIdentStart(c)) {
                size_t j=i+1;
                while (j<n && isIdentChar(s[j])) ++j;
                string id = s.substr(i, j-i);
                Token t{TokType::Ident, id, 0.0, 0};
                out.push_back(t);
                i=j; continue;
            }
            if (c=='+'||c=='-'||c=='*'||c=='/'||c=='^') {
                string op(1,c);
                Token t{TokType::Op, op, 0.0, 0};
                out.push_back(t);
                ++i; continue;
            }
            if (c=='(') { out.push_back(Token{TokType::LParen,"(",0.0,0}); ++i; continue; }
            if (c==')') { out.push_back(Token{TokType::RParen,")",0.0,0}); ++i; continue; }
            if (c==',') { out.push_back(Token{TokType::Comma,",",0.0,0}); ++i; continue; }

            throw runtime_error(string("Carácter no reconocido: '")+c+"'");
        }
        return out;
    }
};

struct Parser {
    // Precedencias: ^ (4, derecha), * / (3, izq), + - (2, izq)
    static int prec(const string& op) {
        if (op=="^" || op=="u-") return 4; // unario '-' como alta
        if (op=="*" || op=="/") return 3;
        if (op=="+" || op=="-") return 2;
        return -1;
    }
    static bool rightAssoc(const string& op) {
        return (op=="^" || op=="u-");
    }

    // funciones soportadas -> aridad
    static int funcArity(const string& f) {
        string lf; lf.reserve(f.size());
        for (char c: f) lf.push_back((char)tolower((unsigned char)c));
        if (lf=="sin"||lf=="cos"||lf=="tan"||lf=="exp"||lf=="log"||lf=="ln"||lf=="sqrt"||
            lf=="abs"||lf=="asin"||lf=="acos"||lf=="atan"||lf=="sinh"||lf=="cosh"||lf=="tanh") return 1;
        if (lf=="pow") return 2;
        return -1; // no-función
    }

    static vector<Token> toRPN(const vector<Token>& toks) {
        vector<Token> output;
        vector<Token> stack;
        // Para manejar unario '-', necesitamos saber si '-' es unario según contexto:
        // Es unario si: al inicio, o después de '(' o ',' u otro operador.
        auto isUnaryPossibleAfter = [&](const Token*        if (!prev) return true;
            if (prev->type==TokType::LParen || prev->type==TokType::Comma) return true;
            if (prev->type==TokType::Op) return true;
            return false;
        };

        const Token* prev = nullptr;
        for (size_t i=0;i<toks.size();++i) {
            Token tk = toks[i];
            if (tk.type==TokType::Number || tk.type==TokType::Ident) {
                output.push_back(tk);
                prev=&toks[i];
            } else if (tk.type==TokType::Op) {
                // Checar unario '-'
                if (tk.text=="-" && isUnaryPossibleAfter(prev)) {
                    Token ut{TokType::Op, "u-", 0.0, 1};
                    // tratarlo como operador de alta precedencia, derecha
                    while (!stack.empty() && stack.back().type==TokType::Op &&
                           (prec(stack.back().text) > prec(ut.text) ||
                           (prec(stack.back().text)==prec(ut.text) && !rightAssoc(ut.text)))) {
                        output.push_back(stack.back()); stack.pop_back();
                    }
                    stack.push_back(ut);
                } else {
                    while (!stack.empty() && stack.back().type==TokType::Op &&
                          (prec(stack.back().text) > prec(tk.text) ||
                          (prec(stack.back().text)==prec(tk.text) && !rightAssoc(tk.text)))) {
                        output.push_back(stack.back()); stack.pop_back();
                    }
                    stack.push_back(tk);
                }
                prev=&toks[i];
            } else if (tk.type==TokType::LParen) {
                stack.push_back(tk);
                prev=&toks[i];
            } else if (tk.type==TokType::Comma) {
                // vaciar hasta '('
                while (!stack.empty() && stack.back().type!=TokType::LParen) {
                    output.push_back(stack.back()); stack.pop_back();
                }
                if (stack.empty()) throw runtime_error("Coma fuera de función o paréntesis desbalanceados.");
                prev=&toks[i];
            } else if (tk.type==TokType::RParen) {
                while (!stack.empty() && stack.back().type!=TokType::LParen) {
                    output.push_back(stack.back()); stack.pop_back();
                }
                if (stack.empty()) throw runtime_error("Paréntesis desbalanceados.");
                stack.pop_back(); // pop '('

                // Si en la cima quedó una función (ident antes de '('), moverla a output.
                if (!stack.empty() && stack.back().type==TokType::Ident) {
                    // Función: la marcamos con aridad
                    Token f = stack.back(); stack.pop_back();
                    int ar = funcArity(f.text);
                    if (ar<0) { // no era función, era variable seguida de '()' -> no soportado
                        throw runtime_error("Uso de 'nombre()' no reconocido como función: " + f.text);
                    }
                    f.arity = ar;
                    output.push_back(f);
                }
                prev=&toks[i];
            } else if (tk.type==TokType::Ident) {
                // ya contemplado arriba
                prev=&toks[i];
            }
        }
        while (!stack.empty()) {
            if (stack.back().type==TokType::LParen || stack.back().type==TokType::RParen)
                throw runtime_error("Paréntesis desbalanceados al final.");
            output.push_back(stack.back()); stack.pop_back();
        }
        return output;
    }
};

struct Evaluator {
    // Evalúa una RPN con variables en 'scope'. Soporta funciones listadas en Parser::funcArity.
    static double getConstOrVar(const string& name, const unordered_map<string,double>& scope) {
        // constantes
        if (iequals(name, "pi")) return acos(-1.0);
        if (iequals(name, "e"))  return exp(1.0);
        auto it = scope.find(name);
        if (it==scope.end()) throw runtime_error("Variable no definida en el alcance: " + name);
        return it->second;
    }

    static double applyFunc(const string& name, const vector<double>& args) {
        string f; f.reserve(name.size());
        for (char c: name) f.push_back((char)tolower((unsigned char)c));
        if (f=="sin") return sin(args[0]);
        if (f=="cos") return cos(args[0]);
        if (f=="tan") return tan(args[0]);
        if (f=="exp") return exp(args[0]);
        if (f=="log"||f=="ln") return log(args[0]);
        if (f=="sqrt") return sqrt(args[0]);
        if (f=="abs") return fabs(args[0]);
        if (f=="asin") return asin(args[0]);
        if (f=="acos") return acos(args[0]);
        if (f=="atan") return atan(args[0]);
        if (f=="sinh") return sinh(args[0]);
        if (f=="cosh") return cosh(args[0]);
        if (f=="tanh") return tanh(args[0]);
        if (f=="pow") return pow(args[0], args[1]);
        throw runtime_error("Función no soportada: " + name);
    }

    static double evalRPN(const vector<Token>& rpn, const unordered_map<string,double>& scope) {
        vector<double> st; st.reserve(32);
        for (const Token& tk: rpn) {
            if (tk.type==TokType::Number) {
                st.push_back(tk.value);
            } else if (tk.type==TokType::Ident) {
                st.push_back(getConstOrVar(tk.text, scope));
            } else if (tk.type==TokType::Op) {
                if (tk.text=="u-") {
                    if (st.empty()) throw runtime_error("Pila vacía para unario '-'");
                    double a = st.back(); st.pop_back();
                    st.push_back(-a);
                } else {
                    if (st.size()<2) throw runtime_error("Faltan operandos para operador: " + tk.text);
                    double b = st.back(); st.pop_back();
                    double a = st.back(); st.pop_back();
                    if (tk.text=="+") st.push_back(a+b);
                    else if (tk.text=="-") st.push_back(a-b);
                    else if (tk.text=="*") st.push_back(a*b);
                    else if (tk.text=="/") st.push_back(a/b);
                    else if (tk.text=="^") st.push_back(pow(a,b));
                    else throw runtime_error("Operador no soportado: " + tk.text);
                }
            } else if (tk.type==TokType::LParen || tk.type==TokType::RParen || tk.type==TokType::Comma) {
                // no aparecen en RPN
            } else if (tk.type==TokType::Ident && tk.arity>0) {
                // manejado como Token Ident con aridad, pero en esta implementación
                // usamos tk.type==Ident y luego applyFunc según aridad; veremos abajo:
            }
            // Funciones se emiten como Ident con aridad>0 al final del parsing de ')'
            if (tk.type==TokType::Ident && tk.arity>0) {
                vector<double> args;
                args.resize(tk.arity);
                for (int i=tk.arity-1; i>=0; --i) {
                    if (st.empty()) throw runtime_error("Faltan argumentos para función: " + tk.text);
                    args[i] = st.back(); st.pop_back();
                }
                st.push_back(applyFunc(tk.text, args));
            }
        }
        if (st.size()!=1) throw runtime_error("Expresión inválida: la evaluación no produjo un único valor.");
        return st.back();
    }
};

struct CompiledExpr {
    vector<Token> rpn;
    double eval(const unordered_map<string,double>& scope) const {
        return Evaluator::evalRPN(rpn, scope);
    }
    static CompiledExpr compile(const string& expr) {
        Lexer lx(expr);
        auto toks = lx.lex();

        // reconocer llamada a función: un identificador seguido inmediatamente por '('
        // Estrategia: Insertar el identificador en la pila cuando se vea '(' y se detecte que antes hay un ident;
        // ya manejado en Parser::toRPN extrayendo función cuando se cierra ')'.
        CompiledExpr ce;
        ce.rpn = Parser::toRPN(toks);
        return ce;
    }
};

// Representa una ecuación left = right como función f(scope)=left-right
struct EquationFunction {
    CompiledExpr left, right;
    double operator()(const unordered_map<string,double>& scope) const {
        return left.eval(scope) - right.eval(scope);
    }
    static EquationFunction fromString(const string& line) {
        string s = line;
        auto pos = s.find('=');
        string L, R;
        if (pos==string::npos) { L = s; R = "0"; }
        else { L = s.substr(0,pos); R = s.substr(pos+1); }
        EquationFunction ef;
        ef.left  = CompiledExpr::compile(L);
        ef.right = CompiledExpr::compile(R);
        return ef;
    }
};

// ===================== Álgebra lineal =====================
using Vec = vector<double>;
using Mat = vector<Vec>;

static inline double norm2(const Vec& v) {
    double s=0; for (double x: v) s += x*x; return sqrt(s);
}

static Vec matVec(const Mat& A, const Vec& x) {
    size_t m=A.size(), n=x.size();
    Vec y(m,0.0);
    for (size_t i=0;i<m;++i) {
        double s=0;
        for (size_t j=0;j<n;++j) s += A[i][j]*x[j];
        y[i]=s;
    }
    return y;
}

static Mat transpose(const Mat& A) {
    size_t m=A.size(), n=A[0].size();
    Mat AT(n, Vec(m,0.0));
    for (size_t i=0;i<m;++i) for (size_t j=0;j<n;++j) AT[j][i]=A[i][j];
    return AT;
}

static Mat matMul(const Mat& A, const Mat& B) {
    size_t m=A.size(), k=A[0].size(), n=B[0].size();
    Mat C(m, Vec(n,0.0));
    for (size_t i=0;i<m;++i) {
        for (size_t t=0;t<k;++t) {
            double a = A[i][t];
            if (a==0) continue;
            for (size_t j=0;j<n;++j) C[i][j] += a * B[t][j];
        }
    }
    return C;
}

static Vec matTVec(const Mat& A, const Vec& b) {
    // devuelve A^T * b
    size_t m=A.size(), n=A[0].size();
    Vec y(n,0.0);
    for (size_t i=0;i<m;++i)
        for (size_t j=0;j<n;++j) y[j] += A[i][j]*b[i];
    return y;
}

static Vec solveGaussian(Mat A, Vec b) {
    // A: n x n; pivoteo parcial
    const double EPS=1e-14;
    int n = (int)A.size();
    for (int col=0; col<n; ++col) {
        int piv = col;
        for (int i=col+1;i<n;++i)
            if (fabs(A[i][col]) > fabs(A[piv][col])) piv=i;
        if (fabs(A[piv][col]) < EPS) throw runtime_error("Matriz singular o mal condicionada.");
        if (piv!=col) { swap(A[piv], A[col]); swap(b[piv], b[col]); }
        for (int i=col+1;i<n;++i) {
            double f = A[i][col]/A[col][col];
            if (f==0) continue;
            for (int j=col;j<n;++j) A[i][j] -= f*A[col][j];
            b[i] -= f*b[col];
        }
    }
    // back-substitution
    Vec x(n,0.0);
    for (int i=n-1;i>=0;--i) {
        double s=0;
        for (int j=i+1;j<n;++j) s += A[i][j]*x[j];
        x[i] = (b[i]-s)/A[i][i];
    }
    return x;
}

// Resuelve mínimos cuadrados mediante ecuaciones normales: (A^T A)x = A^T b
static Vec solveLeastSquares(const Mat& A, const Vec& b) {
    Mat AT = transpose(A);
    Mat ATA = matMul(AT, A);
    Vec ATb = matTVec(A, b);
    return solveGaussian(ATA, ATb);
}

// ===================== Construcción de A,b para sistema lineal desde ecuaciones =====================
struct CompiledEq {
    EquationFunction f;
};

static bool buildLinearSystem(
    const vector<CompiledEq>& eqs,
    const vector<string>& vars,
    Mat& A, Vec& b, bool& looksLinear)
{
    size_t m = eqs.size(), n = vars.size();
    A.assign(m, Vec(n,0.0));
    b.assign(m, 0.0);

    // Construimos un scope con todas las variables
    unordered_map<string,double> scope;
    for (auto& v: vars) scope[v] = 0.0;

    // c_i = f(0)
    Vec c(m,0.0);
    try {
        for (size_t i=0;i<m;++i) c[i] = eqs[i].f(scope);
    } catch (const exception& e) {
        cerr << "Error evaluando f(0): " << e.what() << "\n";
        return false;
    }

    looksLinear = true;
    const double TOL = 1e-6;

    for (size_t i=0;i<m;++i) {
        for (size_t j=0;j<n;++j) {
            // f(e_j) - c -> coeficiente de x_j (si es lineal)
            scope[vars[j]] = 1.0;
            double val1 = eqs[i].f(scope);
            scope[vars[j]] = 0.0;
            double aij = val1 - c[i];

            // chequeo: f(2*e_j) - c ≈ 2*aij
            scope[vars[j]] = 2.0;
            double val2 = eqs[i].f(scope);
            scope[vars[j]] = 0.0;
            double linCheck = fabs( (val2 - c[i]) - 2.0*aij );
            if (linCheck > TOL) looksLinear = false;

            A[i][j] = aij;
        }
        // A x + c = 0 => A x = -c
        b[i] = -c[i];
    }
    return true;
}

// ===================== Newton para sistema no lineal =====================
static Mat numericalJacobian(const vector<EquationFunction>& fs, const vector<string>& vars, const Vec& x, double h=1e-6) {
    size_t n = vars.size();
    Mat J(n, Vec(n,0.0));
    unordered_map<string,double> scope;
    for (size_t j=0;j<n;++j) scope[vars[j]] = x[j];

    for (size_t j=0;j<n;++j) {
        double xj = x[j];
        scope[vars[j]] = xj + h;
        Vec fp(n,0.0);
        for (size_t i=0;i<n;++i) fp[i] = fsi;
        scope[vars[j]] = xj - h;
        Vec fm(n,0.0);
        for (size_t i=0;i<n;++i) fm[i] = fsi;
        scope[vars[j]] = xj;
        for (size_t i=0;i<n;++i) J[i][j] = (fp[i]-fm[i])/(2*h);
    }
    return J;
}

// ===================== Interfaz de usuario =====================
const string LINE(60, '—');

static vector<string> readVariables() {
    cout << "Ingresa las variables separadas por espacios (ej: x y z): ";
    string raw; getline(cin, raw); raw = trim(raw);
    if (raw.empty()) throw runtime_error("No ingresaste variables.");
    // reemplazar comas/semicolons por espacios
    for (char& c: raw) if (c==','||c==';') c=' ';
    stringstream ss(raw);
    string v; vector<string> vars;
    unordered_set<string> seen;
    while (ss >> v) {
        if (!seen.count(v)) {
            vars.push_back(v);
            seen.insert(v);
        }
    }
    if (vars.empty()) throw runtime_error("No se detectaron variables.");
    return vars;
}

static vector<CompiledEq> readEquationsAsCompiled(const vector<string>& vars) {
    cout << "Ingresa las ecuaciones (una por línea). Deja una línea en blanco para terminar:\n";
    vector<CompiledEq> eqs;
    while (true) {
        cout << "> ";
        string line; getline(cin, line);
        line = trim(line);
        if (line.empty()) break;
        try {
            CompiledEq ce;
            ce.f = EquationFunction::fromString(line);
            // Advertencia básica: variables no listadas (heurística: buscar identificadores)
            // Aquí omitimos análisis estático; la evaluación fallará si faltan.
            eqs.push_back(ce);
        } catch (const exception& e) {
            cout << "  ⚠ No se pudo interpretar esa línea: " << e.what() << "\n";
        }
    }
    return eqs;
}

static void opcionSistemaLineal() {
    cout << LINE << "\n";
    cout << "Resolver SISTEMA DE ECUACIONES LINEALES\n";
    cout << LINE << "\n";
    try {
        auto vars = readVariables();
        auto eqs = readEquationsAsCompiled(vars);
        if (eqs.empty()) { cout << "No ingresaste ecuaciones.\n"; return; }

        Mat A; Vec b; bool looksLinear=true;
        if (!buildLinearSystem(eqs, vars, A, b, looksLinear)) {
            cout << "⚠ No se pudo construir el sistema.\n";
            return;
        }
        if (!looksLinear) {
            cout << "⚠ Parece que al menos una ecuación no es lineal.\n"
                 << "   Usa la opción 'Sistema GENERAL (no lineal)'.\n";
            return;
        }

        size_t m=A.size(), n=A[0].size();
        cout.setf(std::ios::fixed); cout<<setprecision(10);
        try {
            Vec x;
            if (m==n) {
                x = solveGaussian(A, b);
                cout << "➤ Solución única:\n";
            } else {
                x = solveLeastSquares(A, b);
                cout << "➤ Solución de mínimos cuadrados (sistema no cuadrado):\n";
            }
            for (size_t j=0;j<n;++j) {
                cout << "   " << vars[j] << " = " << x[j] << "\n";
            }
        } catch (const exception& e) {
            cout << "⚠ Problema resolviendo el sistema: " << e.what() << "\n";
            cout << "   Sugerencia: verifica consistencia o reescala las ecuaciones.\n";
        }
    } catch (const exception& e) {
        cout << "⚠ Error: " << e.what() << "\n";
    }
}

static void opcionCuadratica() {
    cout << LINE << "\n";
    cout << "Resolver ECUACIÓN CUADRÁTICA: a*x^2 + b*x + c = 0\n";
    cout << LINE << "\n";
    cout << "a = "; string sa; getline(cin, sa);
    cout << "b = "; string sb; getline(cin, sb);
    cout << "c = "; string sc; getline(cin, sc);
    try {
        double a = stod(trim(sa));
        double b = stod(trim(sb));
        double c = stod(trim(sc));
        if (fabs(a) < 1e-15) {
            cout << "⚠ a = 0 ⇒ no es cuadrática. Usa el modo lineal o general.\n";
            return;
        }
        double disc = b*b - 4*a*c;
        cout.setf(std::ios::fixed); cout<<setprecision(10);
        cout << "Discriminante: Δ = " << disc << "\n";
        double twoA = 2*a;
        if (disc >= 0) {
            double sq = sqrt(disc);
            double x1 = (-b + sq)/twoA;
            double x2 = (-b - sq)/twoA;
            cout << "Raíces reales:\n";
            cout << "  x1 = " << x1 << "\n";
            cout << "  x2 = " << x2 << "\n";
        } else {
            double sq = sqrt(-disc);
            double real = -b / twoA;
            double imag =  sq / twoA;
            cout << "Raíces complejas:\n";
            cout << "  x1 = " << real << " + " << imag << "i\n";
            cout << "  x2 = " << real << " - " << imag << "i\n";
        }
    } catch (...) {
        cout << "⚠ Coeficientes inválidos. Usa números (ej. 2, -3, 4.5).\n";
    }
}

static void opcionSistemaGeneral() {
    cout << LINE << "\n";
    cout << "Resolver SISTEMA GENERAL (lineal o no lineal) con Newton-Raphson\n";
    cout << LINE << "\n";
    try {
        auto vars = readVariables();
        cout << "Debe haber tantas ecuaciones como variables.\n";
        auto eqsComp = readEquationsAsCompiled(vars);
        if (eqsComp.empty()) { cout << "No ingresaste ecuaciones.\n"; return; }
        if (eqsComp.size() != vars.size()) {
            cout << "⚠ Debe haber el mismo número de ecuaciones ("<<eqsComp.size()
                 <<") que de variables ("<<vars.size()<<").\n";
            return;
        }
        vector<EquationFunction> fs;
        fs.reserve(eqsComp.size());
        for (auto& ce : eqsComp) fs.push_back(ce.f);

        cout << "\nValores iniciales (Newton). Ejemplo si variables son 'x y z':\n";
        cout << "  x0 = 1, y0 = 0, z0 = 2\n";
        Vec x(vars.size(), 0.0);
        for (size_t i=0;i<vars.size();++i) {
            cout << vars[i] << "0 = ";
            string sv; getline(cin, sv); sv = trim(sv);
            try { x[i] = stod(sv); }
            catch (...) { cout << "⚠ Valor inicial inválido.\n"; return; }
        }

        cout << "Iteraciones máximas [por defecto 50]: ";
        string smax; getline(cin, smax); int MAX_IT = smax.empty() ? 50 : stoi(smax);
        cout << "Tolerancia en ||Δx|| [por defecto 1e-8]: ";
        string sTx; getline(cin, sTx); double TOL_X = sTx.empty() ? 1e-8 : stod(sTx);
        cout << "Tolerancia en ||f(x)|| [por defecto 1e-8]: ";
        string sTf; getline(cin, sTf); double TOL_F = sTf.empty() ? 1e-8 : stod(sTf);

        bool converged=false;
        cout.setf(std::ios::scientific); cout<<setprecision(3);

        for (int k=1;k<=MAX_IT;++k) {
            // f(x)
            unordered_map<string,double> scope;
            for (size_t j=0;j<vars.size();++j) scope[vars[j]] = x[j];
            Vec fx(vars.size(),0.0);
            for (size_t i=0;i<vars.size();++i) fx[i] = fsi;
            double nF = norm2(fx);
            if (nF < TOL_F) { converged=true; cout << "Convergió por ||f|| en iter "<<k<<".\n"; break; }

            // J(x)
            Mat J = numericalJacobian(fs, vars, x);
            // Resolver J * Δ = -f
            Vec rhs = fx;
            for (double& v: rhs) v = -v;
            Vec delta;
            try { delta = solveGaussian(J, rhs); }
            catch (const exception& e) {
                cout << "⚠ Falló el paso lineal (J*Δ=-f): " << e.what() << "\n";
                cout << "   Sugerencia: cambia valores iniciales o reescala el sistema.\n";
                return;
            }
            double nDx = norm2(delta);
            for (size_t j=0;j<x.size();++j) x[j] += delta[j];

            cout << "Iter " << k << ": ||f||=" << nF << "  ||Δx||=" << nDx << "\n";
            if (nDx < TOL_X) { converged=true; cout << "Convergió por ||Δx|| en iter "<<k<<".\n"; break; }
        }

        cout.setf(std::ios::fixed); cout<<setprecision(10);
        if (!converged) {
            cout << "⚠ No convergió. Prueba con otros valores iniciales o reescala las ecuaciones.\n";
        }
        cout << "➤ Resultado aproximado:\n";
        for (size_t i=0;i<vars.size();++i) {
            cout << "   " << vars[i] << " ≈ " << x[i] << "\n";
        }
    } catch (const exception& e) {
        cout << "⚠ Error: " << e.what() << "\n";
    }
}

static void menu() {
    while (true) {
        cout << "\n" << LINE << "\n";
        cout << "👋 Bienvenido/a al RESOLVEDOR MATEMÁTICO (C++)\n";
        cout << LINE << "\n";
        cout << "Elige una opción:\n";
        cout << "  1) Sistema de ecuaciones LINEALES (por texto)\n";
        cout << "  2) Ecuación CUADRÁTICA\n";
        cout << "  3) Sistema GENERAL (lineal o no lineal)\n";
        cout << "  0) Salir\n";
        cout << "Opción: ";
        string op; getline(cin, op); op = trim(op);
        if (op=="1") opcionSistemaLineal();
        else if (op=="2") opcionCuadratica();
        else if (op=="3") opcionSistemaGeneral();
        else if (op=="0" || op=="q" || op=="Q") { cout << "¡Hasta luego!\n"; break; }
        else cout << "❓ Opción no válida. Intenta de nuevo.\n";
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    try { menu(); }
    catch (const exception& e) {
        cerr << "Error fatal: " << e.what() << "\n";
    }
    return 0;
}
``