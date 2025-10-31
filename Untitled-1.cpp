/*
 * RESOLVEDOR MATEMÁTICO EN C++ (C++17) — Compatible con MSVC y MinGW
 *
 * Funcionalidad:
 *  1) Sistemas de ecuaciones LINEALES ingresando ecuaciones como texto (ej: 2*x + 3*y = 5)
 *     - Si m=n: eliminación gaussiana con pivoteo parcial.
 *     - Si m!=n: solución de mínimos cuadrados (ecuaciones normales).
 *  2) Ecuación CUADRÁTICA: a*x^2 + b*x + c = 0 (con raíces reales o complejas).
 *  3) Sistemas GENERALES (no lineales) con Newton-Raphson y Jacobiano numérico.
 *
 * Entrada: usa punto decimal (3.5), + - * / ^, funciones sin, cos, tan, exp, log/ln,
 * sqrt, abs, asin, acos, atan, sinh, cosh, tanh; constantes 'pi', 'e'.
 * NO hay multiplicación implícita: escribe 2*x, sin(x), etc.
 */

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <stdexcept>
#include <iomanip>
#include <algorithm>
#include <cctype>
#include <cmath>

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
    for (size_t i=0;i<a.size();++i) {
        unsigned char ca = static_cast<unsigned char>(a[i]);
        unsigned char cb = static_cast<unsigned char>(b[i]);
        if (std::tolower(ca) != std::tolower(cb)) return false;
    }
    return true;
}

// ===================== Parser de expresiones =====================
// Shunting-yard con soporte de funciones: sin, cos, tan, exp, log/ln, sqrt, abs,
// asin, acos, atan, sinh, cosh, tanh y pow(a,b).

enum class TokType { Number, Ident, Op, LParen, RParen, Comma, Func };

struct Token {
    TokType type;
    string text;
    double value = 0.0; // si Number
    int arity = 0;      // si Func
};

struct Lexer {
    string s; size_t i=0, n=0;
    explicit Lexer(string str): s(std::move(str)), n(s.size()) {}
    static bool isIdentStart(char c){ return std::isalpha(static_cast<unsigned char>(c)) || c=='_'; }
    static bool isIdentChar(char c){ return std::isalnum(static_cast<unsigned char>(c)) || c=='_'; }

    vector<Token> lex() {
        vector<Token> out;
        while (i<n) {
            char c = s[i];
            if (std::isspace(static_cast<unsigned char>(c))) { ++i; continue; }

            if (std::isdigit(static_cast<unsigned char>(c)) || c=='.') {
                size_t j=i; bool sawDot = (c=='.');
                ++j;
                while (j<n) {
                    char d = s[j];
                    if (std::isdigit(static_cast<unsigned char>(d))) { ++j; }
                    else if (d=='.' && !sawDot) { sawDot=true; ++j; }
                    else break;
                }
                if (j<n && (s[j]=='e' || s[j]=='E')) {
                    size_t k=j+1;
                    if (k<n && (s[k]=='+' || s[k]=='-')) ++k;
                    bool hasExpDigit=false;
                    while (k<n && std::isdigit(static_cast<unsigned char>(s[k]))) { hasExpDigit=true; ++k; }
                    if (hasExpDigit) j=k;
                }
                string num = s.substr(i, j-i);
                Token t{TokType::Number, num};
                t.value = std::stod(num);
                out.push_back(t);
                i=j; continue;
            }

            if (isIdentStart(c)) {
                size_t j=i+1;
                while (j<n && isIdentChar(s[j])) ++j;
                string id = s.substr(i, j-i);
                out.push_back(Token{TokType::Ident, id});
                i=j; continue;
            }

            if (c=='+'||c=='-'||c=='*'||c=='/'||c=='^') { out.push_back(Token{TokType::Op, string(1,c)}); ++i; continue; }
            if (c=='(') { out.push_back(Token{TokType::LParen,"("}); ++i; continue; }
            if (c==')') { out.push_back(Token{TokType::RParen,")"}); ++i; continue; }
            if (c==',') { out.push_back(Token{TokType::Comma,","}); ++i; continue; }

            throw runtime_error(string("Carácter no reconocido: '")+c+"'");
        }
        return out;
    }
};

struct Parser {
    static int prec(const string& op) {
        if (op=="^" || op=="u-") return 4; // unario '-' alta y asociativa a derecha
        if (op=="*" || op=="/") return 3;
        if (op=="+" || op=="-") return 2;
        return -1;
    }
    static bool rightAssoc(const string& op) { return (op=="^" || op=="u-"); }

    static int funcArity(const string& f) {
        string lf; lf.reserve(f.size());
        for (char c: f) lf.push_back((char)std::tolower(static_cast<unsigned char>(c)));
        if (lf=="sin"||lf=="cos"||lf=="tan"||lf=="exp"||lf=="log"||lf=="ln"||lf=="sqrt"||
            lf=="abs"||lf=="asin"||lf=="acos"||lf=="atan"||lf=="sinh"||lf=="cosh"||lf=="tanh") return 1;
        if (lf=="pow") return 2;
        return -1;
    }

    static vector<Token> toRPN(const vector<Token>& toks) {
        vector<Token> output;
        vector<Token> stack;

        auto isUnaryPossibleAfter = & -> bool {
            if (!prev) return true;
            if (prev->type==TokType::LParen || prev->type==TokType::Comma) return true;
            if (prev->type==TokType::Op) return true;
            if (prev->type==TokType::Func) return true;
            return false;
        };

        const Token* prev = nullptr;
        for (size_t i=0;i<toks.size();++i) {
            Token tk = toks[i];
            if (tk.type==TokType::Number) {
                output.push_back(tk);
                prev = &toks[i];
            } else if (tk.type==TokType::Ident) {
                // ¿Función? Ident seguido de '('
                bool isFunc = (i+1<toks.size() && toks[i+1].type==TokType::LParen && funcArity(tk.text) > 0);
                if (isFunc) {
                    Token f = tk; f.type = TokType::Func; // guardamos como función en la pila
                    stack.push_back(f);
                } else {
                    // Variable o constante
                    output.push_back(tk);
                }
                prev = &toks[i];
            } else if (tk.type==TokType::Op) {
                // unario '-'
                if (tk.text=="-" && isUnaryPossibleAfter(prev)) {
                    Token ut{TokType::Op, "u-"};
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
                prev = &toks[i];
            } else if (tk.type==TokType::LParen) {
                stack.push_back(tk);
                prev = &toks[i];
            } else if (tk.type==TokType::Comma) {
                while (!stack.empty() && stack.back().type!=TokType::LParen) {
                    output.push_back(stack.back()); stack.pop_back();
                }
                if (stack.empty()) throw runtime_error("Coma fuera de función o paréntesis desbalanceados.");
                prev = &toks[i];
            } else if (tk.type==TokType::RParen) {
                while (!stack.empty() && stack.back().type!=TokType::LParen) {
                    output.push_back(stack.back()); stack.pop_back();
                }
                if (stack.empty()) throw runtime_error("Paréntesis desbalanceados.");
                stack.pop_back(); // elimina '('

                // Si hay una función encima, pasarla a salida
                if (!stack.empty() && stack.back().type==TokType::Func) {
                    Token f = stack.back(); stack.pop_back();
                    f.arity = funcArity(f.text);
                    if (f.arity < 0) throw runtime_error("Función no soportada: " + f.text);
                    output.push_back(f);
                }
                prev = &toks[i];
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
    static double getConstOrVar(const string& name, const unordered_map<string,double>& scope) {
        if (iequals(name, "pi")) return std::acos(-1.0);
        if (iequals(name, "e"))  return std::exp(1.0);
        auto it = scope.find(name);
        if (it==scope.end()) throw runtime_error("Variable no definida: " + name);
        return it->second;
    }

    static double applyFunc(const string& name, const vector<double>& args) {
        string f; f.reserve(name.size());
        for (char c: name) f.push_back((char)std::tolower(static_cast<unsigned char>(c)));
        if (f=="sin") return std::sin(args[0]);
        if (f=="cos") return std::cos(args[0]);
        if (f=="tan") return std::tan(args[0]);
        if (f=="exp") return std::exp(args[0]);
        if (f=="log"||f=="ln") return std::log(args[0]);
        if (f=="sqrt") return std::sqrt(args[0]);
        if (f=="abs") return std::fabs(args[0]);
        if (f=="asin") return std::asin(args[0]);
        if (f=="acos") return std::acos(args[0]);
        if (f=="atan") return std::atan(args[0]);
        if (f=="sinh") return std::sinh(args[0]);
        if (f=="cosh") return std::cosh(args[0]);
        if (f=="tanh") return std::tanh(args[0]);
        if (f=="pow") return std::pow(args[0], args[1]);
        throw runtime_error("Función no soportada: " + name);
    }

    static double evalRPN(const vector<Token>& rpn, const unordered_map<string,double>& scope) {
        vector<double> st; st.reserve(32);
        for (const Token& tk: rpn) {
            if (tk.type==TokType::Number) {
                st.push_back(tk.value);
            } else if (tk.type==TokType::Func) {
                vector<double> args(tk.arity);
                for (int i=tk.arity-1;i>=0;--i) {
                    if (st.empty()) throw runtime_error("Faltan argumentos para función: " + tk.text);
                    args[i] = st.back(); st.pop_back();
                }
                st.push_back(applyFunc(tk.text, args));
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
                    else if (tk.text=="^") st.push_back(std::pow(a,b));
                    else throw runtime_error("Operador no soportado: " + tk.text);
                }
            }
        }
        if (st.size()!=1) throw runtime_error("Expresión inválida (la evaluación no produjo un único valor).");
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
        CompiledExpr ce;
        ce.rpn = Parser::toRPN(toks);
        return ce;
    }
};

struct EquationFunction {
    CompiledExpr left, right;
    double operator()(const unordered_map<string,double>& scope) const {
        return left.eval(scope) - right.eval(scope);
    }
    static EquationFunction fromString(const string& line) {
        auto pos = line.find('=');
        string L = (pos==string::npos) ? line : line.substr(0,pos);
        string R = (pos==string::npos) ? "0"  : line.substr(pos+1);
        EquationFunction ef{CompiledExpr::compile(L), CompiledExpr::compile(R)};
        return ef;
    }
};

// ===================== Álgebra lineal =====================
using Vec = vector<double>;
using Mat = vector<Vec>;

static inline double norm2(const Vec& v) { double s=0; for (double x: v) s+=x*x; return std::sqrt(s); }

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
            double a=A[i][t]; if (a==0) continue;
            for (size_t j=0;j<n;++j) C[i][j]+=a*B[t][j];
        }
    }
    return C;
}
static Vec matTVec(const Mat& A, const Vec& b) {
    size_t m=A.size(), n=A[0].size();
    Vec y(n,0.0);
    for (size_t i=0;i<m;++i) for (size_t j=0;j<n;++j) y[j]+=A[i][j]*b[i];
    return y;
}
static Vec solveGaussian(Mat A, Vec b) {
    const double EPS=1e-14;
    int n=(int)A.size();
    for (int col=0; col<n; ++col) {
        int piv=col;
        for (int i=col+1;i<n;++i) if (std::fabs(A[i][col])>std::fabs(A[piv][col])) piv=i;
        if (std::fabs(A[piv][col])<EPS) throw runtime_error("Matriz singular o mal condicionada.");
        if (piv!=col) { std::swap(A[piv],A[col]); std::swap(b[piv],b[col]); }
        for (int i=col+1;i<n;++i) {
            double f=A[i][col]/A[col][col];
            if (f==0) continue;
            for (int j=col;j<n;++j) A[i][j]-=f*A[col][j];
            b[i]-=f*b[col];
        }
    }
    Vec x(n,0.0);
    for (int i=n-1;i>=0;--i) {
        double s=0; for (int j=i+1;j<n;++j) s += A[i][j]*x[j];
        x[i]=(b[i]-s)/A[i][i];
    }
    return x;
}
static Vec solveLeastSquares(const Mat& A, const Vec& b) {
    Mat AT = transpose(A);
    Mat ATA = matMul(AT, A);
    Vec ATb = matTVec(A, b);
    return solveGaussian(ATA, ATb);
}

// ===================== Construcción de sistema lineal =====================
struct CompiledEq { EquationFunction f; };

static bool buildLinearSystem(
    const vector<CompiledEq>& eqs, const vector<string>& vars,
    Mat& A, Vec& b, bool& looksLinear)
{
    size_t m=eqs.size(), n=vars.size();
    A.assign(m, Vec(n,0.0));
    b.assign(m, 0.0);
    unordered_map<string,double> scope;
    for (auto& v: vars) scope[v]=0.0;

    Vec c(m,0.0);
    try { for (size_t i=0;i<m;++i) c[i]=eqs[i].f(scope); }
    catch (const exception& e) { cerr<<"Error evaluando f(0): "<<e.what()<<"\n"; return false; }

    looksLinear=true;
    const double TOL=1e-6;
    for (size_t i=0;i<m;++i) {
        for (size_t j=0;j<n;++j) {
            scope[vars[j]]=1.0; double val1=eqs[i].f(scope);
            scope[vars[j]]=0.0; double aij = val1 - c[i];

            scope[vars[j]]=2.0; double val2=eqs[i].f(scope);
            scope[vars[j]]=0.0; double linCheck = std::fabs((val2 - c[i]) - 2.0*aij);
            if (linCheck>TOL) looksLinear=false;

            A[i][j]=aij;
        }
        b[i] = -c[i];
    }
    return true;
}

// ===================== Newton no lineal =====================
static Mat numericalJacobian(const vector<EquationFunction>& fs, const vector<string>& vars, const Vec& x, double h=1e-6) {
    size_t n=vars.size();
    Mat J(n, Vec(n,0.0));
    unordered_map<string,double> scope;
    for (size_t j=0;j<n;++j) scope[vars[j]]=x[j];

    for (size_t j=0;j<n;++j) {
        double xj=x[j];
        scope[vars[j]]=xj+h; Vec fp(n,0.0); for (size_t i=0;i<n;++i) fp[i]=fsi;
        scope[vars[j]]=xj-h; Vec fm(n,0.0); for (size_t i=0;i<n;++i) fm[i]=fsi;
        scope[vars[j]]=xj;
        for (size_t i=0;i<n;++i) J[i][j]=(fp[i]-fm[i])/(2*h);
    }
    return J;
}

// ===================== Interfaz de usuario =====================
const string LINE(60,'—');

static vector<string> readVariables() {
    cout<<"Ingresa las variables separadas por espacios (ej: x y z): ";
    string raw; getline(cin, raw); raw=trim(raw);
    if (raw.empty()) throw runtime_error("No ingresaste variables.");
    for (char& c: raw) if (c==','||c==';') c=' ';
    stringstream ss(raw);
    string v; vector<string> vars; unordered_set<string> seen;
    while (ss>>v) if (!seen.count(v)) { vars.push_back(v); seen.insert(v); }
    if (vars.empty()) throw runtime_error("No se detectaron variables.");
    return vars;
}
static vector<CompiledEq> readEquationsAsCompiled(const vector<string>& /*vars*/) {
    cout<<"Ingresa las ecuaciones (una por línea). Deja una línea en blanco para terminar:\n";
    vector<CompiledEq> eqs;
    while (true) {
        cout<<"> ";
        string line; getline(cin, line); line=trim(line);
        if (line.empty()) break;
        try { CompiledEq ce; ce.f=EquationFunction::fromString(line); eqs.push_back(ce); }
        catch (const exception& e) { cout<<"  ⚠ No se pudo interpretar esa línea: "<<e.what()<<"\n"; }
    }
    return eqs;
}
static void opcionSistemaLineal() {
    cout<<LINE<<"\n"<<"Resolver SISTEMA DE ECUACIONES LINEALES\n"<<LINE<<"\n";
    try {
        auto vars=readVariables();
        auto eqs=readEquationsAsCompiled(vars);
        if (eqs.empty()) { cout<<"No ingresaste ecuaciones.\n"; return; }
        Mat A; Vec b; bool looksLinear=true;
        if (!buildLinearSystem(eqs, vars, A, b, looksLinear)) { cout<<"⚠ No se pudo construir el sistema.\n"; return; }
        if (!looksLinear) { cout<<"⚠ Parece que alguna ecuación no es lineal. Usa 'Sistema GENERAL'.\n"; return; }
        size_t m=A.size(), n=A[0].size();
        cout.setf(std::ios::fixed); cout<<setprecision(10);
        try {
            Vec x;
            if (m==n) { x=solveGaussian(A,b); cout<<"➤ Solución única:\n"; }
            else { x=solveLeastSquares(A,b); cout<<"➤ Solución de mínimos cuadrados (m≠n):\n"; }
            for (size_t j=0;j<n;++j) cout<<"   "<<vars[j]<<" = "<<x[j]<<"\n";
        } catch (const exception& e) {
            cout<<"⚠ Problema resolviendo: "<<e.what()<<"\n"
                <<"   Sugerencia: verifica consistencia o reescala las ecuaciones.\n";
        }
    } catch (const exception& e) { cout<<"⚠ Error: "<<e.what()<<"\n"; }
}
static void opcionCuadratica() {
    cout<<LINE<<"\n"<<"Resolver ECUACIÓN CUADRÁTICA: a*x^2 + b*x + c = 0\n"<<LINE<<"\n";
    cout<<"a = "; string sa; getline(cin, sa);
    cout<<"b = "; string sb; getline(cin, sb);
    cout<<"c = "; string sc; getline(cin, sc);
    try {
        double a=stod(trim(sa)), b=stod(trim(sb)), c=stod(trim(sc));
        if (std::fabs(a)<1e-15) { cout<<"⚠ a = 0 ⇒ no es cuadrática.\n"; return; }
        double disc=b*b-4*a*c;
        cout.setf(std::ios::fixed); cout<<setprecision(10);
        cout<<"Discriminante: Δ = "<<disc<<"\n";
        double twoA=2*a;
        if (disc>=0) {
            double sq=std::sqrt(disc);
            cout<<"Raíces reales:\n";
            cout<<"  x1 = "<<(-b+sq)/twoA<<"\n";
            cout<<"  x2 = "<<(-b-sq)/twoA<<"\n";
        } else {
            double sq=std::sqrt(-disc), real=-b/twoA, imag=sq/twoA;
            cout<<"Raíces complejas:\n";
            cout<<"  x1 = "<<real<<" + "<<imag<<"i\n";
            cout<<"  x2 = "<<real<<" - "<<imag<<"i\n";
        }
    } catch (...) { cout<<"⚠ Coeficientes inválidos. Usa números (ej. 2, -3, 4.5).\n"; }
}
static void opcionSistemaGeneral() {
    cout<<LINE<<"\n"<<"Resolver SISTEMA GENERAL (lineal o no lineal) con Newton-Raphson\n"<<LINE<<"\n";
    try {
        auto vars=readVariables();
        cout<<"Debe haber tantas ecuaciones como variables.\n";
        auto eqsComp=readEquationsAsCompiled(vars);
        if (eqsComp.empty()) { cout<<"No ingresaste ecuaciones.\n"; return; }
        if (eqsComp.size()!=vars.size()) {
            cout<<"⚠ Debe haber el mismo número de ecuaciones ("<<eqsComp.size()
                <<") que de variables ("<<vars.size()<<").\n"; return;
        }
        vector<EquationFunction> fs; fs.reserve(eqsComp.size());
        for (auto& ce: eqsComp) fs.push_back(ce.f);

        cout<<"\nValores iniciales (Newton). Si variables son 'x y z':\n  x0 = 1, y0 = 0, z0 = 2\n";
        Vec x(vars.size(),0.0);
        for (size_t i=0;i<vars.size();++i) {
            cout<<vars[i]<<"0 = "; string sv; getline(cin, sv); sv=trim(sv);
            try { x[i]=stod(sv); } catch (...) { cout<<"⚠ Valor inicial inválido.\n"; return; }
        }
        cout<<"Iteraciones máximas [50]: "; string smax; getline(cin, smax); int MAX_IT = smax.empty()?50:stoi(smax);
        cout<<"Tolerancia ||Δx|| [1e-8]: "; string sTx; getline(cin, sTx); double TOL_X = sTx.empty()?1e-8:stod(sTx);
        cout<<"Tolerancia ||f(x)|| [1e-8]: "; string sTf; getline(cin, sTf); double TOL_F = sTf.empty()?1e-8:stod(sTf);

        bool converged=false; cout.setf(std::ios::scientific); cout<<setprecision(3);
        for (int k=1;k<=MAX_IT;++k) {
            unordered_map<string,double> scope; for (size_t j=0;j<vars.size();++j) scope[vars[j]]=x[j];
            Vec fx(vars.size(),0.0); for (size_t i=0;i<vars.size();++i) fx[i]=fsi;
            double nF=norm2(fx); if (nF<TOL_F) { converged=true; cout<<"Convergió por ||f|| en iter "<<k<<".\n"; break; }
            Mat J = numericalJacobian(fs, vars, x);
            Vec rhs=fx; for (double& v: rhs) v=-v;
            Vec delta;
            try { delta = solveGaussian(J, rhs); }
            catch (const exception& e) { cout<<"⚠ Falló J*Δ=-f: "<<e.what()<<"\n"; return; }
            double nDx=norm2(delta);
            for (size_t j=0;j<x.size();++j) x[j]+=delta[j];
            cout<<"Iter "<<k<<": ||f||="<<nF<<"  ||Δx||="<<nDx<<"\n";
            if (nDx<TOL_X) { converged=true; cout<<"Convergió por ||Δx|| en iter "<<k<<".\n"; break; }
        }
        cout.setf(std::ios::fixed); cout<<setprecision(10);
        if (!converged) cout<<"⚠ No convergió. Cambia valores iniciales o reescala ecuaciones.\n";
        cout<<"➤ Resultado aproximado:\n";
        for (size_t i=0;i<vars.size();++i) cout<<"   "<<vars[i]<<" ≈ "<<x[i]<<"\n";
    } catch (const exception& e) { cout<<"⚠ Error: "<<e.what()<<"\n"; }
}
static void menu() {
    while (true) {
        cout<<"\n"<<LINE<<"\n"<<"👋 Bienvenido/a al RESOLVEDOR MATEMÁTICO (C++)\n"<<LINE<<"\n";
        cout<<"Elige una opción:\n";
        cout<<"  1) Sistema de ecuaciones LINEALES (por texto)\n";
        cout<<"  2) Ecuación CUADRÁTICA\n";
        cout<<"  3) Sistema GENERAL (lineal o no lineal)\n";
        cout<<"  0) Salir\n";
        cout<<"Opción: ";
        string op; getline(cin, op); op=trim(op);
        if (op=="1") opcionSistemaLineal();
        else if (op=="2") opcionCuadratica();
        else if (op=="3") opcionSistemaGeneral();
        else if (op=="0" || op=="q" || op=="Q") { cout<<"¡Hasta luego!\n"; break; }
        else cout<<"❓ Opción no válida. Intenta de nuevo.\n";
    }
}
int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    try { menu(); }
    catch (const exception& e) { cerr<<"Error fatal: "<<e.what()<<"\n"; }
    return 0;
}