#include <iostream>
#include <iomanip>
#include <cstring>
#include <limits.h>
#include <stack>
#include <queue>
#include <string>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include <graphics.h>

#define infinit 2147483646
#define epsi 0.0001

using namespace std;
bool eroare;
bool light=true, dark=false, culoarea_aleasa=false;
int CUL, k;
bool precizie0=false ,precizie1=false, precizie2=false, precizie3=false, precizie4=true;

bool DifInf(float x)
{
    return fabs(infinit-fabs(x)) > infinit / 2.0;
};

float Modul(float x)
{
    if (DifInf(x))
        return fabs(x);
    else
        return infinit;
}

float Ln(float x)
{
    if (x>epsi && DifInf(x))
        return log(x);
    else
        return infinit;
}

float Lg(float x)
{
    if (x>epsi && DifInf(x))
        return log10(x);
    else
        return infinit;
}

float Plus(float x, float y)
{
    if (DifInf(x) && DifInf(y))  return x+y;
    else
        return infinit;
}

float Minus(float x, float y)
{
    if (DifInf(x) && DifInf(y))  return x+y;
    else
        return infinit;
}

float Inmultit(float x, float y)
{
    if (fabs(x) < epsi || fabs(y) < epsi)
        return 0;
    else if (DifInf(x) && DifInf(y))
        return x*y;
    else
        return infinit;
}

float Impartit(float x, float y)
{
    if (abs(y)>epsi) return x/y;
    else
        return infinit;
}

float Putere(float x, float y)
{
    float p;
    if (y==0) return 0;
    else
        if (x==0) return 1;
        else
            if (x==infinit || y==infinit) return infinit;
            else
                return pow(x,y);
}

float Radical(float x, float y)
{
    if (DifInf(x) && (x>epsi) && y>0)
        return pow(x,1/y*1.0);
    else
        return infinit;
}


float Sinus(float x)
{
    if (DifInf(x))
        return sin(x);
    else
        return infinit;
}

float Cosinus(float x)
{
    if (DifInf(x))
        return cos(x);
    else
        return infinit;
}

float Tangenta(float x)
{
    if (DifInf(x))
        return sin(x)/cos(x);
    else
        return infinit;
}

float Cotangenta(float x)
{
    if (DifInf(x))
        return cos(x)/sin(x);
    else
        return infinit;
}

float Arcsinus(float x)
{
    if (DifInf(x))
        return asin(x);
    else
        return infinit;
}

float Arccosinus(float x)
{
    if (DifInf(x))
        return acos(x);
    else
        return infinit;
}

float Arctangenta(float x)
{
    if (DifInf(x))
        return atan(x);
    else
        return infinit;
}

float Arccotangenta(float x)
{
    if (DifInf(x))
        return acos(x/sqrt(1+x*x));
    else
        return infinit;
}

float Egal(float x, float y)
{
    	return x==y;
}

float Diferit(float x, float y)
{
    return x!=y;
}

float MaiMic(float x, float y)
{
    return x < y;
}

float MaiMare(float x, float y)
{
    return x > y;
}

int prio(char o)
{
    if(o=='(' || o==')')
        return 1;
    else if(o=='+' || o=='-')
        return 2;
    else if(o=='*' || o=='/')
        return 3;
    else if(o=='^')
        return 4;
    else if(o=='#' || o=='=' || o=='<' || o=='>')
        return 5;
    else if(o=='s' || o=='c' || o=='t' || o=='g' || o=='a' || o=='r' || o=='l' || o=='L' || o=='S' || o=='C' ||
            o=='T' || o=='G')///r-radical, a-abs, g-ctg, l-ln
        return 6;
}

int n,v[1005],j,m,nrvar;
stack <string> S;
queue <string> in, post;
string fctar1[] = {"sin", "cos", "tg", "ctg", "arcsin", "arccos", "arctg", "arcctg", "lg", "ln", "vector", "abs"};
string fctar2[] = {"log", "sqrt", "ind"};
string fctar3[] = {"lim"};
string fctar4[] = {"sum", "prod"};
int bgc1,bgc2,mx,my,l,st[10],lin, culoare, s, f, cnt, nrar1=12, nrar2=3, nrar3=1, nrar4=2;
int window1, window2, lungimetxt, colormode, precizie=4;
struct nod
{
    nod* fiu[6];
    string op, nr;
    int h, hj,w,s;
};
bool ok=false;
char input[100005], c, fct[100005][10], var[100005];
string pre[1005];
void mainwindow();
void MeniuFormule();
void ButoaneMeniu();
void calculatorwindow();
void setariwindow();
void back_to_menu();
int prioritate(string x)
{
    switch (x[0])
    {
    case '>':
    case '<':
    case '#':
    case '=': return 2;
    case '^': return 5;
    case '(': return 7;
    case ')': return 8;
    case '+':
    case '-': return 3;
    case '*':
    case '/': return 4;
    case ',': return 1;
    }
    int i;
    if(x=="<=" || x==">=") return 2;
    for (i = 0; i < nrar1; i++)
        if (fctar1[i] == x)
            return 6;
    for (i = 0; i < nrar2; i++)
        if (fctar2[i] == x)
            return 6;
    for (i = 0; i < nrar3; i++)
        if (fctar3[i] == x)
            return 6;
    for (i = 0; i < nrar4; i++)
        if (fctar4[i] == x)
            return 6;
}
bool operatie(string x)
{
    int i;
    switch (x[0])
    {
    case '>':
    case '<':
    case '#':
    case '=':
    case '+':
    case '-':
    case '*':
    case '/':
    case '(':
    case ')':
    case '^':
    case ',':
        return 1;
    }
    if(x=="<=" || x==">=") return 1;
    for (i = 0; i < nrar1; i++)
        if (fctar1[i] == x)
            return 1;
    for (i = 0; i < nrar2; i++)
        if (fctar2[i] == x)
            return 1;
    for (i = 0; i < nrar3; i++)
        if (fctar3[i] == x)
            return 1;
    for (i = 0; i < nrar4; i++)
        if (fctar4[i] == x)
            return 1;
    return 0;
}
void postordine()
{
    string x;
    int i;
    while (!in.empty())
    {
        x = in.front();
        in.pop();
        if (isdigit(x[0]) || ((x[0] == '-'||x[0]=='+') && isdigit(x[1])) || !operatie(x) || x=="+inf" || x=="-inf")
            post.push(x);
        else
        {
            if (x[0] == ')')
            {
                while(S.top()[0] != '(')
                {
                    post.push(S.top());
                    S.pop();
                }
                post.push(x);
                S.pop();
            }
            else
            {
                while(!S.empty() && S.top()[0] != '(' && (prioritate(S.top()) >= prioritate(x)))
                {
                    post.push(S.top());
                    S.pop();
                }
                S.push(x);
            }
        }
    }
    while(!S.empty())
    {
        post.push(S.top());
        S.pop();
    }
}
void arbore(nod *vf)
{
    //cout<<pre[j];
    char *aux=&pre[j][0];
    if (isdigit(pre[j][0]) || ((pre[j][0] == '-'||pre[j][0]=='+') && isdigit(pre[j][1])) || !operatie(pre[j]) || pre[j]=="+inf" || pre[j]=="-inf")
    {
        vf->nr = pre[j];
        j++;
        vf->op[0] = NULL;
        vf->fiu[1] = NULL;
    }
    else
    {
        vf->nr[0]=NULL;
        vf->op=pre[j];
        j++;
        for (int i = 0; i < nrar1-2; i++)
            if (fctar1[i] == vf->op)
            {
                if (!pre[j][0])
                {
                    cout<<"Eroare: functia "<<vf->op<<" este goala \n";
                    eroare=1;
                    return;
                }
                nod *x = new nod;
                vf->fiu[1] = x;
                arbore(vf->fiu[1]);
                if(eroare) return;
                vf->fiu[2] = NULL;
                return;
            }
        for (int i = 0; i < nrar2; i++)
            if (fctar2[i] == vf->op)
            {
                if(pre[j][0]&&pre[j]==")") j++;
                if(pre[j][0]&&pre[j]==",") j++;
                nod *x = new nod;
                vf->fiu[2] = x;
                if(!pre[j][0])
                {
                    cout<<"Eroare: functia "<<vf->op<<" este goala \n";
                    eroare=1;
                    return;
                }
                arbore(vf->fiu[2]);
                if(eroare) return;
                nod *y = new nod;
                vf->fiu[1] = y;
                if(!pre[j][0])
                {
                    cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                    eroare=1;
                    return;
                }
                arbore(vf->fiu[1]);
                if(eroare) return;
                vf->fiu[3] = NULL;
                return;
            }
        if (vf->op == "vector" || vf->op=="abs")
        {
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este goala \n";
                eroare=1;
                return;
            }
            nod *x = new nod;
            vf->fiu[1] = x;
            arbore(vf->fiu[1]);
            if(eroare) return;
            vf->fiu[2] = NULL;
            vf->fiu[1] = vf->fiu[1]->fiu[1];
            return;
        }
        if(vf->op == "lim")
        {
            if(pre[j][0]&&pre[j]==")") j++;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *z = new nod;
            vf->fiu[3] = z;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este goala \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[3]);
            if(eroare) return;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *x = new nod;
            vf->fiu[2] = x;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[2]);
            if(eroare) return;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *y = new nod;
            vf->fiu[1] = y;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[1]);
            if(eroare) return;
            vf->fiu[4] = NULL;
            return;
        }
        if(vf->op=="sum" || vf->op=="prod")
        {
            if(pre[j][0]&&pre[j]==")") j++;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *t = new nod;
            vf->fiu[4] = t;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este goala \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[4]);
            if(eroare) return;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *z = new nod;
            vf->fiu[3] = z;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[3]);
            if(eroare) return;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *x = new nod;
            vf->fiu[2] = x;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[2]);
            if(eroare) return;
            if(pre[j][0]&&pre[j]==",") j++;
            nod *y = new nod;
            vf->fiu[1] = y;
            if(!pre[j][0])
            {
                cout<<"Eroare: functia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[1]);
            if(eroare) return;
            vf->fiu[5] = NULL;
            return;
        }
        if (vf->op!=")")
        {
            nod *x = new nod;
            vf->fiu[2] = x;
            if(!pre[j][0])
            {
                cout<<"Eroare: operatia "<<vf->op<<" este incompleta \n";
                eroare=1;
                return;
            }
            arbore(vf->fiu[2]);
            if(eroare) return;
        }
        nod *x = new nod;
        vf->fiu[1] = x;
        if(!pre[j][0])
        {
            cout<<"Eroare: operatia "<<vf->op<<" este incompleta \n";
            eroare=1;
            return;
        }
        arbore(vf->fiu[1]);
        if(eroare) return;
        vf->fiu[3] = NULL;
        if(vf->op==")")
            vf->fiu[2] = NULL;
    }
}
void recalc(nod *vf, int s)
{
    vf->s=s;
    if(vf->nr[0])
    {
        setusercharsize(s,1,s,1);
        char *aux=&vf->nr[0];
        vf->w = textwidth(aux);
        vf->h = textheight(aux);
        vf->hj=0;
    }
    else {if(vf->fiu[1])
    {
        char *aux=&vf->op[0];
        if(vf->op == "log")
        {
            recalc(vf->fiu[1],max(0,s-2));
            recalc(vf->fiu[2],s);
            setusercharsize(s,1,s,1);
            vf->w=vf->fiu[1]->w+textwidth(aux)+vf->fiu[2]->w;;
            vf->hj=vf->fiu[1]->h/4;
            vf->h=textheight(aux)+vf->hj;
            vf->h=max(vf->h-vf->hj,vf->fiu[2]->h-vf->fiu[2]->hj);
            vf->hj=max(vf->hj,vf->fiu[2]->hj);
            vf->h+=vf->hj;
            return;
        }
        if(vf->op == "ind")
        {
            recalc(vf->fiu[1],max(0,s-2));
            vf->w=vf->fiu[1]->w-textwidth("1")+vf->fiu[2]->w;
            recalc(vf->fiu[2],s);
            vf->hj=vf->fiu[1]->h/4;
            vf->h=vf->fiu[1]->h;
            vf->h=max(vf->h-vf->hj,vf->fiu[2]->h-vf->fiu[2]->hj);
            vf->hj=max(vf->hj,vf->fiu[2]->hj);
            vf->h+=vf->hj;
            return;
        }
        if(vf->op=="sqrt")
        {
            recalc(vf->fiu[1],max(0,s-1));
            recalc(vf->fiu[2],s);
            setusercharsize(s,1,s,1);
            vf->w=vf->fiu[1]->w+vf->fiu[2]->w;
            vf->h=vf->fiu[1]->h;
            vf->h=max(vf->h+vf->fiu[2]->h/5*2,vf->fiu[2]->h);
            vf->hj=vf->fiu[2]->hj;
            return;
        }
        if(vf->op=="lim")
        {
            recalc(vf->fiu[1],max(0,s-2));
            recalc(vf->fiu[2],max(0,s-2));
            vf->w=vf->fiu[1]->w+vf->fiu[2]->w+textwidth("->");
            vf->h=max(max(vf->fiu[1]->h,vf->fiu[2]->h),textheight("->"))-textheight("1")/8;
            vf->hj=vf->h;
            recalc(vf->fiu[3],s);
            setusercharsize(s,1,s,1);
            vf->w=max(vf->w,textwidth("lim"))+textwidth("1")/4+vf->fiu[3]->w;
            vf->h+=textheight("lim");
            vf->h=max(vf->h-vf->hj,vf->fiu[3]->h-vf->fiu[3]->hj);
            vf->hj=max(vf->hj,vf->fiu[3]->hj);
            vf->h+=vf->hj;
            return;
        }
        if(vf->op=="sum" || vf->op=="prod")
        {
            recalc(vf->fiu[1],max(0,s-2));
            recalc(vf->fiu[2],max(0,s-2));
            recalc(vf->fiu[3],max(0,s-2));
            vf->w=max(max(vf->fiu[1]->w+vf->fiu[2]->w+textwidth("="), vf->fiu[3]->w)+textwidth("1"),textwidth("______"));
            recalc(vf->fiu[4],s);
            setusercharsize(s,1,s,1);
            vf->hj=max(vf->fiu[1]->h,vf->fiu[2]->h);
            vf->h=vf->hj+vf->fiu[3]->h+textheight("1")*5/4;
            vf->w+=vf->fiu[4]->w+textwidth("1")/4;
            vf->h=max(vf->h-vf->hj,vf->fiu[4]->h-vf->fiu[4]->hj);
            vf->hj=max(vf->hj,vf->fiu[4]->hj);
            vf->h+=vf->hj;
            return;
        }
        recalc(vf->fiu[1],s);
        //setusercharsize(s,1,s,1);
        for (int i = 0; i < nrar1-2; i++)
            if (fctar1[i] == vf->op)
            {
                vf->w = textwidth(aux) + vf->fiu[1]->w;
                vf->h = max(textheight(aux),vf->fiu[1]->h);
                vf->hj=vf->fiu[1]->hj;
                return;
            }
        if (vf->op == "vector")
        {
            vf->w = vf->fiu[1]->w;
            setusercharsize(max(0,s-3),1,max(0,s-3),1);
            vf->h = vf->fiu[1]->h + textheight("-")/4;
            vf->hj=vf->fiu[1]->hj;
        }
        else if (vf->op == "abs")
        {
            setusercharsize(s,1,s,1);
            vf->w = vf->fiu[1]->w + textwidth("|");
            vf->h = vf->fiu[1]->h;
            vf->hj=vf->fiu[1]->hj;
        }
        else if (vf->op[0] == ')')
        {
            int S=s;
            while(textheight("(")<=vf->fiu[1]->h)
            {
                S++;
                setusercharsize(S,1,S,1);
            }
            S--;
            setusercharsize(S,1,S,1);
            vf->w = vf->fiu[1]->w + textwidth("()");
            vf->h = max(textheight("("),vf->fiu[1]->h);
            vf->hj= vf->fiu[1]->hj;
            return;
        }
        else
        {
            vf->w = vf->fiu[1]->w;
            vf->h = vf->fiu[1]->h;
            if(vf->fiu[1]->op[0]==')'&&vf->fiu[1]->fiu[1]->op[0]=='/')
            {
                vf->fiu[1]=vf->fiu[1]->fiu[1];
                vf->w = vf->fiu[1]->w;
                vf->h = vf->fiu[1]->h;
                vf->hj=vf->fiu[1]->hj;
            }
            else if (vf->op[0] == '/')
            {
                setusercharsize(s,1,s,1);
                vf->w += textwidth("__");
                vf->h += textheight("__")/4;
                if(vf->fiu[1]->op[0]==')')
                {
                    vf->fiu[1]=vf->fiu[1]->fiu[1];
                    vf->w = vf->fiu[1]->w + textwidth("__");
                    vf->h = vf->fiu[1]->h + textheight("__")/4;
                }
            }
            else if(vf->op[0]=='^')
                vf->hj= vf->fiu[1]->hj;
            else if(vf->op==">=" || vf->op=="<=")
            {
                setusercharsize(s,1,s,1);
                vf->w+=textwidth(">");
                vf->h = max(textheight(">"),vf->h);
                vf->hj=vf->fiu[1]->hj;
            }
            else //+,-,>,>=,<,<=,=,*,#(inegal)
            {
                setusercharsize(s,1,s,1);
                vf->w += textwidth(aux);
                vf->h = max(textheight(aux),vf->h);
                vf->hj=vf->fiu[1]->hj;
            }
        }
    }
    if(vf->fiu[2])
    {
        if(vf->op[0]=='^') recalc(vf->fiu[2], max(0,s-1));
        else recalc(vf->fiu[2],s);
        if(vf->fiu[2]->op[0]==')'&&vf->fiu[2]->fiu[1]->op[0]=='/')
        {
            vf->fiu[2]=vf->fiu[2]->fiu[1];
            vf->w = vf->fiu[2]->w;
            vf->h = vf->fiu[2]->h;
            vf->hj=vf->fiu[2]->hj;
        }
        else if (vf->op[0] == '/')
        {
            setusercharsize(s,1,s,1);
            vf->w = max(vf->w, vf->fiu[2]->w + textwidth("__"));
            vf->h += vf->fiu[2]->h;
            vf->hj=vf->fiu[2]->h+textheight("__")/8-textheight("1")/2;
        }

        else if(vf->op[0]=='^')
        {
            vf->w += vf->fiu[2]->w;
            vf->h = vf->fiu[2]->h+(vf->h-vf->hj)/2+vf->hj;
        }
        else//+,-,>,>=,<,<=,=,*,#(inegal)
        {
            vf->w += vf->fiu[2]->w;
            vf->hj=max(vf->hj,vf->fiu[2]->hj);
            vf->h=max(vf->h-vf->hj,vf->fiu[2]->h-vf->fiu[2]->hj)+vf->hj;
        }
    }
    }
}
int auxsq=0;
void afisare(nod *vf, int x, int y)
{
    int i, s=vf->s;
    char *p=&vf->op[0];
    setusercharsize(s,1,s,1);
    if(vf->nr[0])
    {
        p=&vf->nr[0];
        outtextxy(x,y,p);
        return;
    }
    if(vf->op=="log")
    {
        int aux=vf->fiu[2]->h-vf->fiu[2]->hj;
        outtextxy(x,y+aux-textheight("1"),p);
        afisare(vf->fiu[1],x+textwidth(p),y+aux-vf->fiu[1]->h/4*3);
        setusercharsize(s,1,s,1);
        afisare(vf->fiu[2],x+textwidth(p)+vf->fiu[1]->w,y);
        return;
    }
    if(vf->op=="ind")
    {
        int aux=vf->fiu[2]->h-vf->fiu[2]->hj;
        afisare(vf->fiu[2],x,y);
        setusercharsize(s,1,s,1);
        afisare(vf->fiu[1],x+vf->fiu[2]->w,y+aux-vf->fiu[1]->h/4*3);
        return;
    }
    if(vf->op=="sqrt")
    {
        afisare(vf->fiu[2],x+vf->fiu[1]->w,y+vf->h-vf->fiu[2]->h);
        if(vf->fiu[1]->nr!="2") afisare(vf->fiu[1],x-textwidth("1")/8,y+vf->h-vf->fiu[1]->h-vf->fiu[2]->h/5*2);
        int aux=textheight("1")/8;
        if(!auxsq) aux=0;
        setusercharsize(s,1,s,1);
        setlinestyle(0,0,s);
        line(x+vf->fiu[1]->w/4,y+vf->h-vf->fiu[2]->h/5*2,x+vf->fiu[1]->w/3*2,y+vf->h-textheight("1")/4);
        line(x+vf->fiu[1]->w/3*2,y+vf->h-textheight("1")/4,x+vf->fiu[1]->w,y+aux);
        line(x+vf->fiu[1]->w,y+aux,x+vf->w,y+aux);
        setlinestyle(0,0,0);
        return;
    }
    if(vf->op=="lim")
    {
        int aux=vf->fiu[3]->h-vf->fiu[3]->hj, aux2;
        setusercharsize(vf->fiu[1]->s,1,vf->fiu[1]->s,1);
        aux2=textwidth("->");
        setusercharsize(s,1,s,1);
        outtextxy(x+(vf->fiu[1]->w+vf->fiu[2]->w+aux2-textwidth("lim"))/2,y+aux-textheight("1"), "lim");
        afisare(vf->fiu[3], x+vf->w-vf->fiu[3]->w, y);
        setusercharsize(vf->fiu[1]->s,1,vf->fiu[1]->s,1);
        afisare(vf->fiu[1],x,y+aux-textheight("1")/4);
        outtextxy(x+vf->fiu[1]->w, y+aux-textheight("1")/4, "->");
        afisare(vf->fiu[2],x+vf->fiu[1]->w+textwidth("->"),y+aux-textheight("1")/4);
        setusercharsize(s,1,s,1);
        return;
    }
    if(vf->op=="sum"||vf->op=="prod")
    {
        int aux=vf->h-vf->hj;
        afisare(vf->fiu[4],x+vf->w-vf->fiu[4]->w,y+aux-(vf->fiu[4]->h-vf->fiu[4]->hj));
        setusercharsize(s,1,s,1);
        int aux2=(vf->w-vf->fiu[4]->w-textwidth("1")/4)/2;
        setusercharsize(max(0,s-2),1,max(0,s-2),1);
        aux2-=(vf->fiu[1]->w+textwidth("=")+vf->fiu[2]->w)/2;
        afisare(vf->fiu[1],x+aux2,y+aux);
        outtextxy(x+aux2+vf->fiu[1]->w,y+aux,"=");
        afisare(vf->fiu[2],x+aux2+vf->fiu[1]->w+textwidth("="),y+aux);
        setusercharsize(s,1,s,1);
        aux2=(vf->w-vf->fiu[4]->w-textwidth("1")/4)/2;
        aux2-=vf->fiu[3]->w/2;
        afisare(vf->fiu[3],x+aux2,y+aux-(vf->fiu[3]->h+textheight("1")*5/4));
        setlinestyle(0,0,3);
        if(vf->op=="sum")
        {
            setusercharsize(s,1,s,1);
            aux2=(vf->w-vf->fiu[4]->w-textwidth("1")/4)/2;
            setusercharsize(max(0,s-2),1,max(0,s-2),1);
            int aux3=textwidth("______");
            aux2-=textwidth("___");
            setusercharsize(s,1,s,1);
            line(x+aux2,y+aux-textheight("1")*5/4+2,x+aux2+aux3,y+aux-textheight("1")*5/4+2);
            line(x+aux2,y+aux-textheight("1")*5/4+2,x+aux2+aux3/2,y+aux-textheight("1")*5/8+1);
            line(x+aux2,y+aux-2,x+aux2+aux3/2,y+aux-textheight("1")*5/8+1);
            line(x+aux2,y+aux-2,x+aux2+aux3,y+aux-2);
        }
        else//=prod
        {
            setusercharsize(s,1,s,1);
            aux2=(vf->w-vf->fiu[4]->w-textwidth("1")/4)/2;
            setusercharsize(max(0,s-2),1,max(0,s-2),1);
            int aux3=textwidth("______");
            aux2-=textwidth("___");
            setusercharsize(s,1,s,1);
            line(x+aux2,y+aux-textheight("1")*5/4+2,x+aux2+aux3,y+aux-textheight("1")*5/4+2);
            line(x+aux2+aux3/6,y+aux-textheight("1")*5/4+2,x+aux2+aux3/6,y+aux-2);
            line(x+aux2+aux3*5/6,y+aux-textheight("1")*5/4+2,x+aux2+aux3*5/6,y+aux-2);
        }
        setlinestyle(0,0,0);
        return;
    }
    if(vf->op[0]==')')
    {
        int aux=s;
        while(textheight("(")<=vf->fiu[1]->h)
        {
            aux++;
            setusercharsize(aux,1,aux,1);
        }
        aux--;
        setusercharsize(aux,1,aux,1);
        outtextxy(x,y,"(");
        afisare(vf->fiu[1], x+textwidth("("), y);
        setusercharsize(aux,1,aux,1);
        outtextxy(x+textwidth("(")+vf->fiu[1]->w, y, ")");
        return;
    }
    for (int i = 0; i < nrar1-2; i++)
    {
        if (fctar1[i] == vf->op)
        {
            int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
            outtextxy(x,y+aux-textheight("1"),p);
            afisare(vf->fiu[1],x+textwidth(p),y+aux-(vf->fiu[1]->h-vf->fiu[1]->hj));
            return;
        }
    }
    if(vf->op=="vector")
    {
        int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
        setusercharsize(max(0,s-3),1,max(0,s-3),1);
        afisare(vf->fiu[1],x,y+textheight("1")/4);
        setusercharsize(max(0,s-3),1,max(0,s-3),1);
        setlinestyle(0,0,2);
        outtextxy(x+vf->w-textwidth(">")+3,y,">");
        line(x,y+textheight("1")/2,x+vf->w,y+textheight("1")/2);
        setusercharsize(s,1,s,1);
        setlinestyle(0,0,0);
        return;
    }
    if(vf->op=="abs")
    {
        int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
        afisare(vf->fiu[1],x+textwidth("|")/2,y);
        setlinestyle(0,0,s);
        line(x+textwidth("|")/4,y,x+textwidth("|")/4, y+vf->h);
        line(x+vf->w-textwidth("|")/4,y,x+vf->w-textwidth("|")/4, y+vf->h);
        setusercharsize(s,1,s,1);
        setlinestyle(0,0,0);
        return;
    }
    if(vf->op[0]=='*')
    {
        int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
        if(vf->fiu[2])
            aux=max(aux,vf->fiu[2]->h-vf->fiu[2]->hj);
        outtextxy(x+vf->fiu[1]->w,y+aux-textheight("1")*5/4,".");
        if(light)
        {
            setcolor(4);
            rectangle(150,365,1050,615);
            setcolor(0);
            rectangle(100,210,1100,340);
        }
        else
        {
            setcolor(3);
            rectangle(150,365,1050,615);
            rectangle(100,210,1100,340);
        }
        if(culoarea_aleasa==true)
                setcolor(CUL);
        afisare(vf->fiu[1],x,y+aux-(vf->fiu[1]->h-vf->fiu[1]->hj));
        afisare(vf->fiu[2],x+(vf->w-vf->fiu[2]->w),y+aux-(vf->fiu[2]->h-vf->fiu[2]->hj));
        return;
    }
    if(vf->op[0]=='^')
    {
        afisare(vf->fiu[1],x,y+(vf->h-vf->fiu[1]->h));
        afisare(vf->fiu[2],x+(vf->w-vf->fiu[2]->w),y);
        return;
    }
    if(vf->op[0]=='/')
    {
        afisare(vf->fiu[1], x+(vf->w-vf->fiu[1]->w)/2,y);
        auxsq++;
        afisare(vf->fiu[2], x+(vf->w-vf->fiu[2]->w)/2,y+(vf->fiu[1]->h)+textheight("1")/4);
        setlinestyle(0,0,3);
        line(x+5,y+vf->fiu[1]->h+textheight("1")/8,x+vf->w-5,y+vf->fiu[1]->h+textheight("1")/8);
        setlinestyle(0,0,0);
        auxsq--;
        return;
    }
    if(vf->op==">=" || vf->op=="<=")
    {
        int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
        if(vf->fiu[2])
            aux=max(aux,vf->fiu[2]->h-vf->fiu[2]->hj);
        if(vf->op==">=") outtextxy(x+vf->fiu[1]->w,y+aux-textheight("1"),">");
        else outtextxy(x+vf->fiu[1]->w,y+aux-textheight("1"),"<");
        setlinestyle(0,0,s);
        line(x+vf->fiu[1]->w+10,y+aux-textheight("1")/6,x+vf->fiu[1]->w+textwidth("<")-10,y+aux-textheight("1")/6);
        setlinestyle(0,0,0);
        afisare(vf->fiu[1],x,y+aux-(vf->fiu[1]->h-vf->fiu[1]->hj));
        afisare(vf->fiu[2],x+(vf->w-vf->fiu[2]->w),y+aux-(vf->fiu[2]->h-vf->fiu[2]->hj));
        return;
    }
    bool inegal=0;
    if(vf->op=="#")
    {
        vf->op="=";
        inegal=1;
    }
    //+,-,>,>=,<,<=,=,#
    int aux=vf->fiu[1]->h-vf->fiu[1]->hj;
    if(vf->fiu[2])
        aux=max(aux,vf->fiu[2]->h-vf->fiu[2]->hj);
    outtextxy(x+vf->fiu[1]->w,y+aux-textheight("1"),p);
    if(inegal)
    {
        setlinestyle(0,0,s);
        line(x+vf->fiu[1]->w+textwidth("1")/6,y+aux-textheight("1")/5,x+vf->fiu[1]->w+textwidth("1")/6*5,y+aux-textheight("1")/5*4);
        setlinestyle(0,0,0);
    }
    afisare(vf->fiu[1],x,y+aux-(vf->fiu[1]->h-vf->fiu[1]->hj));
    afisare(vf->fiu[2],x+(vf->w-vf->fiu[2]->w),y+aux-(vf->fiu[2]->h-vf->fiu[2]->hj));

}
void vizual()
{
    int x = 0,nr=0, neg=0,i=0;
    string aux2;
    if(light==true)
    {
        setfillstyle(SOLID_FILL,COLOR(225,211,208));
        bar(150,365,1050,615);
    }
    else
    {
        setfillstyle(SOLID_FILL,COLOR(0,0,0));
        bar(150,365,1050,615);
    }
    if(light==true)
        setcolor(4);
    else
        setcolor(3);
    rectangle(150,365,1050,615);
    if(culoarea_aleasa==true)
        setcolor(CUL);
    else
    {
        if(light==true)
            setcolor(0);
        else
            setcolor(3);
    }
    if ((input[0] == '-'||input[0]=='+'))
    {
        if(isdigit(input[1]))
        {
            neg = 1;
            i++;
        }
        else if(input[1]=='i'&&input[2]=='n'&&input[3]=='f')
        {
            string aux;
            aux.append(1, input[0]);
            aux.append(1, input[1]);
            aux.append(1, input[2]);
            aux.append(1, input[3]);
            in.push(aux);
            i=i+4;
        }
    }
    for (; i < l; i++)
    {
        string aux;
        if (isdigit(input[i]))
        {
            if (neg)
            {
                aux.append(1, input[i - 1]);
                neg = 0;
            }
            if(!aux2.empty())
            {
                in.push(aux2);
                aux2.clear();
            }
            while (isdigit(input[i]) || input[i] == '.')
                aux.append(1, input[i++]);
            in.push(aux);
            i--;
            aux2.append(1,'*');
        }
        else if (isalpha(input[i]))
        {
            if(!aux2.empty())
            {
                in.push(aux2);
                aux2.clear();
            }
            while (isalpha(input[i]))
                aux.append(1, input[i++]);
            in.push(aux);
            i--;
        }
        else if((input[i]=='<'||input[i]=='>')&&input[i+1]=='=')
        {
            if(!aux2.empty())
                aux2.clear();
            aux.append(1, input[i]);
            aux.append(1, input[i+1]);
            in.push(aux);
            i++;
        }
        else
        {
            if(!aux2.empty())
            {
                if(input[i]=='(')
                    in.push(aux2);
                aux2.clear();
            }
            if(input[i]==')')
                aux2.append(1,'*');
            if (input[i] != ')' && (input[i + 1] == '-'||input[i+1]=='+'))
            {
                if(isdigit(input[i + 2]))
                {
                    if(input[i]!=' ')
                    {
                        aux.append(1, input[i]);
                        in.push(aux);
                    }
                    neg = 1;
                    i++;
                }
                else if(input[i+2]=='i'&&input[i+3]=='n'&&input[i+4]=='f')
                {
                    aux.append(1, input[i + 1]);
                    aux.append(1, input[i + 2]);
                    aux.append(1, input[i + 3]);
                    aux.append(1, input[i + 4]);
                    in.push(aux);
                    i=i+4;
                }
            }
            else if (input[i]!=' ')
            {
                aux.append(1, input[i]);
                in.push(aux);
            }
        }
    }
    postordine();
    pre[post.size()+1][0]=NULL;
    for (int j=post.size(); !post.empty(); post.pop())
        pre[j--] = post.front();
    j = 1;
    nod* r = new nod;
    setusercharsize(1,1,1,1);
    settextstyle(8,0,0);
    arbore(r);
    if(eroare) return;
    s=8;
    do
    {
        s--;
        setusercharsize(s,1,s,1);
        recalc(r,s);
    }
    while((r->h>240)||(r->w>890));
    setusercharsize(s,1,s,1);
    afisare(r,150+(900-r->w)/2, 365+(250-r->h)/2);
}

float calculare()
{
    bool Minu[10005]={};
    char variabile[10005]={}, ap[10005]={};
    char stiva_operatori[10005]={};
    float stiva_operanzi[10005]={}, necunoscute[10005]={};
    int top_operanzi=0, top_operatori=0, mn=0, nec=0;


    bool semn=false, neg=false, minus_in_fata=false, radical=false, virgula=false;
    mn=0;
    int i;
    for(i=0;i<strlen(input);i++)
    {
        if((input[i]>='a' && input[i]<='z') || (input[i]>='A' && input[i]<='Z'))
            if(input[i+1]!='(' && (isalpha(input[i+1])==0))
                if(!ap[input[i]])
                {
                   variabile[nec++]=input[i];
                   ap[input[i]]=1;
                }
    }
    for(i=0;i<nec;i++)
    {
        float x;
        cin>>x;
        necunoscute[i]=x;
    }
    stiva_operatori[top_operatori++]='(';
    for(i=0;i<strlen(input);i++)
    {
        if(input[i]==' ');
        else if(input[i]==',' && radical==true)
        {
            stiva_operatori[top_operatori]='(';
            virgula=true;
        }
        else if(input[i]=='>' && input[i+1]=='=')
        {
            stiva_operatori[top_operatori++]='?';
            i++;
        }
        else if(input[i]=='<' && input[i+1]=='=')
        {
            stiva_operatori[top_operatori++]='!';
            i++;
        }
        else if(input[i]>='0' && input[i]<='9')
        {
            float nr=0, nr2;
            char c;
            c=input[i];
            nr2=c;
            nr=nr*10.0+(nr2-48);
            while(input[i+1]>='0' && input[i+1]<='9')
            {
                c=input[i+1];
                nr2=c;
                nr=nr*10.0+(nr2-48);
                i++;
            }
            if(semn==false)
                stiva_operanzi[top_operanzi++]=nr*1.0;
            else
            {
                stiva_operanzi[top_operanzi++]=nr*(-1.0);
                semn=false;
            }
        }
        else if((input[i]>='a' && input[i]<='z') || (input[i]>='A' && input[i]<='Z'))   ///partea pentru op de prio 6
        {
            if(input[i-1]=='-')
            {
                Minu[mn++]=true;
                semn=false;
            }
            else
                mn++;

            if(isdigit(input[i-1]))///cazul cu inmultire fara sa fie scris "*"
                    stiva_operatori[top_operatori++]='*';
            else if(input[i-1]==')')
                    stiva_operatori[top_operatori++]='*';

            if(!((input[i+1]>='a' && input[i+1]<='z') || (input[i+1]>='A' && input[i+1]<='Z'))&&input[i+1]!='(')
            {
                for(int j=0;j<nec;j++)
                    if(variabile[j]==input[i])
                    {
                        if(semn==false)
                            stiva_operanzi[top_operanzi++]=necunoscute[j]*1.0;
                        else
                        {
                            stiva_operanzi[top_operanzi++]=necunoscute[j]*(-1.0);
                            semn=false;
                        }
                        break;
                    }
            }

            if(input[i]=='s')
            {
                if(input[i+1]=='i')
                {
                    stiva_operatori[top_operatori++]='s';//sinus
                    i+=2;
                }
                else if(input[i+1]=='q')
                {
                    stiva_operatori[top_operatori++]='r';//radical
                    i+=3;
                    radical=true;
                }
            }
            else if(input[i]=='c')
            {
                if(input[i+1]=='o')
                {
                    stiva_operatori[top_operatori++]='c';//cosinus
                    i+=2;
                }
                else if(input[i+1]=='t')
                {
                    stiva_operatori[top_operatori++]='g';//ctg
                    i+=2;
                }
            }
            else if(input[i]=='t')
            {
                stiva_operatori[top_operatori++]='t';//tg
                i++;
            }
            else if(input[i]=='a')
            {
                if(input[i+1]=='b')
                {
                    stiva_operatori[top_operatori++]='a';//abs
                    i+=2;
                }
                else if(input[i+3]=='s')//arcsin
                {
                    stiva_operatori[top_operatori++]='S';
                    i+=5;
                }
                else if(input[i+3]=='c')
                {
                    if(input[i+4]=='o')//arccos
                    {
                        stiva_operatori[top_operatori++]='C';
                        i+=5;
                    }
                    else            //arcctg
                    {
                        stiva_operatori[top_operatori++]='G';
                        i+=5;
                    }
                }
                else if(input[i+3]=='t')//arctg
                {
                    stiva_operatori[top_operatori++]='T';
                    i+=4;
                }
            }
            else if(input[i]=='l')
            {
                if(input[i+1]=='n')
                {
                    stiva_operatori[top_operatori++]='l';//ln
                    i++;
                }
                else if(input[i+1]=='g')
                {
                    stiva_operatori[top_operatori++]='L';//lg
                    i++;
                }
            }
        }                                                       ///final op prio 6
        else if(input[i]=='(')
        {
            if(isdigit(input[i-1]))///cazul cu inmultire fara sa fie scris "*"
                    stiva_operatori[top_operatori++]='*';
            else if(input[i-1]==')')
                    stiva_operatori[top_operatori++]='*';
            stiva_operatori[top_operatori++]='(';
            if(input[i-1]=='-')
            {
                Minu[mn++]=true;
                semn=false;
            }
            else
                mn++;
            if(input[i+1]=='-')
                neg=true;
        }
        else
        {
            if(input[i]=='-')
            {
                semn=true;
                if(stiva_operatori[top_operatori-1]=='(' && top_operanzi==0)///inca nu e bagat '-'
                   neg=true;
            }
            if(prio(stiva_operatori[top_operatori-1])<prio(input[i]))
            {
                if(neg==false)///daca primu element e -ceva nu bagam - in stiva
                    stiva_operatori[top_operatori++]=input[i];
                neg=false;
            }
            else
            {
                top_operanzi--;
                bool ok=false;///pentru cazurile 1+2*2/2, practic daca ajunge la ) se duce pana la capat
                if(input[i]!=')')
                {
                    ok=true;
                }
                while(stiva_operatori[top_operatori-1]!='(')
                {
                    top_operatori--;
                    float a=stiva_operanzi[top_operanzi--]*1.0;///varful
                    float b=stiva_operanzi[top_operanzi]*1.0;///varful-1
                    if(stiva_operatori[top_operatori]=='+')//+
                    {
                        stiva_operanzi[top_operanzi]=Plus(a,b);
                    }
                    else if(stiva_operatori[top_operatori]=='-')//-
                    {
                        stiva_operanzi[top_operanzi]=Minus(b,a);

                    }
                    else if(stiva_operatori[top_operatori]=='*')//*
                    {
                        stiva_operanzi[top_operanzi]=Inmultit(a,b);
                        if(ok==true)
                            break;

                    }
                    else if(stiva_operatori[top_operatori]=='/')// /
                    {
                        if(a==0 || b==0)
                            stiva_operanzi[top_operanzi]=0;
                        else
                            stiva_operanzi[top_operanzi]=Impartit(b,a);
                        if(ok==true)
                            break;
                    }
                    else if(stiva_operatori[top_operatori]=='^')//^
                    {
                        stiva_operanzi[top_operanzi]=pow(b,a);
                        if(ok==true)
                            break;
                    }
                    else if(stiva_operatori[top_operatori]=='s')//sin
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Sinus(a);
                    }
                    else if(stiva_operatori[top_operatori]=='c')//cos
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Cosinus(a);
                    }
                    else if(stiva_operatori[top_operatori]=='t')//tg
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Tangenta(a);
                    }
                    else if(stiva_operatori[top_operatori]=='g')//ctg
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Cotangenta(a);
                    }
                    else if(stiva_operatori[top_operatori]=='a')//modul, abs
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Modul(a);
                    }
                    else if(stiva_operatori[top_operatori]=='r')//radical
                    {
                        stiva_operanzi[top_operanzi]=Radical(a,b);
                        radical=false;
                    }
                    else if(stiva_operatori[top_operatori]=='l')//ln
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Ln(a);
                    }
                    else if(stiva_operatori[top_operatori]=='L')//ln
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Lg(a);
                    }
                    else if(stiva_operatori[top_operatori]=='A')//arcsin
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Arcsinus(a);
                    }
                    else if(stiva_operatori[top_operatori]=='C')//arccos
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Arccosinus(a);
                    }
                    else if(stiva_operatori[top_operatori]=='T')//arctg
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Arctangenta(a);
                    }
                    else if(stiva_operatori[top_operatori]=='G')//arcctg
                    {
                        top_operanzi++;
                        stiva_operanzi[top_operanzi]=Arccotangenta(a);
                    }
                    else if(stiva_operatori[top_operatori]=='>')
                    {
                        stiva_operanzi[top_operanzi]=MaiMare(b,a);
                    }
                    else if(stiva_operatori[top_operatori]=='<')
                    {
                        stiva_operanzi[top_operanzi]=MaiMic(b,a);
                    }
                    else if(stiva_operatori[top_operatori]=='=')
                    {
                        stiva_operanzi[top_operanzi]=Egal(b,a);
                    }
                    else if(stiva_operatori[top_operatori]=='#')
                    {
                        stiva_operanzi[top_operanzi]=Diferit(b,a);
                    }
                    else if(stiva_operatori[top_operatori]=='?')///>=
                    {
                        bool rasp=0;
                        if(b>=a)
                            rasp=1;
                        stiva_operanzi[top_operanzi]=rasp;
                    }
                     else if(stiva_operatori[top_operatori]=='!')///<=
                    {
                        bool rasp=0;
                        if(b<=a)
                            rasp=1;
                        stiva_operanzi[top_operanzi]=rasp;
                    }

                }
                if(input[i]==')')
                {
                    if(Minu[mn-1]==true)/// cazuri cu -(...-(
                    {
                        mn--;
                        Minu[mn]=false;
                        stiva_operanzi[top_operanzi]*=-1.0;
                    }
                    else
                        mn--;
                }
                top_operanzi++;
                if(input[i]!=')')/// in cazul in care avem ceva de prioritate mai mica si intra in else trebuie retinut si urmatorul semn
                    stiva_operatori[top_operatori++]=input[i];
                if(virgula==true && input[i]==')' && input[i+1]==')')
                {
                    virgula=false;
                    i--;
                }
                if(stiva_operatori[top_operatori-1]=='(' && top_operatori-1!=0)
                    top_operatori--;
            }
        }
    }

    stiva_operatori[top_operatori]='\n';
    stiva_operanzi[top_operanzi]='\n';
    return stiva_operanzi[0];
}
void scrie()
{
    if(c==8)//backspace
    {
        if(!l) return;
        input[l-1]=' ';
        outtextxy(110,220+25*lin,input+st[lin]);
        input[--l]=0;
        if(st[lin]>l-1) lin=max(lin-1,0);
    }
    else if(c==13)//enter
    {
        if(l==0)
        {
            cout<<"Va rugam introduceti o formula, sirul este gol! \n";
            return;
        }
        bool prima_tura=true;
        vizual();
        settextstyle(8,0,3);
        if(eroare)
        {
            eroare=0;
            return;
        }
        input[strlen(input)]=')';
        input[strlen(input)+1]='\0';
        if(precizie0==true)
        {
            cout << fixed << showpoint;
            cout << setprecision(0);
        }
        else if(precizie1==true)
        {
            cout << fixed << showpoint;
            cout << setprecision(1);
        }
        else if(precizie2==true)
        {
            cout << fixed << showpoint;
            cout << setprecision(2);
        }
        else if(precizie3==true)
        {
            cout << fixed << showpoint;
            cout << setprecision(3);
        }
        else if(precizie4==true)
        {
            cout << fixed << showpoint;
            cout << setprecision(4);
        }
        if(prima_tura==true)
        {
            cout<<calculare()<<'\n';
            input[strlen(input)-1]=' ';
            prima_tura=false;
        }
        else
        {
            cout<<calculare()<<'\n';
        }
    }
    else if(c&&lin<5)
    {
        input[l]=c;
        input[++l]=0;
        if(textwidth(input+st[lin])>=980)
            st[++lin]=l-1;
    }
}
void inapoi_formule()/// buton inapoi pentru meniu
{
   if(light==true)
   {
        culoare=4;
        settextstyle(10,0,6);
        setbkcolor(culoare);
        setfillstyle(SOLID_FILL,culoare);
        bar(50,500,250,565);
        outtextxy(55,505,"Inapoi");
        culoare=16;
        setbkcolor(culoare);
    }
    else
    {
        culoare=5;
        settextstyle(10,0,6);
        setbkcolor(culoare);
        setfillstyle(SOLID_FILL,culoare);
        bar(50,500,250,565);
        outtextxy(55,505,"Inapoi");
        culoare=16;
        setbkcolor(culoare);
    }

}
void updateformuleb()//"formule"
{
    settextstyle(10,0,3);
    setbkcolor(bgc2);
    setfillstyle(SOLID_FILL,bgc2);
    bar(100, 90, 300, 140);
    outtextxy(150, 105, "Formule");
    setlinestyle(0,0,4);
    rectangle(100, 90, 300, 140);
    setlinestyle(0,0,0);
}
void updatebackb()//"inapoi"
{
    settextstyle(10,0,6);
    setbkcolor(bgc1);
    setfillstyle(SOLID_FILL,bgc1);
    bar(50,645,250,710);
    outtextxy(55,650,"Inapoi");
    setlinestyle(0,0,4);
    rectangle(50,645,250,710);
    setlinestyle(0,0,0);

}
void updatemainb1()//"calculator"
{
    settextstyle(10,0,6);
    setbkcolor(bgc1);
    setfillstyle(SOLID_FILL,bgc1);
    bar(400,400,800,500);
    outtextxy(445,425,"Calculator");
    setlinestyle(0,0,6);
    rectangle(400,400,800,500);
    setlinestyle(0,0,0);
}
void updatemainb2()//"setari"
{
    settextstyle(10,0,6);
    setbkcolor(bgc2);
    setfillstyle(SOLID_FILL,bgc2);
    bar(400,550,800,650);
    outtextxy(510,575,"Setari");
    setlinestyle(0,0,6);
    rectangle(400,550,800,650);
    setlinestyle(0,0,0);

}
int b1, b2;
void mainwindow()
{
    //golire window
    clearviewport();
    clearmouseclick(WM_LBUTTONUP);
    clearmouseclick(WM_LBUTTONDOWN);
    //setting up new window
    if(light==true)
    {
        setbkcolor(COLOR(225,211,208));
        cleardevice();
        setcolor(0);
    }
    else
    {
        setbkcolor(COLOR(0,0,0));
        cleardevice();
        setcolor(3);
    }
    settextstyle(8,0,1);
    outtextxy(280,685,"Proiect realizat de: Duluman Sebastian-Andrei si Vlad Adriana");
    settextstyle(10,0,6);
    if(light==true)
    {
        readimagefile("logo_light.jpg", 275, 5, 925, 395);
    }
    else
    {
        readimagefile("logo_dark.jpg", 275, 5, 925, 395);
    }
    //every frame
    updatemainb1();
    updatemainb2();
    while(1)
    {
        if(b1!=bgc1)
        {
            updatemainb1();
            b1=bgc1;
        }
        if(b2!=bgc2)
        {
            updatemainb2();
            b2=bgc2;
        }
        mx=mousex();
        my=mousey();
        //hover&click pt butonul "calculator"
        if(mx>=400&&mx<=800&&my>=400&&my<=500)
        {
            while(ismouseclick(WM_LBUTTONDOWN))
            {
                if(light==true)
                    bgc1=15;
                else
                    bgc1=0;
                if(b1!=bgc1)
                {
                    updatemainb1();
                    b1=bgc1;
                }
                mx=mousex();
                my=mousey();
                if(!(mx>=400&&mx<=800&&my>=400&&my<=500)) break;
                if(ismouseclick(WM_LBUTTONUP))
                    calculatorwindow();
                delay(10);
            }
            clearmouseclick(WM_LBUTTONUP);
            clearmouseclick(WM_LBUTTONDOWN);
            if(light==true)
                bgc1=12;
            else
                bgc1=13;
        }
        else
        {
            if(light==true)
                bgc1=4;
            else
                bgc1=5;
            //hover&click pt butonul "setari"
            if(mx>=400&&mx<=800&&my>=550&&my<=650)
            {
                while(ismouseclick(WM_LBUTTONDOWN))
                {
                    if(light)
                        bgc2=15;
                    else
                        bgc2=0;
                    if(b2!=bgc2)
                    {
                        updatemainb2();
                        b2=bgc2;
                    }
                    mx=mousex();
                    my=mousey();
                    if(!(mx>=400&&mx<=800&&my>=550&&my<=650)) break;
                    if(ismouseclick(WM_LBUTTONUP))
                        setariwindow();
                    delay(10);
                }
                clearmouseclick(WM_LBUTTONUP);
                clearmouseclick(WM_LBUTTONDOWN);
                if(light)
                    bgc2=12;
                else
                    bgc2=13;
            }

            else
            {
                if(light==true)
                    bgc2=4;
                else if(dark==true)
                    bgc2=5;
                clearmouseclick(WM_LBUTTONUP);
                clearmouseclick(WM_LBUTTONDOWN);
            }
        }
        delay(20);
    }
}

void desenare_meniu()
{
    setbkcolor(5);
    setfillstyle(SOLID_FILL, 5);
    bar(700, 530, 754, 580);
    outtextxy(701, 544, "OK");
    setlinestyle(0,0,5);
    rectangle(700, 530, 754, 580);
    setlinestyle(0,0,0);

    readimagefile("sin.jpg", 20, 125, 80, 175);
    readimagefile("cos.jpg", 120, 125, 180, 175);
    readimagefile("tan.jpg", 220, 125, 280, 175);
    readimagefile("ctg.jpg", 320, 125, 380, 175);
    readimagefile("arcsin.jpg", 420, 125, 480, 175);
    readimagefile("arccos.jpg", 520, 125, 580, 175);
    readimagefile("arctg.jpg", 620, 125, 680, 175);
    readimagefile("arcctg.jpg", 720, 125, 780, 175);

    readimagefile("mare.jpg", 120, 200, 180, 250);
    readimagefile("mareegal.jpg", 220, 200, 280, 250);
    readimagefile("mic.jpg", 320, 200, 380, 250);
    readimagefile("micegal.jpg", 420, 200, 480, 250);
    readimagefile("egal.jpg", 520, 200, 580, 250);
    readimagefile("diferit.jpg", 620, 200, 680, 250);

    readimagefile("plus.jpg", 120, 275, 180, 325);
    readimagefile("minus.jpg", 220, 275, 280, 325);
    readimagefile("asuprab.jpg", 320, 275, 380, 325);
    readimagefile("inmultit.jpg", 420, 275, 480, 325);
    readimagefile("xlaa.jpg", 520, 275, 580, 325);
    readimagefile("modul.jpg", 620, 275, 680, 325);

    readimagefile("sum.jpg", 120, 350, 180, 400);
    readimagefile("radical.jpg", 220, 350, 280, 400);
    readimagefile("radicala.jpg", 320, 350, 380, 400);
    readimagefile("ln.jpg", 420, 350, 480, 400);
    readimagefile("lg.jpg", 520, 350, 580, 400);
    readimagefile("prod.jpg", 620, 350, 680, 400);

    setlinestyle(0,0,5);
    for(int i=20;i<=720;i+=100)
        rectangle(i+1,125,i+60,175);

    for(int i=120;i<=620;i+=100)
        rectangle(i+1,200,i+60,250);

    for(int i=120;i<=620;i+=100)
        rectangle(i+1,275,i+60,325);

    for(int i=120;i<=620;i+=100)
        rectangle(i+1,350,i+60,400);

    setbkcolor(0);
    settextstyle(6,0,3);
    outtextxy(25, 425, "Culoarea textului:");

    int culoare=0;

    setlinestyle(0,0,7);
    for(int i=10;i<=760;i+=50)
    {
        setcolor(3);
        rectangle(i,465,i+30,490);

        setfillstyle(SOLID_FILL, culoare);
        bar(i+5, 470, i+26, 486);
        culoare++;
    }
    setlinestyle(0,0,0);

}

///meniul propriu-zis
void ButoaneMeniu()
{
    clearmouseclick(WM_LBUTTONDOWN);
    clearmouseclick(WM_LBUTTONUP);

    desenare_meniu();

    int xx1[]={10, 60, 110, 160, 210, 260, 310, 360, 410, 460, 510, 560,
    610, 660, 710, 760};
    int x1, x2, y1, y2;

    if(ok==true)
    {
        setcurrentwindow(window1);
    }
    else
    {
        while(1)/// aici pentru orice buton din meniu va duce utilizatorul catre o noua pagina
        {
            int mx=mousex();
            int my=mousey();
            if(mx>=700 && mx<=754 && my>=530 && my<=580)///ok
            {
                x1=700, x2=754, y1=530, y2=580;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    ok=true;
                    clearmouseclick(WM_LBUTTONDOWN);
                    clearmouseclick(WM_LBUTTONUP);
                    setcurrentwindow(window1);
                    calculatorwindow();
                    break;
                }
            }
            else if(mx>=20 && mx<=80 && my>=125 && my<=175)///line1
            {
                x1=20, x2=80, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='s';
                    input[l++]='i';
                    input[l++]='n';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=120 && mx<=180 && my>=125 && my<=175)
            {
                x1=120, x2=180, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='c';
                    input[l++]='o';
                    input[l++]='s';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=220 && mx<=280 && my>=125 && my<=175)
            {
                x1=220, x2=280, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='t';
                    input[l++]='g';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=320 && mx<=380 && my>=125 && my<=175)
            {
                x1=320, x2=380, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='c';
                    input[l++]='t';
                    input[l++]='g';
                    input[l++]='(';

                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=420 && mx<=480 && my>=125 && my<=175)
            {
                x1=420, x2=480, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='a';
                    input[l++]='r';
                    input[l++]='c';
                    input[l++]='s';
                    input[l++]='i';
                    input[l++]='n';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=520 && mx<=580 && my>=125 && my<=175)
            {
                x1=520, x2=580, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='a';
                    input[l++]='r';
                    input[l++]='c';
                    input[l++]='c';
                    input[l++]='o';
                    input[l++]='s';
                    input[l++]='(';

                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=620 && mx<=680 && my>=125 && my<=175)
            {
                x1=620, x2=680, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='a';
                    input[l++]='r';
                    input[l++]='c';
                    input[l++]='t';
                    input[l++]='g';
                    input[l++]='(';

                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=720 && mx<=780 && my>=125 && my<=175)
            {
                x1=720, x2=780, y1=125, y2=175;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='a';
                    input[l++]='r';
                    input[l++]='c';
                    input[l++]='c';
                    input[l++]='t';
                    input[l++]='g';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=120 && mx<=180 && my>=200 && my<=250)///line2
            {
                x1=120, x2=180, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='>';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=220 && mx<=280 && my>=200 && my<=250)
            {
                x1=220, x2=280, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='?';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=320 && mx<=380 && my>=200 && my<=250)
            {
                x1=320, x2=380, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='<';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=420 && mx<=480 && my>=200 && my<=250)
            {
                x1=420, x2=480, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='!';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=520 && mx<=580 && my>=200 && my<=250)
            {
                x1=520, x2=580, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='=';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=620 && mx<=680 && my>=200 && my<=250)
            {
                x1=620, x2=680, y1=200, y2=250;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='#';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=120 && mx<=180 && my>=275 && my<=325)///line3
            {
                x1=120, x2=180, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='+';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=220 && mx<=280 && my>=275 && my<=325)
            {
                x1=220, x2=280, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='-';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=320 && mx<=380 && my>=275 && my<=325)
            {
                x1=320, x2=380, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='/';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=420 && mx<=480 && my>=275 && my<=325)
            {
                x1=420, x2=480, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='*';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=520 && mx<=580 && my>=275 && my<=325)
            {
                x1=520, x2=580, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='^';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=620 && mx<=680 && my>=275 && my<=325)
            {
                x1=620, x2=680, y1=275, y2=325;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='a';
                    input[l++]='b';
                    input[l++]='s';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=120 && mx<=180 && my>=350 && my<=400)///line 4
            {
                x1=120, x2=180, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='s';
                    input[l++]='u';
                    input[l++]='m';
                    input[l++]='(';
                }
            }
            else if(mx>=220 && mx<=280 && my>=350 && my<=400)
            {
                x1=220, x2=280, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='s';
                    input[l++]='q';
                    input[l++]='r';
                    input[l++]='t';
                    input[l++]='(';
                    input[l++]='2';
                    input[l++]=',';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=320 && mx<=380 && my>=350 && my<=400)
            {
                x1=320, x2=380, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='s';
                    input[l++]='q';
                    input[l++]='r';
                    input[l++]='t';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=420 && mx<=480 && my>=350 && my<=400)
            {
                x1=420, x2=480, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='l';
                    input[l++]='n';
                    input[l++]='(';

                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=520 && mx<=580 && my>=350 && my<=400)
            {
                x1=520, x2=580, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='l';
                    input[l++]='g';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(mx>=620 && mx<=680 && my>=350 && my<=400)
            {
                x1=620, x2=680, y1=350, y2=400;
                setcolor(15);
                setlinestyle(0,0,7);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
                if(ismouseclick(WM_LBUTTONDOWN))
                {
                    input[l++]='p';
                    input[l++]='r';
                    input[l++]='o';
                    input[l++]='d';
                    input[l++]='(';
                }
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
            }
            else if(my>=465 && my<=490)
            {
                for(int j=0;j<16;j++)
                {
                    if(mx>=xx1[j] && mx<=xx1[j]+30 && my>=465 && my<=490)
                    {
                        x1=xx1[j], x2=xx1[j]+30, y1=465, y2=490;
                        setcolor(15);
                        setlinestyle(0,0,7);
                        rectangle(x1,y1,x2,y2);
                        setlinestyle(0,0,0);
                        if(ismouseclick(WM_LBUTTONDOWN))
                        {
                            culoarea_aleasa=true;
                            CUL=j;
                        }
                        clearmouseclick(WM_LBUTTONDOWN);
                        clearmouseclick(WM_LBUTTONUP);
                        delay(5);
                    }
                    else
                    {
                        setlinestyle(0,0,7);
                        setcolor(3);
                        rectangle(xx1[j],465,xx1[j]+30,490);
                        setlinestyle(0,0,0);
                    }
                }
            }
            else
            {
                clearmouseclick(WM_LBUTTONDOWN);
                clearmouseclick(WM_LBUTTONUP);
                setlinestyle(0,0,7);
                setcolor(3);
                rectangle(x1,y1,x2,y2);
                setlinestyle(0,0,0);
            }
            delay(20);
        }
    }
}

/// butonul care creeaza meniul
void MeniuFormule()
{
    clearviewport();
    clearmouseclick(WM_LBUTTONUP);
    clearmouseclick(WM_LBUTTONDOWN);
    setcolor(3);
    lungimetxt = textwidth("Alegeti una din formulele urmatoare");
    settextstyle(4, 0, 3);
    outtextxy(145, 25, "Alegeti una din formule urmatoare");
    ButoaneMeniu();

}

void updateok()
{
    if(light==true)
    {
        culoare=4;
        settextstyle(10,0,6);
        setbkcolor(culoare);
        setfillstyle(SOLID_FILL,culoare);
        bar(925, 645, 1075, 710);
        outtextxy(967,652,"OK");
        culoare=16;
        setbkcolor(culoare);
        setlinestyle(0,0,4);
        rectangle(925, 645, 1075, 710);
        setlinestyle(0,0,0);
    }
    else
    {
        culoare=4;
        settextstyle(10,0,6);
        setbkcolor(5);
        setfillstyle(SOLID_FILL,5);
        bar(925, 645, 1075, 710);
        outtextxy(967,652,"OK");
        culoare=16;
        setbkcolor(culoare);
        setlinestyle(0,0,4);
        rectangle(925, 645, 1075, 710);
        setlinestyle(0,0,0);
    }

}

void calculatorwindow()
{
    //goline window
    if(ok)
        ok=ok;
    clearviewport();
    clearmouseclick(WM_LBUTTONUP);
    clearmouseclick(WM_LBUTTONDOWN);
    //setting up new window
    if(light==true)
    {
        setbkcolor(COLOR(225,211,208));
        cleardevice();
        setcolor(4);
    }
    else
    {
        setbkcolor(COLOR(0,0,0));
        cleardevice();
        setcolor(3);
    }
    rectangle(150,365,1050,615);
    if(light==true)
        setcolor(0);
    else
        setcolor(3);

    settextstyle(8,0,4);
    outtextxy(100,150,"Introduceti formula:");
    rectangle(100,210,1100,340);
    setlinestyle(0,0,0);
    updatebackb();
    updateformuleb();
    updateok();
    // in cazul in care ne-am intors din meniu formule
    if(ok)
    {
        if(light==true)
            setbkcolor(COLOR(225,211,208));
        else
            setbkcolor(COLOR(0,0,0));
        settextstyle(8,0,3);
        if(culoarea_aleasa==true)
        {
            setcolor(CUL);
            outtextxy(110,220+25*lin,input+st[lin]);
            if(light==true)
                setcolor(0);
            else
                setcolor(3);
        }
        else
            outtextxy(110,220+25*lin,input+st[lin]);
    }
    //every frame:
    while(1)
    {
        mx=mousex();
        my=mousey();
        if(b1!=bgc1)
        {
            updatebackb();
            b1=bgc1;
        }
        if(b2!=bgc2)
        {
            updateformuleb();
            b2=bgc2;
        }
        //hover&click pt butonul "inapoi"
        if(mx>=50&&mx<=250&&my>=645&&my<=710)
        {
            while(ismouseclick(WM_LBUTTONDOWN))
            {
                if(light==true)
                    bgc1=15;
                else
                    bgc1=0;
                if(b1!=bgc1)
                {
                    updatebackb();
                    b1=bgc1;
                }
                mx=mousex();
                my=mousey();
                if(!(mx>=50&&mx<=250&&my>=645&&my<=710)) break;
                if(ismouseclick(WM_LBUTTONUP))
                    mainwindow();
                delay(10);
            }
            clearmouseclick(WM_LBUTTONUP);
            clearmouseclick(WM_LBUTTONDOWN);
            if(light==true)
                bgc1=12;
            else
                bgc1=13;
        }
        else  if(ok==true && (mx>=900 && my>=645 && mx<=1100 && my<=710))
        {
            if(ismouseclick(WM_LBUTTONDOWN))
            {
                ok=false;
                setcurrentwindow(window2);
                clearviewport();
                MeniuFormule();
            }
            clearmouseclick(WM_LBUTTONDOWN);
            clearmouseclick(WM_LBUTTONUP);
        }
        else
        {
            if(light==true)
                bgc1=4;
            else
                bgc1=5;
            if(mx>=100 && mx<=300 && my>=90 && my<=140)
            {
                while(ismouseclick(WM_LBUTTONDOWN))
                {
                    if(light==true)
                        bgc2=15;
                    else
                        bgc2=0;
                    if(b2!=bgc2)
                    {
                        updateformuleb();
                        b2=bgc2;
                    }
                    mx=mousex();
                    my=mousey();
                    if(!(mx>=100 && mx<=300 && my>=90 && my<=140)) break;
                    if(ismouseclick(WM_LBUTTONUP))
                    {
                        clearmouseclick(WM_LBUTTONDOWN);
                        window2=initwindow(800, 600, "Formule");
                        setcurrentwindow(window2);
                        MeniuFormule();
                    }
                    delay(10);
                }
                clearmouseclick(WM_LBUTTONUP);
                clearmouseclick(WM_LBUTTONDOWN);
                if(light==true)
                    bgc2=12;
                else
                    bgc2=13;
            }
            else
            {
                if(light==true)
                    bgc2=4;
                else
                    bgc2=5;
                clearmouseclick(WM_LBUTTONUP);
                clearmouseclick(WM_LBUTTONDOWN);
            }
        }
        if(kbhit())
        {
            if(light==true)
                setbkcolor(COLOR(225,211,208));
            else
                setbkcolor(COLOR(0,0,0));
            settextstyle(8,0,3);
            c=getch();
            scrie();
            if(culoarea_aleasa==true)
            {
                setcolor(CUL);
                outtextxy(110,220+25*lin,input+st[lin]);
                if(light==true)
                    setcolor(0);
                else
                    setcolor(3);
            }
            else
                outtextxy(110,220+25*lin,input+st[lin]);
        }
        clearmouseclick(WM_LBUTTONDOWN);
        clearmouseclick(WM_LBUTTONUP);
        delay(20);
    }
}
//checkboxes
int i,x[]={170,520,170,320,470,620,770},y[]={254,254,454,454,454,454,454};
void updatesettings(int i)
{
    if(light==true)
        setfillstyle(SOLID_FILL,COLOR(225,211,208));
    else
        setfillstyle(SOLID_FILL,COLOR(0,0,0));
    if(y[i]==254) // mod culoare
    {
        if(x[i]==170) // light
        {
            bar(523,157,537,171);
            colormode=0;
        }
        else  // dark
        {
            bar(173,157,187,171);
            colormode=1;
        }
        return;
    }
    //precizie
    for(int j=2;j<7;j++)
        bar(x[j]+3,y[j]+3,x[j]+18,y[j]+18);
    precizie=i-2;
    i-=2;
}
void setariwindow()
{
    //golire window
    clearviewport();
    clearmouseclick(WM_LBUTTONUP);
    clearmouseclick(WM_LBUTTONDOWN);
    //setting up new window

    if(light==true)
        setbkcolor(COLOR(225,211,208));
    else
    {
        setbkcolor(COLOR(0,0,0));
    }
    cleardevice();
    if(light==true)
        setcolor(0);
    else
        setcolor(3);
    settextstyle(8,0,5);
    outtextxy(100,150,"Mod culoare:");
    outtextxy(100,350,"Precizie rezultat(nr zecimare):");
    settextstyle(8,0,4);
    outtextxy(200,250,"luminos");
    outtextxy(550,250,"intunecat");
    outtextxy(200,450,"0");
    outtextxy(350,450,"1");
    outtextxy(500,450,"2");
    outtextxy(650,450,"3");
    outtextxy(800,450,"4");
    if(light==true)
    {
        setfillstyle(SOLID_FILL,4);
        bar(50,645,250,710);

        setlinestyle(0,0,4);
        rectangle(50,645,250,710);
        setlinestyle(0,0,0);
    }
    else
    {
        setfillstyle(SOLID_FILL,5);
        bar(50,645,250,710);

        setlinestyle(0,0,4);
        rectangle(50,645,250,710);
        setlinestyle(0,0,0);
    }

    bar(x[colormode]+3,y[colormode]+3,x[colormode]+18,y[colormode]+18);
    bar(x[precizie+2]+3,y[precizie+2]+3,x[precizie+2]+18,y[precizie+2]+18);
    //every frame:
    updatebackb();
    while(1)
    {
        mx=mousex();
        my=mousey();
        if(b1!=bgc1)
        {
            updatebackb();
            b1=bgc1;
        }
        for(i=0;i<7;i++)
            rectangle(x[i],y[i],x[i]+20,y[i]+20);
        //hover&click pt butonul "inapoi"

        if(mx>=50&&mx<=250&&my>=645&&my<=710) //Inapoi
        {
            while(ismouseclick(WM_LBUTTONDOWN))
            {
                if(light)
                    bgc1=15;
                else
                    bgc1=0;
                if(b1!=bgc1)
                {
                    updatebackb();
                    b1=bgc1;
                }
                mx=mousex();
                my=mousey();
                if(!(mx>=50&&mx<=250&&my>=645&&my<=710)) break;
                if(ismouseclick(WM_LBUTTONUP))
                    mainwindow();
                delay(20);
            }
            clearmouseclick(WM_LBUTTONUP);
            clearmouseclick(WM_LBUTTONDOWN);
            if(light)
                bgc1=12;
            else
                bgc1=13;
        }
        else
        {
            if(light==true)
                bgc1=4;
            else
                bgc1=5;
            for(i=0;i<7;i++)
            {
                if(mx>=x[i]&&mx<=x[i]+20&&my>=y[i]&&my<=y[i]+20)
                {

                    if(light==true)
                    {
                        setcolor(15);
                        rectangle(x[i],y[i],x[i]+20,y[i]+20);
                        setcolor(0);
                    }
                    else
                    {
                        setcolor(15);
                        rectangle(x[i],y[i],x[i]+20,y[i]+20);
                        setcolor(3);
                    }
                    while(ismouseclick(WM_LBUTTONDOWN))
                    {
                        mx=mousex();
                        my=mousey();
                        if(!(mx>=x[i]&&mx<=x[i]+20&&my>=y[i]&&my<=y[i]+20)) break;
                        if(ismouseclick(WM_LBUTTONUP))
                        {
                            updatesettings(i);
                            if(i==0)
                            {
                                light=true;
                                dark=false;
                                if(light==true)
                                    setfillstyle(SOLID_FILL,4);
                                else
                                    setfillstyle(SOLID_FILL,5);
                                bar(x[i]+3,y[i]+3,x[i]+18,y[i]+18);
                                setariwindow();
                            }
                            else if(i==1)
                            {
                                light=false;
                                dark=true;
                                if(light==true)
                                    setfillstyle(SOLID_FILL,4);
                                else
                                    setfillstyle(SOLID_FILL,5);
                                bar(x[i]+3,y[i]+3,x[i]+18,y[i]+18);
                                setariwindow();
                            }
                            else
                            {
                                if(light==true)
                                    setfillstyle(SOLID_FILL,4);
                                else
                                    setfillstyle(SOLID_FILL,5);
                                bar(x[i]+3,y[i]+3,x[i]+18,y[i]+18);
                            }
                            if(i==6)
                            {
                                precizie4=true;
                                precizie3=false;
                                precizie2=false;
                                precizie1=false;
                                precizie0=false;
                            }
                            else if(i==5)
                            {
                                precizie4=false;
                                precizie3=true;
                                precizie2=false;
                                precizie1=false;
                                precizie0=false;
                            }
                            else if(i==4)
                            {
                                precizie4=false;
                                precizie3=false;
                                precizie2=true;
                                precizie1=false;
                                precizie0=false;
                            }
                            else if(i==3)
                            {
                                precizie4=false;
                                precizie3=false;
                                precizie2=false;
                                precizie1=true;
                                precizie0=false;
                            }
                            else if(i==2)
                            {
                                precizie4=false;
                                precizie3=false;
                                precizie2=false;
                                precizie1=false;
                                precizie0=true;
                            }

                        }
                        delay(20);
                    }

                    clearmouseclick(WM_LBUTTONUP);
                    clearmouseclick(WM_LBUTTONDOWN);
                }
            }
        }
        delay(20);
    }
}
int main()
{
    window1=initwindow(1200,720,"Math");
    setcurrentwindow(window1);
    mainwindow();
    return 0;
}
