:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_head4([H|_],H).
my_double5(N,M):-M is 2*N,M =< 10.
my_len6(A,B):-length(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_min_list8(A,B):-min_list(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_set10(A):-list_to_set(A,A).
my_pred11(A,B):-succ(B,A),A > 0.
my_tail12([_|TL],TL).
my_last13(A,B):-last(A,B).
my_even14(A):-0 is A mod 2.
my_msort15(A,B):-msort(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
my_reverse17(A,B):-reverse(A,B).
my_succ18(A,B):-succ(A,B),B =< 10.
my_flatten19(A,B):-flatten(A,B).
my_odd20(A):-1 is A mod 2.
my_element21(A,B):-member(B,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_head4,[list(T),T]).
prim(my_double5,[int,int]).
prim(my_len6,[list(_),int]).
prim(my_toupper7,[char,char]).
prim(my_min_list8,[list(int),int]).
prim(my_lowercase9,[char]).
prim(my_set10,[list(_)]).
prim(my_pred11,[int,int]).
prim(my_tail12,[list(T),list(T)]).
prim(my_last13,[list(T),T]).
prim(my_even14,[int]).
prim(my_msort15,[list(int),list(int)]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_reverse17,[list(T),list(T)]).
prim(my_succ18,[int,int]).
prim(my_flatten19,[list(list(T)),list(T)]).
prim(my_odd20,[int]).
prim(my_element21,[list(T),T]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([b,b,c,'C',p,p],[c]).
p(['R',j,'Y',b],[r,y]).
p(['I','X','T',v,u,r,g,f,n],[i,x,t]).
p([h,'A',y,g,'Z','L'],[a,z,l]).
p(['Q',a,q,'S','H',n,'Z','D','Q'],[q,s,h,z,d,q]).
q(['G','R','Q','U','A'],[r,t,g,a,u,q]).
q(['M','T','C',y,'R',y],[f,m,r,c,t]).
q(['R','S','R','A','Y','E',a,r],[e,r,y,y,s,r,a]).
q(['T',o,c,x,'E','S','R',i],[e,t,s,r,s]).
q(['T',q,'S',r,c,d],[s,t,m]).
