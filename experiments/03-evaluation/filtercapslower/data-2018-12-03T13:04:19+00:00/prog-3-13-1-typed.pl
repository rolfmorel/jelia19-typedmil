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

my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_msort11(A,B):-msort(A,B).
my_even12(A):-0 is A mod 2.
my_last13(A,B):-last(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_head15([H|_],H).
my_set16(A):-list_to_set(A,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_min_list4,[list(int),int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_pred6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_max_list8,[list(int),int]).
prim(my_len9,[list(_),int]).
prim(my_lowercase10,[char]).
prim(my_msort11,[list(int),list(int)]).
prim(my_even12,[int]).
prim(my_last13,[list(T),T]).
prim(my_toupper14,[char,char]).
prim(my_head15,[list(T),T]).
prim(my_set16,[list(_)]).
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
p(['I',v,'X','F','Y',u],[i,x,f,y]).
p([l,z,w,z,r,'H','W','M'],[h,w,m]).
p(['N','Q','F',w,'K','B','T',l],[n,q,f,k,b,t]).
p(['H','E','P','U'],[h,e,p,u]).
p(['Q','M',v,'N','J'],[q,m,n,j]).
q([y,'S','I','J','N','M','N','B'],[j,h,b,n,n,m,i,s]).
q([g,'L',w,'C','L',a,'I',r],[l,s,l,i,c]).
q([s,'O',x,'L',d,s,y],[o,'N',l]).
q(['J','U',y,c,'D','P',y],[j,u,d,'R',p]).
q(['Q','W',f,e,o],[w,q,o]).
