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

my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A),A > 0.
my_element8(A,B):-member(B,A).
my_lowercase9(A):-downcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_sumlist4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_tail6,[list(T),list(T)]).
prim(my_pred7,[int,int]).
prim(my_element8,[list(T),T]).
prim(my_lowercase9,[char]).
prim(my_last10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
prim(my_max_list12,[list(int),int]).
prim(my_toupper13,[char,char]).
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
p([d,'J',a,v,a,'S'],[j,s]).
p(['K','P',l,'Y',v,j,d,n,'A'],[k,p,y,a]).
p([t,b,h,'I'],[i]).
p(['H',s,'Z',z,'I','Y',f],[h,z,i,y]).
p([t,d,'U','D',z,'K',w],[u,d,k]).
q(['X','J','N','V'],[x,n,j,'Z',v]).
q([y,'J',h,m,b,'Q',f],[h,q,j]).
q([m,'P',u,'G','C','M','Q',x,'Q'],[g,q,m,p,q,c,y]).
q(['S','F','O','E','R',v],[f,'S',e,o,r,s]).
q(['N',v,n,p],[n,'W']).
