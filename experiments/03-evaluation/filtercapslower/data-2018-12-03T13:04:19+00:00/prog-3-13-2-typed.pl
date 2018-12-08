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

my_toupper4(A,B):-upcase_atom(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_last7(A,B):-last(A,B).
my_element8(A,B):-member(B,A).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_even12(A):-0 is A mod 2.
my_pred13(A,B):-succ(B,A),A > 0.
my_flatten14(A,B):-flatten(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_toupper4,[char,char]).
prim(my_succ5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_last7,[list(T),T]).
prim(my_element8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_even12,[int]).
prim(my_pred13,[int,int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_head15,[list(T),T]).
prim(my_len16,[list(_),int]).
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
p(['M','P',s,'A',m,'O'],[m,p,a,o]).
p(['V',x,'Z','R'],[v,z,r]).
p(['B',d,'A','D',n,s,'R','X',v],[b,a,d,r,x]).
p([k,'I',x,'J',r,t,x,i],[i,j]).
p(['F',f,'O',m,'H',v,'X',u,'K'],[f,o,h,x,k]).
q([s,'F',a,d,'S',z],[s,'X',f]).
q([h,e,h,g,'S'],[s,z]).
q(['O',s,e,l,'T','N'],[t,g,n,o]).
q(['U','O',p,'W',j,'V'],[o,v,u,w,b]).
q(['X',h,'E',o,m],['B',e,x]).
