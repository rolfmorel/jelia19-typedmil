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

my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_tail6([_|TL],TL).
my_list_to_set7(A,B):-list_to_set(A,B).
my_odd8(A):-1 is A mod 2.
my_last9(A,B):-last(A,B).
my_len10(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_max_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_tail6,[list(T),list(T)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_odd8,[int]).
prim(my_last9,[list(T),T]).
prim(my_len10,[list(_),int]).
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
p(['M',e,'B',m,'W',k],[m,b,w]).
p([a,e,d,e,d,'V'],[v]).
p([e,'Q','Y',p,u,'R'],[q,y,r]).
p([y,'S','I','B','U','F',c,'I'],[s,i,b,u,f,i]).
p([w,'Q','H',m,s],[q,h]).
q([l,e,'S','D',j,'W',u],[d,w,y,s]).
q([a,j,h,r,'I','N',o,'W'],[i,n,'R',w]).
q([d,'L','T','I','S'],[t,i,s,l,'X']).
q(['D','S','P','Q',d,'X',r,'L'],[p,x,q,l,x,d,s]).
q(['S','Q','K',p,'H','Y',k,y],[y,h,k,s,z,q]).