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
my_len5(A,B):-length(A,B).
my_reverse6(A,B):-reverse(A,B).
my_msort7(A,B):-msort(A,B).
my_max_list8(A,B):-max_list(A,B).
my_flatten9(A,B):-flatten(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_sumlist11(A,B):-sumlist(A,B).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_list_to_set15(A,B):-list_to_set(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_even17(A):-0 is A mod 2.
my_set18(A):-list_to_set(A,A).
my_odd19(A):-1 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_toupper4,[char,char]).
prim(my_len5,[list(_),int]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_msort7,[list(int),list(int)]).
prim(my_max_list8,[list(int),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_lowercase10,[char]).
prim(my_sumlist11,[list(int),int]).
prim(my_head12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_succ14,[int,int]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_pred16,[int,int]).
prim(my_even17,[int]).
prim(my_set18,[list(_)]).
prim(my_odd19,[int]).
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
p([v,b,k,'H'],[h]).
p([l,'B','P',w,w,'Z',b,b,'W'],[b,p,z,w]).
p([k,y,'C',u,'Q'],[c,q]).
p(['X','D','J',s,e],[x,d,j]).
p(['E',z,'S','V'],[e,s,v]).
q([e,'D',e,'F','M',y,o,v],[z,f,m,d]).
q(['V',s,'J',e,'P','X',v,n,n],[v,j,x,p,h]).
q(['W','Y',v,u],[m,y,w]).
q(['A',o,'A',p,'P'],['K',p,a,a]).
q([a,e,'G','S',v,p],[s,'W',g]).
