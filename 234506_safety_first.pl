/* -*- mode: Prolog; comment-column: 48 -*- */



% auxiliary initial action generating rule
act(Action, Knowledge) :-

	% To avoid looping on act/2.
	not(gameStarted),
	assert(gameStarted),

	% Creating initial knowledge
	worldSize(X,Y),				%this is given
	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),		%this we assume by default
	assert(myTrail([])),
	assert(haveGold(0)),
	assert(visit([])),
	assert(stench([])),
	assert(breeze([])),
	act(Action, Knowledge).

% standard action generating rules
% this is our agent's algorithm, the rules will be tried in order
act(Action, Knowledge) :- exit_if_home(Action, Knowledge). %if at home with gold
act(Action, Knowledge) :- go_back_step(Action, Knowledge). %if have gold elsewhere
act(Action, Knowledge) :- pick_up_gold(Action, Knowledge). %if just found gold
act(Action, Knowledge) :- big_nope(Action, Knowledge). 
act(Action, Knowledge) :- nope(Action, Knowledge).
act(Action, Knowledge) :- turn_if_wall(Action, Knowledge). %if against the wall
act(Action, Knowledge) :- else_move_on(Action, Knowledge). %otherwise


exit_if_home(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myPosition(1, 1, Orient),
	Action = exit,				%done game
	Knowledge = [].				%irrelevant but required
	
%wyjdź jeśli jest zablokowany przy wejsciu
exit_if_home(Action, Knowledge) :-
	myPosition(1, 1, Orient),
	visit(OldVis),
	check_if_smth_in_box_beside_in_dir(1, 1, north, OldVis),
	check_if_smth_in_box_beside_in_dir(1, 1, east, OldVis),
	Action = exit,				
	Knowledge = [].				
	
go_back_step(Action, Knowledge) :-
	%%% assuming we have just found gold:
	%%% 1. our last action must have been grab
	%%% 2. our previuos action must have been moveForward
	%%% 3. so we are initiating a turnback and then return:
	%%%    (a) pop grab from the stack
	%%%    (b) replace it by an artificial turnRight we have never
	%%%        executed, but we will be reversing by turning left
	%%%    (c) execute a turnRight now which together will turn us back
	%%% 4. after that we are facing back and can execute actions in reverse
	%%% 5. because of grab we can be sure this rule is executed exactly once
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail(Trail),
	Trail = [ [grab,X,Y,Orient] | Trail_Tail ],
	New_Trail = [ [turnRight,X,Y,Orient] | Trail_Tail ], %Orient is misleading here
	Action = turnLeft,
	Knowledge = [gameStarted,
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(New_Trail)].

go_back_step(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [Action,X,Y,Orient] | Trail_Tail ]),
	Action = moveForward,
	Knowledge = [gameStarted,
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

%%% backtracking a step can be moving or can be turning
go_back_step(Action, Knowledge) :- go_back_turn(Action, Knowledge).

go_back_turn(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [OldAct,X,Y,Orient] | Trail_Tail ]),
	%% if our previous action was a turn, we must reverse it now
	((OldAct=turnLeft,Action=turnRight);(OldAct=turnRight,Action=turnLeft)),
	Knowledge = [gameStarted,
		             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

pick_up_gold(Action, Knowledge) :-
	glitter,
	Action = grab,			    %this is easy, we are sitting on it
	haveGold(NGolds),		    %we must know how many golds we have
	NewNGolds is NGolds + 1,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ], %important to remember grab
	Knowledge = [gameStarted,
		            haveGold(NewNGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),	%the position stays the same
			 visit(OldVis),
			 stench(OldSten),
			 breeze(OldBree),
		     myTrail(New_Trail)].

			 
big_nope(Action, Knowledge) :-
	(breeze ; stench),
	myPosition(1, 1, Orient),
	Action = exit,				%done game
	Knowledge = [].				

nope(Action, Knowledge) :-
	%%% Nope, thank you.... jak wyczuje zagrozenie ma zrobic tyl zwrot i sie wycofac
	not(getBack(A)),
	(breeze ; stench),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	locate_bree(X, Y,OldBree, NewBree),
	locate_sten(X, Y,OldSten, NewSten),
	myWorldSize(Max_X,Max_Y),
	haveGold(NGolds),
	myTrail(Trail),
	shiftOrient(Orient, NewOrient),
	Action = turnLeft,
	Knowledge = [gameStarted,
					getBack(turnLeft),
				haveGold(NGolds),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, NewOrient),
				visit(OldVis),
				stench(NewSten),
				breeze(NewBree),
				myTrail(Trail)
				].
				
nope(Action, Knowledge) :-
	getBack(turnLeft),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	haveGold(NGolds),
	myTrail(Trail),
	shiftOrient(Orient, NewOrient),
	Action = turnLeft,
	Knowledge = [gameStarted,
					getBack(moveForward),
				haveGold(NGolds),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, NewOrient),
				visit(OldVis),
				stench(OldSten),
				breeze(OldBree),
				myTrail(Trail)
				].
				

	
%inny system wycofania sie, gdy wiemy ze po prawej jest sciana

nope(Action, Knowledge) :-
	getBack(moveForward),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	haveGold(NGolds),
	shiftOrientalter(Orient, Right),
	againstWall(X, Y, Right, Max_X, Max_Y), %sprawdza czy po prawej jest sciana
	forwardStep(X, Y, Orient, New_X, New_Y),
	myTrail([ [moveForward,X1,Y1,Orient1] | Trail_Tail ]), %sprawdza czy ostatnim ruchem byl ruch do przodu. Jesli tak, to musi obrocic sie 0 180 stopni
	shiftOrientalter(Orient1, NewOrient1),
	shiftOrientalter(NewOrient1, NewOrient2),
	NewTrail = [[turnLeft,X1,Y1,NewOrient1],[turnLeft,X1,Y1,NewOrient2] | Trail_Tail ],
	add_to_list_if_there_is_not_on_list([X, Y], OldVis, NewVisited),
	Action = moveForward,
	Knowledge = [gameStarted,
				haveGold(NGolds),
				myWorldSize(Max_X, Max_Y),
				myPosition(New_X, New_Y, Orient),
				visit(NewVisited),
				stench(OldSten),
				breeze(OldBree),
				myTrail(NewTrail)
				].
				
				

nope(Action, Knowledge) :-
	getBack(moveForward),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	haveGold(NGolds),
	shiftOrientalter(Orient, Right),
	againstWall(X, Y, Right, Max_X, Max_Y), 
	forwardStep(X, Y, Orient, New_X, New_Y),
	myTrail([ [Action1,X1,Y1,Orient1] , [Action2,X2,Y2,Orient2] | Trail_Tail ]),
	shiftOrient(Orient2, NewOrient1),
	shiftOrient(Orient2, NewOrient2),
	NewTrail = [ [turnLeft,X2,Y2,NewOrient2] | Trail_Tail ],
	add_to_list_if_there_is_not_on_list([X, Y], OldVis, NewVisited),
	Action = moveForward,
	Knowledge = [gameStarted,
				haveGold(NGolds),
				myWorldSize(Max_X, Max_Y),
				myPosition(New_X, New_Y, Orient),
				visit(NewVisited),
				stench(OldSten),
				breeze(OldBree),
				myTrail(NewTrail)
				].


nope(Action, Knowledge) :-
	getBack(moveForward),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	haveGold(NGolds),
	forwardStep(X, Y, Orient, New_X, New_Y),
	myTrail(Trail),
	add_to_list_if_there_is_not_on_list([X, Y], OldVis, NewVisited),
	Action = moveForward,
	Knowledge = [gameStarted,
				haveGold(NGolds),
				getBack(turnRight),
				myWorldSize(Max_X, Max_Y),
				myPosition(New_X, New_Y, Orient),
				visit(NewVisited),
				stench(OldSten),
				breeze(OldBree),
				myTrail(Trail)
				].
				
nope(Action, Knowledge) :-
	%wycofaj sie jak wpadles do bryzy lub smrodu
	getBack(turnRight),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	shiftOrientalter(Orient, NewOrient),
	not(againstWall(X, Y, NewOrient, Max_X, Max_Y)), %odwolanie czynnosci ze wzgledu na sciane
	haveGold(NGolds),
	myTrail([ [Action1,X1,Y1,Orient1] | Trail_Tail ]),
	NewTrail = [ [turnLeft,X1,Y1,NewOrient] | Trail_Tail ],
	Action = turnRight,
	Knowledge = [gameStarted,
				haveGold(NGolds),
				alreadyRotated(yes),
				myWorldSize(Max_X, Max_Y),
				myPosition(X, Y, NewOrient),
				visit(OldVis),
				stench(OldSten),
				breeze(OldBree),
				myTrail(NewTrail)
				].

%obroc sie w prawo jesli tam nie byles i pole jest wolne
else_move_on(Action, Knowledge) :-
	not(alreadyRotated(A)),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	Action = turnRight,			%always successful
	shiftOrientalter(Orient, NewOrient),		%always successful
	not(againstWall(X, Y, NewOrient, Max_X, Max_Y)), %not against wall to the right
	(not(check_if_smth_in_box_beside_in_dir(X,Y,NewOrient, OldVis)) ; 
		(check_if_smth_in_box_beside_in_dir(X,Y,NewOrient, OldVis), not(check_if_smth_in_box_beside_in_dir(X,Y,NewOrient, OldBree)), not(check_if_smth_in_box_beside_in_dir(X,Y,NewOrient, OldSten)) ) ),
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
			 alreadyRotated(yes),
	         myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
			 visit(OldVis),
			 stench(OldSten),
			breeze(OldBree),
		     myTrail(New_Trail)].			 
%obroc sie w prawo przy scianie
turn_if_wall(Action, Knowledge) :-
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	againstWall(X, Y, Orient, Max_X, Max_Y),
	Action = turnRight,			
	shiftOrientalter(Orient, NewOrient),		
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
			 alreadyRotated(yes),
	         myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
			 visit(OldVis),
			 stench(OldSten),
			breeze(OldBree),
		     myTrail(New_Trail)].
					

%obroc sie w lewo w prawym rogu
turn_if_wall(Action, Knowledge) :-
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	againstWall(X, Y, Orient, Max_X, Max_Y),
	shiftOrientalter(Orient, Right),
	againstWall(X, Y, Right, Max_X, Max_Y),
	Action = turnLeft,			
	shiftOrient(Orient, NewOrient),		
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
	         myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
			 visit(oldVis),
			 stench(OldSten),
			breeze(OldBree),
		     myTrail(New_Trail)].		

% obroc sie w lewo, jesli stoisz przed sciana i prawy jest odwiedzony
turn_if_wall(Action, Knowledge) :-
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	myWorldSize(Max_X,Max_Y),
	againstWall(X, Y, Orient, Max_X, Max_Y),
	shiftOrientalter(Orient, Right),
	check_if_smth_in_box_beside_in_dir(X,Y,Right, OldVis),
	shiftOrient(Orient, Left),
	not(againstWall(X, Y, Left, Max_X, Max_Y)),
	not(check_if_smth_in_box_beside_in_dir(X,Y,Left, OldVis)),
	Action = turnLeft,			%always successful
	shiftOrient(Orient, NewOrient),		
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
	         myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
			 visit(OldVis),
			 stench(OldSten),
			breeze(OldBree),
		     myTrail(New_Trail)].					 





			 
else_move_on(Action, Knowledge) :-
	Action = moveForward,			
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	visit(OldVis),
	stench(OldSten),
	breeze(OldBree),
	add_to_list_if_there_is_not_on_list([X,Y], OldVis, NewVisited),
	forwardStep(X, Y, Orient, New_X, New_Y),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
	             myWorldSize(Max_X, Max_Y),
		     myPosition(New_X, New_Y, Orient),
			 visit(NewVisited),
			 stench(OldSten),
			breeze(OldBree),
		     myTrail(New_Trail)].



			 
checking_to_right_not_necessary(X, Y, Orient, Max_X, Max_Y, Visited) :-
	shiftOrientalter(Orient, NewOrient),
	forwardStep(X, Y, NewOrient,  New_X, New_Y),
	(againstWall(X, Y, NewOrient, Max_X, Max_Y) ; member([New_X, New_Y], Visited)).

smth_on_box_beside_or_wall(X, Y, Orient, Max_X, Max_Y, SmthList):-
	shiftOrient(Orient, NewOrient),
	checking_to_right_not_necessary(X, Y, NewOrient, Max_X, Max_Y, SmthList).


locate_bree(X,Y, OldBree, NewBree) :-
	breeze,
	add_to_list_if_there_is_not_on_list([X, Y], OldBree, NewBree).
locate_bree(X,Y, OldBree, NewBree) :-
	not(breeze),
	NewBree = OldBree.
	

locate_sten(X,Y, OldSten, NewSten) :-
	stench,
	add_to_list_if_there_is_not_on_list([X, Y], OldSten, NewSten).
locate_sten(X,Y, OldSten, NewSten) :-
	not(stench),
	NewSten = OldSten.
	
	
check_if_smth_in_box_beside_in_dir(CurrX, CurrY, Orient, Visited) :-
	forwardStep(CurrX, CurrY, Orient,  New_X, New_Y),
	member([New_X, New_Y], Visited).
	
add_to_list_if_there_is_not_on_list(Elem, List, NewList) :-
	not(member( Elem, List)),
	NewList = [Elem | List].
add_to_list_if_there_is_not_on_list(Elem, List, NewList) :-
	member( Elem, List),
	NewList = List.

	
			 
forwardStep(X, Y, east,  New_X, Y) :- New_X is (X+1).
forwardStep(X, Y, south, X, New_Y) :- New_Y is (Y-1).
forwardStep(X, Y, west,  New_X, Y) :- New_X is (X-1).
forwardStep(X, Y, north, X, New_Y) :- New_Y is (Y+1).

shiftOrient(east, north).
shiftOrient(north, west).
shiftOrient(west, south).
shiftOrient(south, east).

shiftOrientalter(north, east).
shiftOrientalter(east, south).
shiftOrientalter(south, west).
shiftOrientalter(west, north).


againstWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = Y1,     Orient = east.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = X1, Y = Max_Y, Orient = north.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = Y1, Orient = west.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = X1,     Y = 1,     Orient = south.


			 