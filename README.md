# confidence_dud

This repository contains all the experimental data (folder "data") and scripts (folder "scripts") used for the study "_Confidence in a perceptual decision paradoxically increases by adding implausible alternatives_"

## Usage

In order to run the scripts you'll have to download the experimental data (is in folder "data"). All scripts contain, at the beginning, a line where you will have to specify the path to where you have the experimental data on your computer. 

## How is data organized 

In folder "data" you will find the data for both experiments. Rows are trials, and the columns are organized as follows:

- _n3SquareOrCircle:_ indicates if the 3rd alternative was a square (1), a circle (2) or was not present (0).
- _BiggerCircleOrSquare:_ indicates if the biggest figure was a circle (1) or a square (2)
- _PosBig:_ this variable is the same as the former, 1 means that the biggest figure was a circle and 2 that was a square. Was used mostly for debugging.
- _StimVal:_ indicates the sizes of the second biggest stimulus, in proportions to the biggest figure. Is used to indicate the levels of task difficulty.
- _StimVal3:_ indicates the sizes of the third biggest stimulus, in proportions to the second biggest figure. 
- _Nalternativas:_ indicates the number of alternatives present. 
- _RT_type1:_ indicates the response time (in ms) of the type 1 task (the perceptual decision).
- _Response:_ indicates which response made the subject: a circle (1), a square (2) or the third alternative (3).
- _Correct:_ boolean indicator for correct (True) and incorrect (False) responses. 
- _Confidence:_ reported confidence in a scale from 0 to 100.
- _RT_confidence:_ indicates the response time (in ms) of the type 2 task (confidence report).
- _N_sujeto:_ indicates the subject's number.
- _binary_correct:_ a binary variable for correct (1) and incorrect (0) responses.
- _Trial_number:_ trial number from 1 to 120 in experiment 1 and from to 480 in experiment 2.
- _Angle:_ random angle for stimuli arrangement. 
- _Step_Angle:_ a rotation of +/-120 degrees of stimuli arrangement. Is expressed in radians (+/- 2.0943...) 
- _Area1:_ size of stimulus 1 (the final size will depend on the screen size).
- _Area2:_ size of stimulus 2 (the final size will depend on the screen size).
- _Area3:_ size of stimulus 3 (the final size will depend on the screen size).
- _Mobile:_ binary indicator, a 1 if the subject performed the experiment on a mobile and a 0 if did it on a computer
- _Code:_ an alphanumeric code that identifies the subject.
- _Gender:_ a variable for gender identification of the participant. f=female; m=male; NoBinario=non binary gender identifications.
- _Age:_ subject's age.

_Note: data from experiment 2 contains more columns because it had more alternatives. For example, there are variables called n4SquareOrCircle and n5SquareOrCircle, indicating if the fourth and fifth alternative was a square (1) or a circle (2) respectively (and 0 if it was not shown). The same is for the other variables, with a 3, 4 or 5 indicating to which alternative refers._

## Contact

For any doubts or suggestions you can send an e-mail to the corresponding author at nicocomay@gmail.com 

