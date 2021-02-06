
%% Clear the workspace, close figures and reset the random numbers

%in case the gripper port didn't close last time
if exist('s')
    fclose(s)
end

close all
clearvars
rng('shuffle')

home
%% Set the directories
 
%piloting: 0 = keyboard space, 1 = hand grip
for_real = 0;
fullscreen = 1;
low_pc = 40;
mid_pc = 50;
hi_pc = 60;
line_threshold_mean = mid_pc; %percent of max (where the line is)
line_threshold_sd = 0;

%define directories
taskDir = 'C:\StimUsers\Arran\TaskScripts\Exercise_Pharma';
%taskDir = '/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 3/Design/MATLAB';
resRootDir = fullfile(taskDir,'Results');
stimDir = fullfile(taskDir,'Faces');


%collect participant details
subDetails.Name = input('Subject Name (e.g., s01): ','s');
if isempty(subDetails.Name)
    rndnum = num2str(ceil(rand .* 1000));
    subDetails.Name = ['test_' rndnum];
    subDetails.PhotoS = 'sub_99_S.jpg';
    subDetails.PhotoC = 'sub_99_C.jpg';
    subDetails.Session = '1';
    subDetails.Drug = 'off'; %drug / normal
    subDetails.GripMax = input('Max Grip? ','s');
    subDetails.GripMin = input('Min Grip? ','s');
    n_trials = 3;
else
    subDetails.PhotoS = [subDetails.Name '_S.jpg'];
    subDetails.PhotoC = [subDetails.Name '_C.jpg'];
    subDetails.Session = input('What session num? ','s');
    subDetails.Drug = input('On of off Drug? ','s'); %drug / normal
    subDetails.GripMax = input('Max Grip? ','s');
    subDetails.GripMin = input('Min Grip? ','s');
end

%put some of the participant details into useable variables
GripMax = str2num(subDetails.GripMax);
GripMin = str2num(subDetails.GripMin);

volt_low = GripMin - (low_pc./100).*(GripMax-GripMin);
volt_mid = GripMin - (mid_pc./100).*(GripMax-GripMin);
volt_high = GripMin - (hi_pc./100).*(GripMax-GripMin);

%create results directory to save data if it doesn't already exist
resDir = fullfile(resRootDir,[subDetails.Name '_sess' subDetails.Session]);
if ~exist(resDir)
    mkdir(resDir)
end

%% Task Parameters

%30; % number of trials
resp_rate = 10;
rs = 1; %randomise start position = 1 (0 = do not randomise)

%set trial order for which face is shown (stranger or support figure)
Trial_order = [1 1 1 2 2 2 1 1 1 1 1 1 2 2 2 1 1 1 2 2 2 2 2 2 1 1 1 2 2 2 1 1 1 2 2 2 1 1 1 2 2 2];
shuff = Shuffle([1 2]);
shuff = shuff(1);

%set trial order for difficulty 
Diff_order(1,:) = [2 3 1 2 3 1 3 1 2 1 2 3 2 1 3 3 1 2 2 1 3];
Diff_order(2,:) = [3 1 2 1 3 2 2 1 3 1 3 2 1 2 3 2 3 1 3 1 2];

%shuffle the orders for each exercise block
switch shuff
    case 1
        Trial_order = Trial_order;
    case 2
        Trial_order = abs(Trial_order - 3);
        Diff_order([1 2],:) = Diff_order([2 1],:);
end

%the post-trial questions
Q{1} = 'How much effort did you put in to keep the ball above the line?';

QR{1,1} = 'no effort at all';
QR{1,2} = 'maximum effort';

Q{2} = 'How hard was it to keep the ball above the line?';
QR{2,1} = 'not hard at all';
QR{2,2} = 'extremely hard';

nQuestions = length(Q);

%% Timings (in seconds)

instruc_delay = 1; %delay (if any) between instructions and start
ITI = 2; %inter trial interval
Bdur = 8; %block duration
ISI1 = 2; %delay between stimulus and first question
ISI2 = 2; %delay between questions
confirm_pause = 0.5;
BlockPause = 18;


%% Initialize screen

%prevents MATLAB from reprinting the source code when the program runs
echo off
KbName('UnifyKeyNames');

% Running on PTB-3?
AssertOpenGL;

%returns an array of screen numbers, and chooses the highest: relevant for dual displays
scr_nbr=max(Screen('Screens')); 
if ~fullscreen
    [w, scr_rect]=Screen('OpenWindow', scr_nbr,[], [10 20 1000 600]); %change it to make a smaller screen: Screen('OpenWindow', scr_nbr, [], [10 20 1200 700]);
else
    [w, scr_rect]=Screen('OpenWindow', scr_nbr,[]);
end

%define colours that may be used in the experiment
black = BlackIndex(w);
white = WhiteIndex(w);
gray = (black + white) / 2;
background_colour = gray;
red = [220 0 0];
green = [10 240 5];
yellow = [220 220 0];

%get the coordinates of the centre of the screen and height/width.
xc=scr_rect(3)/2;
yc=scr_rect(4)/2;
x_width = scr_rect(3);
y_height = scr_rect(4);

%% Load stim images

%load the photos of the stranger and support figure
face_pic = imread(fullfile(stimDir, subDetails.PhotoS));
ctrl_pic = imread(fullfile(stimDir, subDetails.PhotoC));
pic = ctrl_pic;

%get the size of the photos
[X Y Z] = size(pic);
stim_size = [0 0 X Y];

%get the xy coordiantes of the top left (TL) and bottom right (BR) of the
%photo
TL_x = xc - .5 .* X; %top left corner position for picture, x coordinate
TL_y = yc - 0.5 .* Y; %top left corner position for picture, y coordinate

BR_x = xc + 0.5 .* X; %(face behind bar)
BR_y = yc + 0.5 .* Y;

%Define the box that the picture will be squeezed into (ideally this should
%be the size of the actual image; 300 x 300px'
pic_box = [TL_x TL_y BR_x BR_y];

%% Lines and ball for grip feedback

line_threshold = (randn .* 7.5) + line_threshold_mean;

%the line requires the xy coordinates at the two ends of the line
y_line_pos = .5 - line_threshold./100;

Lin_L_x = xc - 0.25 .* X;
Lin_L_y = yc + y_line_pos .* Y; 
Lin_R_x = xc + 0.25 .* X;
Lin_R_y = yc + y_line_pos .* Y;

%get the xy coordinates of the centre of the bar (previously a ball)
bar_pos_x = xc;
bar_pos_y = yc + Y ./ 2;

%Define the radius of the ball (this is used to define the 'corners' of the
%circle which we use when drawing the ball.
re=10;

%convert voltages into pixels 
Grip_range = GripMin - GripMax;
bar_unit(1) = (Y .* (mid_pc./low_pc)) ./ Grip_range; %the unit to multiply the voltages by in order to move bar
bar_unit(2) = Y ./ Grip_range;
bar_unit(3) = (Y .* (mid_pc./hi_pc)) ./ Grip_range;

%xy coordinates for the response line
resp_line_y = y_height .* 0.75;
resp_line_x(1) = xc - 250;
resp_line_x(2) = xc + 250;


length_bar = resp_line_x(2) - resp_line_x(1);

%offsets so that text line up with the response line
txt_y_off = 20;
txt_x_off = [205 20];

%size of the line position marker
fixpt_ext=100;

%% Activate the hand gripper

%The usb attached to the gripper has an 'address'; open this address so
%that the USB input can be read
if for_real == 1;
    s = serial('COM5'); %'/dev/cu.usbmodem781451'
    fopen(s);
end

%% Task Start Screen

%draw the instruction screen and text, and present it
Screen('FillRect', w, gray);
Screen('TextSize', w, 24);
Screen('TextFont', w, 'Arial');
DrawFormattedText(w, 'Get Ready...', 'center','center')
Screen('Flip', w);

%experiment won't start until the space bar is pressed
Down = 0;
while ~Down
    %'listen' for a press of the keyboard.
    [Down, press_time, keyCode] = KbCheck;
    %covert the keyCode into a human-readable name
    keyName = KbName(keyCode);
    display(keyName)
    %if it was the space key pressed, continue, otherwise not
    if strcmp(keyName,'space')
        Down = 1;
    else
        Down = 0;
    end
    
end

%delay after the space key is pressed
Screen('FillRect', w, black);
Screen('Flip', w);
WaitSecs(instruc_delay)

start_time = GetSecs;

Diff_sup_count = 0;
Diff_ctl_count = 0;

%% Trial Loop

%trial order (for piloting or not)
if ~exist('n_trials')
    n_trials = length(Trial_order);
else
    Trial_order = [1 2 1];
end

%iterate through trials
for t = 1:n_trials
    
    trl_type = Trial_order(t);
    V_rec = NaN;
    Vt_rec = NaN;
    Vt_rec2 = NaN;
   
    %introduce photo and trial difficulty variation
    switch trl_type
        case 1
            pic = face_pic;
            Diff_sup_count = Diff_sup_count + 1;
            Diff = Diff_order(1,Diff_sup_count);
        case 2
            pic = ctrl_pic;
            Diff_ctl_count = Diff_ctl_count + 1;
            Diff = Diff_order(2,Diff_ctl_count);
    end
    
    %screen elements
    Screen('FillRect', w, black);
    Screen('TextSize', w, 24);
    DrawFormattedText(w, 'Get Ready...', 'center','center',white)
    Screen('Flip', w);
    WaitSecs(1)
    
    %reselect line height
    line_threshold = (randn .* line_threshold_sd) + line_threshold_mean;
    
    %the line requires the xy coordinates at the two ends (left and right)
    y_line_pos = .5 - line_threshold./100;
    Lin_L_y = yc + y_line_pos .* Y; 
    Lin_R_y = yc + y_line_pos .* Y;
    
    %get trial start time
    tr_start = GetSecs;
    
    %start point
    Grip_px = 0;
    
    l = 1;
    display(Diff)
    while GetSecs < (tr_start + Bdur)
        
        %for the experiment: record variables
        if for_real == 1
                %tead the voltage (grip strength)
                flushinput(s);
                fprintf(s,'r'); %queries button state
                r = fscanf(s); %converts output into better format
                output = textscan(r, '%s'); %converts into text
                V = str2num(cell2mat(output{1}(3))); %voltage text to number
                Vt = str2num(cell2mat(output{1}(2))); %the time of the reading
                
                %grip strength and subsequent movement
                Grip_dV = GripMin - V;
                Grip_px = Grip_dV .* bar_unit(Diff);
                
                %record grip strength (in voltage) and time of reading
                Vt_rec(l) = Vt;
                Vt_rec2(l) = GetSecs;
                
        elseif for_real == 0;
        
            %for testing: pretending time holding down space is grip
            %strength
            %=====================================
            Down = 0;
            [Down, press_time, keyCode] = KbCheck;
            keyName = KbName(keyCode);
            if strcmp(keyName,'space')
                Grip_px = Grip_px + 2;
            else
                Grip_px = Grip_px - 1;
            end
            %=====================================
        
        end
        
        if Grip_px < 0
            Grip_px = 0;
        end
        
        %get x and y coordinates of the bar
        bar_y = bar_pos_y - Grip_px;
        bar_x = bar_pos_x;
        
        %draw all the elements
        
        %black Background
        Screen('FillRect', w, black);
        
        %draw the photo onto screen
        imageTexture = Screen('MakeTexture', w, pic);
        Screen('DrawTexture', w, imageTexture, [], pic_box) % cue_size, position
        
        %draw the white line for the bar
        Screen('DrawLine', w, white, Lin_L_x, Lin_L_y, Lin_R_x, Lin_R_y, 3);
        
        %is bar above (green) or below (red) the line
        if bar_y < Lin_L_y
            col = green;
        else
            col = red;
        end
        
        %draw the line
        Screen('DrawLine', w, white, TL_x-20, Lin_L_y, BR_x+20, Lin_R_y, 3);
        Screen('DrawLine', w, col, TL_x-20, bar_y, BR_x+20, bar_y, 3);
        Screen('DrawTexture', w, imageTexture, [], pic_box) % cue_size, position
        
        %show all the elements
        Screen('Flip', w);
        
        %record the bar position (proxy for voltage / grip strength)
        V_rec(l) = bar_y;
        l = l+1;
    end
    
    %record the voltage/bar height across the trial
    record_V{t} = V_rec;
    record_Volt{t} = V;
    record_VoltTime{t} = Vt_rec;
    record_VoltTime2{t} = Vt_rec2;
    
    %Draw a yellow bar at the end of the trial
    Screen('FillRect', w, black);
    imageTexture = Screen('MakeTexture', w, pic);
    Screen('DrawTexture', w, imageTexture, [], pic_box);
    Screen('DrawLine', w, white, Lin_L_x, Lin_L_y, Lin_R_x, Lin_R_y, 3);
    col = yellow;

    Screen('DrawLine', w, white, TL_x-20, Lin_L_y, BR_x+20, Lin_R_y, 3);
    Screen('DrawLine', w, col, TL_x-20, bar_y, BR_x+20, bar_y, 3);
    Screen('DrawTexture', w, imageTexture, [], pic_box) % cue_size, position
    
    Screen('Flip', w);
    
    %pause before first question (random decimal between 0 and 1 multiplied by 2 seconds)
    WaitSecs(rand .* ISI1)
    
    %the order of the post-trial questions (question #2, on "how hard" the trial was, will come first)
    order = [2 1];
    
    first_q = order(1);
     for q = order
         
        %ask the first question
        %define the starting position of the response dot
        curr_x = randomise_start(xc,resp_line_x,rs);
        Q_start_pos(q) = curr_x;
        Q_start_time(q) = GetSecs;
        
        %prepare the response screen
        %background
        Screen('FillRect', w, black);
        %response Line
        Screen('DrawLine', w, white, resp_line_x(1), resp_line_y, resp_line_x(2), resp_line_y, 5);
        %question text and the 0/100 text at the end of the lines
        Screen('TextSize', w, 24);
        DrawFormattedText(w, Q{q}, 'center',(y_height.*0.5),white)
        Screen('DrawText', w, QR{q,1},  (resp_line_x(1) - txt_x_off(1)) , (resp_line_y - txt_y_off), white);
        Screen('DrawText', w, QR{q,2}, (resp_line_x(2) + txt_x_off(2)) , (resp_line_y - txt_y_off), white);
        %response Dot
        Screen('FillOval', w, fixpt_ext, [curr_x-re resp_line_y-re curr_x+re resp_line_y+re]);
        %present Screen
        Screen('Flip', w)
        
        %for when participants start moving the response dot
        pressed = 0;
        while ~pressed;
            
            %get button press
            Down = 0;
            while ~Down
                [Down, keyName, press_time] = WaitForKey(Q_start_time, q);
            end
            
            %update screen with new info - this is identical same as above, but without the response dot
            Screen('FillRect', w, black);
            Screen('DrawLine', w, white, resp_line_x(1), resp_line_y, resp_line_x(2), resp_line_y, 5);
            Screen('TextSize', w, 24);
            DrawFormattedText(w, Q{q}, 'center',(y_height.*0.5),white)
            Screen('DrawText', w, QR{q,1},  (resp_line_x(1) - txt_x_off(1)) , (resp_line_y - txt_y_off), white);
            Screen('DrawText', w, QR{q,2}, (resp_line_x(2) + txt_x_off(2)) , (resp_line_y - txt_y_off), white);
            
            %recognise the keypress and move dot in the appropriate direction or confirm response.
            if strcmp(keyName,'LeftArrow') %move left
                
                curr_x = MoveLeft(curr_x,resp_rate,resp_line_x);
                
                Screen('FillOval', w, fixpt_ext, [curr_x-re resp_line_y-re curr_x+re resp_line_y+re]);
                
            elseif strcmp(keyName,'RightArrow') %move right
                
                curr_x = MoveRight(curr_x,resp_rate,resp_line_x);
                
                Screen('FillOval', w, fixpt_ext, [curr_x-re resp_line_y-re curr_x+re resp_line_y+re]);
                
            elseif strcmp(keyName,'UpArrow') %confirm response
                
                Screen('FillOval', w, green, [curr_x-re resp_line_y-re curr_x+re resp_line_y+re]); %not sure if color is matlab or [200 0 0] RGB
                
                Q_resp_t(q) = press_time;
                Q_resp_r(q) = curr_x;
                pressed = 1;
                
                Q_pc(q) = ((Q_resp_r(q) - resp_line_x(1))./length_bar).*100;
                
            end
            
            %end question and record response dot position if five seconds have elapsed
            if (Q_start_time(q) + 5) < GetSecs
                Screen('FillOval', w, red, [curr_x-re resp_line_y-re curr_x+re resp_line_y+re]); 
                
                Q_resp_t(q) = NaN;
                Q_resp_r(q) = curr_x;
                pressed = 1;
                
                Q_pc(q) = ((Q_resp_r(q) - resp_line_x(1))./length_bar).*100;
            end

            Screen('Flip', w)
            
        end
        
        %brief pause so they see their response dot go red (confirmation)
        WaitSecs(confirm_pause);
        WaitSecs('UntilTime', Q_start_time(q) + 5) %ensures 5 seconds per question
        
        %pause before next question with blank screen
        Screen('FillRect', w, black);
        Screen('Flip', w)
        WaitSecs(rand .* ISI2)
        
    end
    
    WaitSecs(rand .* ITI)
    
    %record all variables in a data frame (add empty colums for ease of reading
    record_dat(t,1) = t; %trial number
    record_dat(t,2) = trl_type; %1 = support figure face, 2= stranger face
    record_dat(t,3) = tr_start; %trial start time
    record_dat(t,4) = NaN;
    record_dat(t,5) = Q_start_time(1); %start time of question 1
    record_dat(t,6) = Q_start_time(2); %start time of question 2 (question 2 was asked first; see line 406)
    record_dat(t,8) = Q_resp_t(1); %response time of question 1
    record_dat(t,9) = Q_resp_t(2); %response time of question 2
    record_dat(t,11) = NaN;
    record_dat(t,12) = Q_start_pos(1); %starting position of indicatior for question 1
    record_dat(t,13) = Q_start_pos(2); %starting position of indicatior for question 2
    record_dat(t,15) = Q_resp_r(1); %position of indicatior for answer to question 1
    record_dat(t,16) = Q_resp_r(2); %position of indicatior for answer to question 2
    record_dat(t,18) = Q_pc(1); %answer to question 1 as a percentage
    record_dat(t,19) = Q_pc(2); %answer to question 2 as a percentage
    record_dat(t,21) = Diff; %trial difficulty
    record_dat(t,22) = first_q; %the first question asked to the participant (this will always be Q2)
    
    %save data (in case it crashes)
    saveFile = fullfile(resDir, 'data_temp.mat');
    save(saveFile);
    
    %pause in between the experimental subblocks
    if mod(t,3)==0
        WaitSecs(BlockPause)
    end
    
end

%% save recorded V

saveFileFinal = fullfile(resDir, 'ExercisePharmaData.mat');
save(saveFileFinal, 'record_dat', 'record_V','subDetails','record_Volt','record_VoltTime','record_VoltTime2')

%% Close the hand gripper

if exist('s')
    fclose(s)
end

%% Close the screen

sca